#' Construct inputs for ADMM solver
#'
#' @description
#' Builds the design matrix and loss functions needed by the ADMM solver.
#' This function takes parsed formula specifications and target values to create
#' a sparse design matrix and corresponding loss functions for optimization.
#'
#' The design matrix is constructed as a sparse indicator matrix where:
#' - Each row represents a constraint (level of a factor or interaction)
#' - Each column represents a sample
#' - Matrix entries are 1 where a sample belongs to a level, 0 otherwise
#'
#' For interactions, the function creates rows for each unique combination of levels.
#'
#' @param data Data frame containing sample data. Character columns will be
#'   automatically converted to factors.
#' @param formula_spec A list containing the parsed formula specification from
#'   \code{parse_raking_formula}, with components:
#'   \itemize{
#'     \item \code{formula}: The model formula
#'     \item \code{terms}: List of term specifications, each containing:
#'       \itemize{
#'         \item \code{type}: Term type ("exact" or "l2")
#'         \item \code{variables}: Variable names
#'         \item \code{interaction}: Logical indicating if term is an interaction
#'       }
#'   }
#' @param target_values List containing target values computed from population data,
#'   with components:
#'   \itemize{
#'     \item \code{targets}: Named list of target values for each term
#'   }
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{design_matrix}: A sparse Matrix (class "dgCMatrix") where each row
#'       represents a constraint and each column represents a sample. For factor
#'       variables, each level gets a row. For interactions, each combination of
#'       levels gets a row.
#'     \item \code{losses}: A list of loss functions, one per term. Each loss
#'       contains:
#'       \itemize{
#'         \item \code{fn}: The loss function
#'         \item \code{target}: Target values for this term
#'         \item \code{prox}: The proximal operator for this loss
#'       }
#'   }
#'
#' @details
#' The function performs several key steps:
#' 1. Converts character columns to factors
#' 2. Creates model matrices for each term
#' 3. Converts to sparse format for efficiency
#' 4. Assigns appropriate loss functions based on term type
#'
#' For sparse matrix operations, the function uses the Matrix package's dgCMatrix
#' format. Column sums of the design matrix should equal 1 for each sample,
#' indicating that each sample belongs to exactly one level of each factor.
#'
#' @examples
#' \dontrun{
#' # Basic usage with a single factor
#' data <- data.frame(x = factor(c("a", "b", "a")))
#' formula_spec <- list(
#'   formula = ~ x,
#'   terms = list(list(
#'     type = "exact",
#'     variables = "x",
#'     interaction = NULL
#'   ))
#' )
#' target_values <- list(
#'   targets = list(exact_x = c(a = 0.6, b = 0.4))
#' )
#' result <- construct_admm_inputs(data, formula_spec, target_values)
#'
#' # Usage with interactions
#' data <- data.frame(
#'   x = factor(c("a", "b", "a")),
#'   y = factor(c("1", "2", "1"))
#' )
#' formula_spec <- list(
#'   formula = ~ x:y,
#'   terms = list(list(
#'     type = "exact",
#'     variables = c("x", "y"),
#'     interaction = TRUE
#'   ))
#' )
#' target_values <- list(
#'   targets = list(
#'     "exact_x:y" = c("a:1" = 0.3, "a:2" = 0.3,
#'                     "b:1" = 0.2, "b:2" = 0.2)
#' )
#' result <- construct_admm_inputs(data, formula_spec, target_values)
#' }
#'
#' @param normalize Logical. If TRUE, continuous variables are scaled by their
#'   target value for numerical stability.
#'
#' @keywords internal
construct_admm_inputs <- function(
  data,
  formula_spec,
  target_values,
  normalize = TRUE
) {
  # Map term types to loss functions
  loss_types <- list(
    exact = list(
      fn = equality_loss,
      prox = prox_equality
    ),
    l2 = list(
      fn = least_squares_loss,
      prox = prox_least_squares
    ),
    kl = list(
      fn = kl_loss,
      prox = prox_kl
    ),
    var = list(
      fn = equality_loss,
      prox = prox_equality
    ),
    quantile = list(
      fn = equality_loss,
      prox = prox_equality
    ),
    range = list(
      fn = inequality_loss,
      prox = prox_inequality
    )
  )

  # Create model frame from sample data, converting characters to factors
  # Build a clean formula from variable names (without exact/l2/kl wrappers)
  # to ensure column names match what we expect in term$variables
  all_vars <- unique(unlist(lapply(formula_spec$terms, function(t) {
    t$variables
  })))
  clean_formula <- as.formula(paste("~", paste(all_vars, collapse = " + ")))

  # Check for NAs in raking variables - always error, no silent dropping
  na_mask <- !complete.cases(data[all_vars])
  if (any(na_mask)) {
    n_na <- sum(na_mask)
    na_vars <- all_vars[vapply(
      all_vars,
      function(v) any(is.na(data[[v]])),
      logical(1)
    )]
    stop(
      n_na, " row(s) contain missing values in raking variable(s): ",
      paste(na_vars, collapse = ", "), ". ",
      "Remove rows with NAs before calling regrake().",
      call. = FALSE
    )
  }

  mf <- model.frame(clean_formula, data = data)

  # Convert any character columns to factors
  char_cols <- vapply(mf, is.character, logical(1))
  mf[char_cols] <- lapply(mf[char_cols], factor)

  # Initialize lists to store design matrix blocks and losses
  design_blocks <- vector("list", length(formula_spec$terms))
  losses <- vector("list", length(formula_spec$terms))

  # Track scaling factors for de-normalization of continuous variables
  scale_factors <- list()
  current_row <- 1 # Track which row we're at in the combined design matrix

  # Process each term in the formula
  for (term_idx in seq_along(formula_spec$terms)) {
    term <- formula_spec$terms[[term_idx]]
    term_name <- if (is.null(term$interaction)) {
      term$variables
    } else {
      paste(term$variables, collapse = ":")
    }

    # Get target values for this term
    target_key <- paste0(term$type, "_", term_name)
    if (!target_key %in% names(target_values$targets)) {
      stop("Missing target values for term: ", term_name)
    }
    targets <- target_values$targets[[target_key]]

    # Initialize range bounds (only used for range constraints)
    range_lower <- NULL
    range_upper <- NULL

    # Check for unsupported continuous + interaction
    if (!is.null(term$interaction)) {
      for (var in term$variables) {
        if (is.numeric(mf[[var]])) {
          stop(
            "Interactions with continuous variables are not supported.\n",
            "Variable '",
            var,
            "' is numeric but appears in interaction '",
            paste(term$variables, collapse = ":"),
            "'.\n",
            "Use rr_mean() for continuous variables without interactions.",
            call. = FALSE
          )
        }
      }
    }

    # Check if this is a continuous variable (single variable, numeric in data)
    is_continuous <- is.null(term$interaction) &&
      length(term$variables) == 1 &&
      is.numeric(mf[[term$variables]])

    if (is_continuous) {
      # Continuous variable: design matrix row contains values
      raw_values <- mf[[term$variables]]
      original_target <- targets

      if (term$type == "quantile") {
        # Quantile constraint: use indicator I(x <= target_quantile_value)
        # Constraint: sum(wi * I(xi <= q)) = p
        if (length(targets) != 1) {
          stop(
            "Quantile constraint requires exactly one target value for variable: ",
            term$variables,
            call. = FALSE
          )
        }
        quantile_value <- unname(targets[1])
        p <- term$params$p
        values <- as.numeric(raw_values <= quantile_value)
        original_target <- p
        targets <- p
        # No normalization needed for quantile (target is already a probability)
      } else {
        # Compute type-specific values
        if (term$type == "var") {
          x_mean <- mean(raw_values)
          values <- (raw_values - x_mean)^2
        } else if (term$type == "range") {
          values <- raw_values
          if (term$params$mode == "margin") {
            range_lower <- -term$params$margin
            range_upper <- term$params$margin
          } else {
            range_lower <- term$params$lower - targets
            range_upper <- term$params$upper - targets
          }
        } else {
          # Mean constraint (exact, l2)
          values <- raw_values
        }

        # Normalize by target for numerical stability
        if (normalize && abs(targets) > .Machine$double.eps) {
          values <- values / targets
          scale_factors[[length(scale_factors) + 1]] <- list(
            index = current_row,
            scale = targets,
            variable = term$variables
          )
          if (term$type == "range") {
            range_lower <- range_lower / targets
            range_upper <- range_upper / targets
          }
          original_target <- targets
          targets <- 1.0
        }
      }

      design_blocks[[term_idx]] <- Matrix::Matrix(
        matrix(values, nrow = 1),
        sparse = TRUE
      )
      current_row <- current_row + 1
    } else {
      # Categorical variable: create indicator matrix

      # Create model matrix for this term
      # For interactions, combine variables with : in formula
      term_formula <- if (is.null(term$interaction)) {
        as.formula(paste("~", term$variables))
      } else {
        as.formula(paste("~", paste(term$variables, collapse = ":")))
      }

      mf_subset <- mf[, term$variables, drop = FALSE]

      # Create model matrix with all levels (no reference level)
      mm <- model.matrix(
        term_formula,
        mf_subset,
        contrasts.arg = lapply(
          mf_subset[vapply(mf_subset, is.factor, logical(1))],
          contrasts,
          contrasts = FALSE
        )
      )
      # Remove intercept
      mm <- mm[, -1, drop = FALSE]

      # Reorder targets to match model matrix column order
      # Model matrix columns are named like "sexF", "sexM" or "sexF:regionN"
      # Extract level names from column names and match to target names
      mm_col_names <- colnames(mm)
      if (is.null(term$interaction)) {
        # Single variable: strip the variable name prefix
        # "sexF" -> "F", "sexM" -> "M"
        level_names <- sub(paste0("^", term$variables), "", mm_col_names)
      } else {
        # Interaction: column names are like "sexF:regionN"
        # Need to convert to "F:N" format to match pop_data level names
        # Split by ":", strip variable prefix from each part, rejoin
        level_names <- vapply(mm_col_names, function(col_name) {
          parts <- strsplit(col_name, ":")[[1]]
          stripped <- character(length(parts))
          for (i in seq_along(parts)) {
            stripped[i] <- sub(paste0("^", term$variables[i]), "", parts[i])
          }
          paste(stripped, collapse = ":")
        }, character(1), USE.NAMES = FALSE)
      }
      # Validate data levels have corresponding targets
      target_levels <- names(targets)
      missing_targets <- setdiff(level_names, target_levels)
      if (length(missing_targets) > 0) {
        stop(
          "Data contains level(s) for '", term_name, "' that have no targets: ",
          paste(missing_targets, collapse = ", "), ". ",
          "Either add targets for these levels or remove them from the data.",
          call. = FALSE
        )
      }

      # Warn about target levels not in data
      unused_targets <- setdiff(target_levels, level_names)
      if (length(unused_targets) > 0) {
        rlang::warn(
          c(
            paste0(
              "Targets exist for '", term_name, "' levels not present in data: ",
              paste(unused_targets, collapse = ", ")
            ),
            i = "These targets will be ignored."
          ),
          class = "regrake_unused_targets"
        )
      }

      # Reorder targets to match design matrix row order
      targets <- targets[level_names]

      # Convert to sparse matrix with correct dimensions
      # Each row is a constraint (level), each column is a sample
      nonzero <- which(mm != 0, arr.ind = TRUE)
      design_blocks[[term_idx]] <- Matrix::sparseMatrix(
        i = nonzero[, 2], # constraint/level index
        j = nonzero[, 1], # sample index
        x = rep(1, nrow(nonzero)), # all 1s for indicator matrix
        dims = c(ncol(mm), nrow(mm)) # transpose dimensions
      )

      # Update row counter
      current_row <- current_row + ncol(mm)

      # For categorical, original_target is same as targets
      original_target <- targets

      # Handle range constraints for categorical variables
      if (term$type == "range") {
        n_levels <- length(targets)
        if (term$params$mode == "margin") {
          margin <- term$params$margin
          if (length(margin) == 1) {
            # Single margin: same for all levels
            range_lower <- rep(-margin, n_levels)
            range_upper <- rep(margin, n_levels)
          } else if (!is.null(names(margin))) {
            # Named vector: match to level names
            if (!all(level_names %in% names(margin))) {
              missing <- setdiff(level_names, names(margin))
              stop(
                "Named margin vector missing levels: ",
                paste(missing, collapse = ", "),
                call. = FALSE
              )
            }
            range_lower <- -margin[level_names]
            range_upper <- margin[level_names]
          } else {
            stop(
              "Margin must be a single number or a named vector matching level names",
              call. = FALSE
            )
          }
        } else {
          # Explicit bounds mode for categorical
          range_lower <- rep(term$params$lower, n_levels) - targets
          range_upper <- rep(term$params$upper, n_levels) - targets
        }
      }
    }

    # Create loss function based on term type
    if (!term$type %in% names(loss_types)) {
      stop("Unknown term type: ", term$type)
    }
    # Ensure consistent order of loss function components
    # Store both normalized target (for solver) and original target (for reporting)
    # Include evaluate method for BooleanRegularizer objective tracking
    # Use local() to properly capture values for closures
    loss_fn <- loss_types[[term$type]]$fn
    loss_target <- targets

    # Build loss entry
    loss_entry <- list(
      fn = loss_fn,
      target = targets,
      original_target = original_target,
      prox = loss_types[[term$type]]$prox,
      evaluate = local({
        fn <- loss_fn
        tgt <- loss_target
        function(x) sum(fn(x, tgt))
      })
    )

    # Add lower/upper for range constraints
    if (term$type == "range") {
      loss_entry$lower <- range_lower
      loss_entry$upper <- range_upper

      # Update evaluate function for inequality loss
      loss_entry$evaluate <- local({
        fn <- loss_fn
        lo <- range_lower
        hi <- range_upper
        function(x) sum(fn(x, lo, hi))
      })
    }

    losses[[term_idx]] <- loss_entry
  }

  # Combine design matrix blocks
  if (length(design_blocks) == 1) {
    design_matrix <- design_blocks[[1]]
  } else {
    design_matrix <- do.call(rbind, design_blocks)
  }

  # Return results
  list(
    design_matrix = design_matrix,
    losses = losses,
    scale_factors = if (length(scale_factors) > 0) scale_factors else NULL
  )
}
