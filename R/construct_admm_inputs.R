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
#'         \item \code{type}: Term type ("exact", "l2", "kl", "var", "quantile", or "range")
#'         \item \code{variables}: Variable names
#'         \item \code{interaction}: List of variable expressions (NULL for main effects)
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
#'
#' @param normalize Logical. If TRUE, continuous variables are scaled by their
#'   target value for numerical stability.
#'
#' @keywords internal
check_hard_point_feasibility <- function(values, target, term_name, term_type) {
  value_min <- min(values)
  value_max <- max(values)
  if (target < value_min - 1e-12 || target > value_max + 1e-12) {
    stop(
      "Infeasible hard constraint for '", term_name, "' (", term_type, "): ",
      "target ", format(target, digits = 8), " is outside achievable range [",
      format(value_min, digits = 8), ", ", format(value_max, digits = 8),
      "] implied by sample data.",
      call. = FALSE
    )
  }
}

check_hard_interval_feasibility <- function(values, lower, upper, term_name) {
  value_min <- min(values)
  value_max <- max(values)
  if (upper < value_min - 1e-12 || lower > value_max + 1e-12) {
    stop(
      "Infeasible hard range constraint for '", term_name, "': ",
      "allowed interval [", format(lower, digits = 8), ", ",
      format(upper, digits = 8), "] does not overlap achievable range [",
      format(value_min, digits = 8), ", ", format(value_max, digits = 8),
      "] implied by sample data.",
      call. = FALSE
    )
  }
}

resolve_term_target_key <- function(term, term_name, target_values) {
  legacy_target_key <- paste0(term$type, "_", term_name)
  target_key <- if (!is.null(term$term_id)) term$term_id else legacy_target_key

  if (target_key %in% names(target_values$targets)) {
    return(target_key)
  }
  if (legacy_target_key %in% names(target_values$targets)) {
    return(legacy_target_key)
  }

  NULL
}

get_mean_target_for_var <- function(variable, formula_spec, target_values) {
  for (term in formula_spec$terms) {
    if (!is.null(term$interaction) || term$type != "exact" || !identical(term$variables, variable)) {
      next
    }

    key <- resolve_term_target_key(term, variable, target_values)
    if (is.null(key)) {
      next
    }

    term_targets <- target_values$targets[[key]]
    target_names <- names(term_targets)

    if (!is.null(target_names)) {
      mean_idx <- tolower(target_names) == "mean"
      if (any(mean_idx)) {
        return(unname(term_targets[which(mean_idx)[1]]))
      }
    }

    if (length(term_targets) == 1) {
      return(unname(term_targets[1]))
    }
  }

  NULL
}

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
    target_key <- resolve_term_target_key(term, term_name, target_values)
    if (is.null(target_key)) {
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
        target_label <- names(targets)[1]
        if (is.null(target_label) || !nzchar(target_label)) {
          target_label <- if (term$type == "var") "var" else "mean"
        }
        if (term$type != "quantile" && length(targets) != 1) {
          stop(
            "Continuous term '", term_name,
            "' requires exactly one target value, but received ",
            length(targets), ".",
            call. = FALSE
          )
        }

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
          quantile_label <- names(original_target)[1]
          if (is.null(quantile_label) || !nzchar(quantile_label)) {
            quantile_label <- "quantile"
          }
          original_target <- stats::setNames(p, quantile_label)
          targets <- stats::setNames(p, quantile_label)
          check_hard_point_feasibility(values, unname(targets[1]), term_name, term$type)
          # No normalization needed for quantile (target is already a probability)
        } else {
          # Compute type-specific values
          if (term$type == "var") {
            x_mean <- get_mean_target_for_var(term$variables, formula_spec, target_values)
            if (is.null(x_mean)) {
              x_mean <- mean(raw_values)
              rlang::warn(
                c(
                  paste0(
                    "rr_var(", term$variables,
                    ") has no matching rr_mean(", term$variables, ") term."
                  ),
                  i = "Centering on the sample mean. Add rr_mean() to center on population mean."
                ),
                class = "regrake_var_center_sample_mean"
              )
            }
            values <- (raw_values - x_mean)^2
            check_hard_point_feasibility(values, unname(targets[1]), term_name, term$type)
          } else if (term$type == "range") {
            values <- raw_values
            if (term$params$mode == "margin") {
              range_lower <- -term$params$margin
              range_upper <- term$params$margin
            } else {
              range_lower <- term$params$lower - targets
              range_upper <- term$params$upper - targets
            }
            feasible_lower <- unname(targets[1] + range_lower)
            feasible_upper <- unname(targets[1] + range_upper)
            check_hard_interval_feasibility(values, feasible_lower, feasible_upper, term_name)
          } else {
            # Mean constraint (exact, l2)
            values <- raw_values
            if (term$type == "exact") {
              check_hard_point_feasibility(values, unname(targets[1]), term_name, term$type)
            }
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
          targets <- stats::setNames(1.0, target_label)
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
        if (term$type %in% c("exact", "range")) {
          stop(
            "Infeasible hard constraint for '", term_name,
            "': target level(s) not present in data: ",
            paste(unused_targets, collapse = ", "), ". ",
            "Remove these levels from targets or include matching rows in data.",
            call. = FALSE
          )
        } else {
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
