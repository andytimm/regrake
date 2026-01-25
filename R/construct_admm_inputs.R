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
construct_admm_inputs <- function(data, formula_spec, target_values, normalize = TRUE) {
  # Map term types to loss functions
  loss_types <- list(
    exact = list(
      fn = equality_loss,
      prox = prox_equality
    ),
    l2 = list(
      fn = least_squares_loss,
      prox = prox_least_squares
    )
  )

  # Create model frame from sample data, converting characters to factors
  # Build a clean formula from variable names (without exact/l2/kl wrappers)
  # to ensure column names match what we expect in term$variables
  all_vars <- unique(unlist(lapply(formula_spec$terms, function(t) t$variables)))
  clean_formula <- as.formula(paste("~", paste(all_vars, collapse = " + ")))
  mf <- model.frame(clean_formula, data = data)

  # Convert any character columns to factors
  char_cols <- vapply(mf, is.character, logical(1))
  mf[char_cols] <- lapply(mf[char_cols], factor)

  # Initialize lists to store design matrix blocks and losses
  design_blocks <- vector("list", length(formula_spec$terms))
  losses <- vector("list", length(formula_spec$terms))

  # Track scaling factors for de-normalization of continuous variables
  scale_factors <- list()
  current_row <- 1  # Track which row we're at in the combined design matrix

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

    # Check for unsupported continuous + interaction
    if (!is.null(term$interaction)) {
      for (var in term$variables) {
        if (is.numeric(mf[[var]])) {
          stop(
            "Interactions with continuous variables are not supported.\n",
            "Variable '", var, "' is numeric but appears in interaction '",
            paste(term$variables, collapse = ":"), "'.\n",
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
      # Continuous variable: design matrix row contains actual values
      # F row = [x1, x2, ..., xn] where xi is the value for sample i
      # Constraint: sum(wi * xi) = target (e.g., weighted mean)
      values <- mf[[term$variables]]
      original_target <- targets

      if (normalize && abs(targets) > .Machine$double.eps) {
        # Normalize by target for numerical stability
        # This transforms constraint from sum(wi * xi) = target
        # to sum(wi * xi/target) = 1
        values <- values / targets
        normalized_target <- 1.0

        # Store scale factor for de-normalization
        scale_factors[[length(scale_factors) + 1]] <- list(
          index = current_row,
          scale = targets,
          variable = term$variables
        )

        targets <- normalized_target
      }

      design_blocks[[term_idx]] <- Matrix::Matrix(
        matrix(values, nrow = 1), sparse = TRUE
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

      # Create subset of model frame with only needed variables
      needed_vars <- if (is.null(term$interaction)) term$variables else term$variables
      mf_subset <- mf[, needed_vars, drop = FALSE]

      # Create model matrix with all levels (no reference level)
      mm <- model.matrix(term_formula, mf_subset,
                        contrasts.arg = lapply(mf_subset[sapply(mf_subset, is.factor)],
                                             contrasts, contrasts = FALSE))
      # Remove intercept
      mm <- mm[, -1, drop = FALSE]

      # Convert to sparse matrix with correct dimensions
      # Each row is a constraint (level), each column is a sample
      nonzero <- which(mm != 0, arr.ind = TRUE)
      design_blocks[[term_idx]] <- Matrix::sparseMatrix(
        i = nonzero[, 2],           # constraint/level index
        j = nonzero[, 1],           # sample index
        x = rep(1, nrow(nonzero)),  # all 1s for indicator matrix
        dims = c(ncol(mm), nrow(mm))  # transpose dimensions
      )

      # Update row counter
      current_row <- current_row + ncol(mm)

      # For categorical, original_target is same as targets
      original_target <- targets
    }

    # Create loss function based on term type
    if (!term$type %in% names(loss_types)) {
      stop("Unknown term type: ", term$type)
    }
    # Ensure consistent order of loss function components
    # Store both normalized target (for solver) and original target (for reporting)
    losses[[term_idx]] <- list(
      fn = loss_types[[term$type]]$fn,
      target = targets,
      original_target = original_target,
      prox = loss_types[[term$type]]$prox
    )
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