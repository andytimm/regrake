#' Parse a raking formula
#'
#' @description
#' Parses a formula that specifies raking constraints. The formula interface provides
#' a natural R-like syntax for specifying both main effects and interactions, with
#' optional constraint types for each term.
#'
#' @param formula A formula specifying raking constraints (e.g., `~ race + rr_l2(age:educ)`)
#' @return A list containing:
#'   \item{terms}{List of parsed terms, each with type and variables}
#'   \item{variables}{Character vector of all required variables}
#'   \item{interactions}{List of interaction specifications}
#'   \item{formula}{The original formula object}
#'
#' @details
#' The formula interface supports:
#' \itemize{
#'   \item Main effects: \code{~ age + race}
#'   \item N-way interactions: \code{~ age:race}, \code{~ age:race:education}
#'   \item Constraint types: \code{rr_exact()}, \code{rr_l2()}, \code{rr_kl()}
#'   \item Mixed constraints: \code{~ age + rr_l2(age:race)}
#' }
#'
#' For categorical variables:
#' \itemize{
#'   \item All levels are preserved (no reference level encoding)
#'   \item This matches typical raking interfaces where margins are specified for all categories
#'   \item Supports linearly dependent constraints in the underlying solver
#' }
#'
#' When variables appear in both main effects and interactions:
#' \itemize{
#'   \item Main effects use exact constraints by default
#'   \item Interactions can use any constraint type (exact, l2, kl)
#'   \item A warning is issued to clarify the behavior
#'   \item Example: \code{~ age + race + rr_l2(age:race)} will use exact constraints for main effects
#'     and l2 for the interaction
#' }
#'
#' Pure duplicates (same variables with different constraints) are not allowed:
#' \itemize{
#'   \item \code{~ age + rr_l2(age)} will error
#'   \item \code{~ age:race + rr_l2(age:race)} will error
#' }
#'
#' @keywords internal
parse_raking_formula <- function(formula) {
  # Validate formula
  if (!rlang::is_formula(formula)) {
    stop("'formula' must be a formula", call. = FALSE)
  }

  # Extract RHS - all our formulas are one-sided
  rhs <- rlang::f_rhs(formula)

  # Check for empty or intercept-only formula
  if (
    rlang::quo_is_missing(rlang::quo(!!rhs)) ||
      identical(rhs, 1L) ||
      identical(rhs, 1)
  ) {
    stop("Empty formula", call. = FALSE)
  }

  # Parse into terms
  terms <- parse_formula_terms(rhs)

  # Validate for overlapping constraints
  validate_overlapping_constraints(terms)

  # Extract all unique variables needed
  variables <- unique(unlist(lapply(terms, function(t) t$variables)))

  # Extract interaction specifications
  interactions <- lapply(terms, function(t) t$interaction)
  names(interactions) <- vapply(terms, function(t) t$term_id, character(1))

  # Return structured specification
  structure(
    list(
      terms = terms,
      variables = variables,
      interactions = interactions[!vapply(interactions, is.null, logical(1))],
      formula = formula # Include the original formula
    ),
    class = "raking_formula"
  )
}

#' Validate that there are no invalid overlapping constraints
#' @keywords internal
validate_overlapping_constraints <- function(terms) {
  # Moment constraint types that can be combined on the same variable
  # (e.g., you can match mean, variance, and quantiles of the same continuous variable)
  # "exact" here refers to rr_mean() on continuous, "var" to rr_var(), "quantile" to rr_quantile()
  combinable_moment_types <- c("exact", "var", "quantile")

  # Check for pure duplicates first
  for (i in seq_along(terms)) {
    for (j in seq_len(i - 1)) {
      term_i <- terms[[i]]
      term_j <- terms[[j]]

      # If both terms have same variables and one is not an interaction while the other is
      # or both are interactions/non-interactions, then it's a potential duplicate
      if (
        identical(sort(term_i$variables), sort(term_j$variables)) &&
          (is.null(term_i$interaction) == is.null(term_j$interaction))
      ) {
        var_desc <- if (is.null(term_i$interaction)) {
          paste0("Variable '", term_i$variables, "'")
        } else {
          paste0(
            "Interaction '",
            paste(
              vapply(term_i$interaction, deparse, character(1)),
              collapse = ":"
            ),
            "'"
          )
        }

        # Different types on same variable
        if (term_i$type != term_j$type) {
          # Allow combining moment constraints (e.g., rr_mean + rr_var on same variable)
          both_combinable <- term_i$type %in%
            combinable_moment_types &&
            term_j$type %in% combinable_moment_types
          if (!both_combinable) {
            stop(
              var_desc,
              " appears multiple times with different constraints",
              call. = FALSE
            )
          }
        } else {
          stop(
            var_desc,
            " appears multiple times with duplicate rr_",
            term_i$type,
            "() constraints",
            call. = FALSE
          )
        }
      }
    }
  }

  # Now check for partial overlaps (variables appearing in both main effects and interactions)
  main_effect_vars <- list()
  for (term in terms) {
    if (is.null(term$interaction)) {
      main_effect_vars[[term$variables]] <- term$type
    }
  }

  for (term in terms) {
    if (!is.null(term$interaction)) {
      overlap <- intersect(term$variables, names(main_effect_vars))
      if (length(overlap) > 0) {
        warning(
          paste0(
            "Variables in rr_",
            term$type,
            "(",
            paste(
              vapply(term$interaction, deparse, character(1)),
              collapse = ":"
            ),
            ") also appear as main effects. Using exact constraints for main effects and ",
            "rr_",
            term$type,
            " constraint for the interaction term"
          ),
          call. = FALSE
        )
      }
    }
  }
}

#' Parse formula terms recursively
#' @keywords internal
split_plus_terms <- function(expr) {
  if (rlang::is_call(expr, "+")) {
    args <- rlang::call_args(expr)
    return(c(split_plus_terms(args[[1]]), split_plus_terms(args[[2]])))
  }
  list(expr)
}

#' @keywords internal
abort_unsupported_constraint_expr <- function(expr, wrapper_name) {
  expr_text <- paste(deparse(expr), collapse = "")
  if (identical(wrapper_name, "formula")) {
    stop(
      "Formula terms only support bare variable names and ':' interactions. ",
      "Unsupported expression: ", expr_text, ". ",
      "Create transformed columns in `data` and use those column names in the formula.",
      call. = FALSE
    )
  }
  stop(
    wrapper_name, "() only supports bare variable names, ':' interactions, ",
    "or '+' combinations of those. Unsupported expression: ", expr_text, ". ",
    "Create transformed columns in `data` and use those column names in the formula.",
    call. = FALSE
  )
}

#' @keywords internal
parse_formula_terms <- function(expr) {
  if (!rlang::is_call(expr)) {
    # Base case: single variable
    return(list(create_exact_term(expr)))
  }

  # Get function name if it's a call
  fun <- rlang::call_name(expr)
  args <- rlang::call_args(expr)

  if (fun == "+") {
    # Combine terms from both sides of +
    c(parse_formula_terms(args[[1]]), parse_formula_terms(args[[2]]))
  } else if (fun %in% c("rr_l2", "rr_kl", "rr_exact", "rr_mean", "rr_var")) {
    # Handle constraint functions
    # rr_mean maps to exact constraint (for continuous variables)
    # rr_var keeps its own type for variance matching
    internal_type <- if (fun == "rr_mean") "exact" else sub("^rr_", "", fun)
    term_exprs <- split_plus_terms(args[[1]])
    out <- vector("list", length(term_exprs))
    for (i in seq_along(term_exprs)) {
      out[[i]] <- create_constraint_term(internal_type, term_exprs[[i]], wrapper_name = fun)
    }
    out
  } else if (fun == "rr_quantile") {
    # rr_quantile(x, p) matches the p-th quantile of x
    # args[[1]] is the variable, args[[2]] or args$p is the probability
    if (length(args) < 2) {
      stop(
        "rr_quantile requires two arguments: variable and probability p",
        call. = FALSE
      )
    }
    p <- if (!is.null(names(args)) && "p" %in% names(args)) {
      args$p
    } else {
      args[[2]]
    }
    if (!is.numeric(p) || p <= 0 || p >= 1) {
      stop(
        "rr_quantile probability p must be between 0 and 1 (exclusive)",
        call. = FALSE
      )
    }
    term_exprs <- split_plus_terms(args[[1]])
    out <- vector("list", length(term_exprs))
    for (i in seq_along(term_exprs)) {
      out[[i]] <- create_quantile_term(term_exprs[[i]], p)
    }
    out
  } else if (fun %in% c("rr_range", "rr_between")) {
    # rr_range(x, margin) or rr_range(x, lower, upper)
    # Also supports: rr_range(x, margin = 0.02), rr_range(x, lower = 0.4, upper = 0.6)
    if (length(args) < 2) {
      stop(
        "rr_range requires at least 2 arguments: variable and margin, or lower/upper bounds",
        call. = FALSE
      )
    }

    # Helper to safely evaluate an argument (handles c() expressions, etc.)
    eval_arg <- function(arg) {
      if (is.numeric(arg)) {
        arg
      } else if (is.call(arg)) {
        tryCatch(
          eval(arg, envir = baseenv()),
          error = function(e) arg
        )
      } else {
        arg
      }
    }

    # Check for named arguments first
    arg_names <- names(args)
    has_lower <- !is.null(arg_names) && "lower" %in% arg_names
    has_upper <- !is.null(arg_names) && "upper" %in% arg_names
    has_margin <- !is.null(arg_names) && "margin" %in% arg_names
    term_exprs <- split_plus_terms(args[[1]])

    if (has_lower && has_upper) {
      # Named bounds: rr_range(x, lower = 0.4, upper = 0.6)
      lower <- eval_arg(args$lower)
      upper <- eval_arg(args$upper)
      if (!is.numeric(lower) || !is.numeric(upper)) {
        stop("rr_range lower and upper must be numeric", call. = FALSE)
      }
      if (any(lower >= upper)) {
        stop("rr_range lower must be less than upper", call. = FALSE)
      }
      out <- vector("list", length(term_exprs))
      for (i in seq_along(term_exprs)) {
        out[[i]] <- create_range_term(term_exprs[[i]], lower = lower, upper = upper)
      }
      out
    } else if (has_margin) {
      # Named margin: rr_range(x, margin = 0.02)
      margin <- eval_arg(args$margin)
      if (!is.numeric(margin) || any(margin <= 0)) {
        stop("rr_range margin must be a positive number", call. = FALSE)
      }
      out <- vector("list", length(term_exprs))
      for (i in seq_along(term_exprs)) {
        out[[i]] <- create_range_term(term_exprs[[i]], margin = margin)
      }
      out
    } else if (length(args) == 2) {
      # Positional margin: rr_range(x, 0.02) or rr_range(x, c(A=0.02, B=0.03))
      margin <- eval_arg(args[[2]])
      if (!is.numeric(margin) || any(margin <= 0)) {
        stop("rr_range margin must be a positive number (or named vector of positive numbers)", call. = FALSE)
      }
      out <- vector("list", length(term_exprs))
      for (i in seq_along(term_exprs)) {
        out[[i]] <- create_range_term(term_exprs[[i]], margin = margin)
      }
      out
    } else if (length(args) >= 3) {
      # Positional bounds: rr_range(x, 0.4, 0.6)
      lower <- eval_arg(args[[2]])
      upper <- eval_arg(args[[3]])
      if (!is.numeric(lower) || !is.numeric(upper)) {
        stop("rr_range lower and upper must be numeric", call. = FALSE)
      }
      if (any(lower >= upper)) {
        stop("rr_range lower must be less than upper", call. = FALSE)
      }
      out <- vector("list", length(term_exprs))
      for (i in seq_along(term_exprs)) {
        out[[i]] <- create_range_term(term_exprs[[i]], lower = lower, upper = upper)
      }
      out
    } else {
      stop("rr_range requires margin or lower/upper bounds", call. = FALSE)
    }
  } else if (fun == ":") {
    # Handle interactions by recursively collecting all variables
    list(create_interaction_term(collect_interaction_vars(expr)))
  } else {
    stop(
      "Unknown function '", fun, "' in formula. ",
      "Supported constraint functions are rr_exact(), rr_l2(), rr_kl(), ",
      "rr_mean(), rr_var(), rr_quantile(), rr_range(), and rr_between().",
      call. = FALSE
    )
  }
}

#' Recursively collect variables from an interaction expression
#' @keywords internal
collect_interaction_vars <- function(expr, wrapper_name = NULL) {
  if (!rlang::is_call(expr)) {
    if (!rlang::is_symbol(expr)) {
      abort_unsupported_constraint_expr(expr, if (is.null(wrapper_name)) "formula" else wrapper_name)
    }
    # Base case: single variable
    return(list(expr))
  }

  fun <- rlang::call_name(expr)
  args <- rlang::call_args(expr)

  if (fun == ":") {
    # Recursively collect variables from both sides
    c(
      collect_interaction_vars(args[[1]], wrapper_name = wrapper_name),
      collect_interaction_vars(args[[2]], wrapper_name = wrapper_name)
    )
  } else {
    abort_unsupported_constraint_expr(expr, if (is.null(wrapper_name)) "formula" else wrapper_name)
  }
}

#' Create a term specification for exact matching
#' @keywords internal
create_exact_term <- function(expr) {
  structure(
    list(
      type = "exact",
      variables = as.character(expr),
      interaction = NULL,
      term_id = create_term_id("exact", expr)
    ),
    class = "raking_term"
  )
}

#' Create a term specification for l2/kl constraints
#' @keywords internal
create_constraint_term <- function(type, expr, wrapper_name = paste0("rr_", type)) {
  # Handle possible interactions within constraint
  if (rlang::is_call(expr, ":")) {
    # Use collect_interaction_vars for consistent n-way interaction handling
    vars <- collect_interaction_vars(expr, wrapper_name = wrapper_name)
    var_names <- unname(vapply(vars, as.character, character(1)))
    structure(
      list(
        type = type,
        variables = var_names,
        interaction = vars,
        term_id = create_term_id(type, expr)
      ),
      class = "raking_term"
    )
  } else {
    if (!rlang::is_symbol(expr)) {
      abort_unsupported_constraint_expr(expr, wrapper_name)
    }
    structure(
      list(
        type = type,
        variables = as.character(expr),
        interaction = NULL,
        term_id = create_term_id(type, expr)
      ),
      class = "raking_term"
    )
  }
}

#' Create a term specification for quantile constraints
#' @keywords internal
create_quantile_term <- function(expr, p) {
  if (!rlang::is_symbol(expr)) {
    abort_unsupported_constraint_expr(expr, "rr_quantile")
  }
  structure(
    list(
      type = "quantile",
      variables = as.character(expr),
      interaction = NULL,
      params = list(p = p),
      term_id = create_term_id("quantile", expr)
    ),
    class = "raking_term"
  )
}

#' Create a term specification for range (inequality) constraints
#' @param expr Variable expression
#' @param margin Symmetric margin around target (mode = "margin")
#' @param lower Lower bound (mode = "bounds")
#' @param upper Upper bound (mode = "bounds")
#' @keywords internal
create_range_term <- function(expr, margin = NULL, lower = NULL, upper = NULL) {
  params <- if (!is.null(margin)) {
    list(mode = "margin", margin = margin)
  } else {
    list(mode = "bounds", lower = lower, upper = upper)
  }

  # Handle interactions
  if (rlang::is_call(expr, ":")) {
    vars <- collect_interaction_vars(expr, wrapper_name = "rr_range")
    var_names <- unname(vapply(vars, as.character, character(1)))

    return(structure(
      list(
        type = "range",
        variables = var_names,
        interaction = vars,
        params = params,
        term_id = create_term_id("range", expr)
      ),
      class = "raking_term"
    ))
  }

  if (!rlang::is_symbol(expr)) {
    abort_unsupported_constraint_expr(expr, "rr_range")
  }

  structure(
    list(
      type = "range",
      variables = as.character(expr),
      interaction = NULL,
      params = params,
      term_id = create_term_id("range", expr)
    ),
    class = "raking_term"
  )
}

#' Create a term specification for interactions
#' @keywords internal
create_interaction_term <- function(vars) {
  # vars is now a list of expressions representing individual variables
  var_names <- unname(vapply(vars, as.character, character(1)))
  structure(
    list(
      type = "exact", # Interactions default to exact matching
      variables = var_names, # No names needed, just the vector
      interaction = vars,
      term_id = create_term_id("exact", rlang::call2(":", !!!vars))
    ),
    class = "raking_term"
  )
}

#' Create a unique identifier for a term
#' @keywords internal
create_term_id <- function(type, expr) {
  paste0(type, "_", digest::digest(expr, algo = "xxhash64"))
}

#' Print method for raking_formula objects
#'
#' @description
#' Displays a human-readable representation of a raking formula.
#' This function shows each term in the formula in a structured format,
#' making it easy to understand complex formulas with multiple constraints.
#'
#' @param x A raking_formula object
#' @param ... Additional arguments passed to other methods
#' @return Invisibly returns the object
#' @export
print.raking_formula <- function(x, ...) {
  cat("Raking Formula Specification:\n")
  cat("Variables:", paste(x$variables, collapse = ", "), "\n")
  cat("Terms:\n")
  for (term in x$terms) {
    cat(
      "  -",
      term$type,
      ":",
      if (!is.null(term$interaction)) {
        paste(vapply(term$interaction, deparse, character(1)), collapse = ":")
      } else {
        paste(term$variables, collapse = " + ")
      },
      "\n"
    )
  }
  invisible(x)
}

#' Raking Constraint Functions
#'
#' These functions specify constraint types for raking formulas. They are used
#' within formula specifications passed to [regrake()].
#'
#' @param x Variable name (unquoted) to apply the constraint to
#' @param p For `rr_quantile`, the quantile probability (0 to 1, e.g., 0.5 for median)
#'
#' @details
#' - `rr_exact()`: Exact equality constraint (weighted sum equals target exactly)
#' - `rr_l2()`: Soft L2/least squares constraint (penalizes deviation from target)
#' - `rr_kl()`: KL divergence constraint
#' - `rr_mean()`: Match the mean of a continuous variable (alias for rr_exact on continuous)
#' - `rr_var()`: Match the variance of a continuous variable
#' - `rr_quantile()`: Match a specific quantile of a continuous variable
#'
#' @return The input variable (these functions are markers for the formula parser)
#'
#' @examples
#' # Match sex proportions exactly, age proportions with soft constraint
#' formula <- ~ rr_exact(sex) + rr_l2(age)
#'
#' # Match categorical variable and continuous mean
#' formula <- ~ rr_exact(region) + rr_mean(income)
#'
#' # Match mean and variance of a continuous variable
#' formula <- ~ rr_mean(income) + rr_var(income)
#'
#' # Match median income
#' formula <- ~ rr_quantile(income, 0.5)
#'
#' @name rr_constraints
#' @export
rr_l2 <- function(x) {
  x
}

#' @rdname rr_constraints
#' @export
rr_kl <- function(x) {
  x
}

#' @rdname rr_constraints
#' @export
rr_exact <- function(x) {
  x
}

#' @rdname rr_constraints
#' @export
rr_mean <- function(x) {
  x
}

#' @rdname rr_constraints
#' @export
rr_var <- function(x) {
  x
}

#' @rdname rr_constraints
#' @export
rr_quantile <- function(x, p) {
  x
}

#' @rdname rr_constraints
#' @param ... For `rr_range`/`rr_between`: either a single margin value (or named
#'   vector for level-specific margins), or separate lower and upper bounds.
#'   Examples: `rr_range(x, 0.02)`, `rr_range(x, c(A=0.01, B=0.02))`,
#'   `rr_range(x, 40, 45)`, `rr_range(x, lower=40, upper=45)`
#' @export
rr_range <- function(x, ...) {
  x
}

#' @rdname rr_constraints
#' @export
rr_between <- function(x, ...) {
  x
}
