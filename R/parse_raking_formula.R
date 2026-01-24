#' Parse a raking formula
#'
#' @description
#' Parses a formula that specifies raking constraints. The formula interface provides
#' a natural R-like syntax for specifying both main effects and interactions, with
#' optional constraint types for each term.
#'
#' @param formula A formula specifying raking constraints (e.g., ~ race + l2(age:educ))
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
  if (rlang::quo_is_missing(rlang::quo(!!rhs)) ||
      identical(rhs, 1L) ||
      identical(rhs, 1)) {
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
  structure(list(
    terms = terms,
    variables = variables,
    interactions = interactions[!vapply(interactions, is.null, logical(1))],
    formula = formula  # Include the original formula
  ), class = "raking_formula")
}

#' Validate that there are no invalid overlapping constraints
#' @keywords internal
validate_overlapping_constraints <- function(terms) {
  # Check for pure duplicates first
  for (i in seq_along(terms)) {
    for (j in seq_len(i-1)) {
      term_i <- terms[[i]]
      term_j <- terms[[j]]

      # If both terms have same variables and one is not an interaction while the other is
      # or both are interactions/non-interactions, then it's a pure duplicate
      if (identical(sort(term_i$variables), sort(term_j$variables)) &&
          (is.null(term_i$interaction) == is.null(term_j$interaction))) {

        var_desc <- if (is.null(term_i$interaction)) {
          paste0("Variable '", term_i$variables, "'")
        } else {
          paste0("Interaction '",
                paste(vapply(term_i$interaction, deparse, character(1)), collapse = ":"),
                "'")
        }

        if (term_i$type != term_j$type) {
          stop(var_desc, " appears multiple times with different constraints", call. = FALSE)
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
        warning(paste0("Variables in rr_", term$type, "(",
                paste(vapply(term$interaction, deparse, character(1)), collapse = ":"),
                ") also appear as main effects. Using exact constraints for main effects and ",
                "rr_", term$type, " constraint for the interaction term"),
                call. = FALSE)
      }
    }
  }
}

#' Parse formula terms recursively
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
    c(parse_formula_terms(args[[1]]),
      parse_formula_terms(args[[2]]))
  } else if (fun %in% c("rr_l2", "rr_kl", "rr_exact", "rr_mean")) {
    # Handle constraint functions
    # rr_mean maps to exact constraint (for continuous variables)
    internal_type <- if (fun == "rr_mean") "exact" else sub("^rr_", "", fun)
    list(create_constraint_term(internal_type, args[[1]]))
  } else if (fun == ":") {
    # Handle interactions by recursively collecting all variables
    list(create_interaction_term(collect_interaction_vars(expr)))
  } else {
    # Default to exact matching for unknown functions
    warning("Unknown function '", fun, "', defaulting to exact constraint", call. = FALSE)
    list(create_exact_term(args[[1]]))
  }
}

#' Recursively collect variables from an interaction expression
#' @keywords internal
collect_interaction_vars <- function(expr) {
  if (!rlang::is_call(expr)) {
    # Base case: single variable
    return(list(expr))
  }

  fun <- rlang::call_name(expr)
  args <- rlang::call_args(expr)

  if (fun == ":") {
    # Recursively collect variables from both sides
    c(collect_interaction_vars(args[[1]]),
      collect_interaction_vars(args[[2]]))
  } else {
    # Non-: call, treat as single term
    list(expr)
  }
}

#' Create a term specification for exact matching
#' @keywords internal
create_exact_term <- function(expr) {
  structure(list(
    type = "exact",
    variables = as.character(expr),
    interaction = NULL,
    term_id = create_term_id("exact", expr)
  ), class = "raking_term")
}

#' Create a term specification for l2/kl constraints
#' @keywords internal
create_constraint_term <- function(type, expr) {
  # Handle possible interactions within constraint
  if (rlang::is_call(expr, ":")) {
    # Use collect_interaction_vars for consistent n-way interaction handling
    vars <- collect_interaction_vars(expr)
    var_names <- unname(vapply(vars, as.character, character(1)))
    structure(list(
      type = type,
      variables = var_names,
      interaction = vars,
      term_id = create_term_id(type, expr)
    ), class = "raking_term")
  } else {
    # Handle nested functions by getting the innermost expression
    while(rlang::is_call(expr)) {
      expr <- rlang::call_args(expr)[[1]]
    }
    structure(list(
      type = type,
      variables = as.character(expr),
      interaction = NULL,
      term_id = create_term_id(type, expr)
    ), class = "raking_term")
  }
}

#' Create a term specification for interactions
#' @keywords internal
create_interaction_term <- function(vars) {
  # vars is now a list of expressions representing individual variables
  var_names <- unname(vapply(vars, as.character, character(1)))
  structure(list(
    type = "exact",  # Interactions default to exact matching
    variables = var_names,  # No names needed, just the vector
    interaction = vars,
    term_id = create_term_id("exact", rlang::call2(":", !!!vars))
  ), class = "raking_term")
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
#' @examples
#' formula <- rr_exact(~ race) + rr_l2(~ age:educ)
#' print(formula)
#' @export
print.raking_formula <- function(x, ...) {
  cat("Raking Formula Specification:\n")
  cat("Variables:", paste(x$variables, collapse = ", "), "\n")
  cat("Terms:\n")
  for (term in x$terms) {
    cat("  -", term$type, ":",
        if (!is.null(term$interaction)) {
          paste(vapply(term$interaction, deparse, character(1)), collapse = ":")
        } else {
          paste(term$variables, collapse = " + ")
        },
        "\n")
  }
  invisible(x)
}

#' @export
rr_l2 <- function(x) {
  # When used in formula context, return unevaluated
  x
}

#' @export
rr_kl <- function(x) {
  # When used in formula context, return unevaluated
  x
}

#' @export
rr_exact <- function(x) {
  # When used in formula context, return unevaluated
  x
}

#' @export
rr_mean <- function(x) {
  # When used in formula context, return unevaluated
  # rr_mean is specifically for continuous variables - matches mean exactly
  x
}

#' Validate raking formula inputs
#'
#' @description
#' Validates that a raking formula and the associated data meet all requirements.
#' This function performs comprehensive checks on both the formula structure and the data,
#' ensuring that the parsing process can proceed without errors.
#'
#' @details
#' The validation process includes:
#' \itemize{
#'   \item Checking that the formula has the correct structure and types
#'   \item Verifying that all variables referenced in the formula exist in the data
#'   \item Validating that all variables have supported types (numeric, factor, or character)
#'   \item Ensuring that no variables contain missing values
#'   \item Warning about empty factor levels
#' }
#'
#' @param formula A raking formula or term
#' @param data A data frame containing the variables
#' @return Invisibly returns TRUE if validation passes, otherwise stops with an error
#' @keywords internal
validate_inputs <- function(formula, data) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame or tibble", call. = FALSE)
  }

  # Use tryCatch for more informative errors
  tryCatch({
    # Handle both single terms and formula combinations
    terms <- if (inherits(formula, "raking_term")) {
      list(formula)
    } else if (inherits(formula, "raking_formula") || inherits(formula, "rakingformula")) {
      formula
    } else {
      stop("Formula must be created with rr_exact(), rr_l2(), rr_kl(), or combinations thereof",
           call. = FALSE)
    }

    # Extract variables from all terms
    vars <- unique(unlist(lapply(terms, function(term) {
      all.vars(term$formula[[2]])
    })))

    # Check for missing variables
    missing_vars <- setdiff(vars, names(data))
    if (length(missing_vars) > 0) {
      stop(sprintf(
        "Variables not found in data: %s",
        paste(missing_vars, collapse = ", ")
      ), call. = FALSE)
    }

    # Validate each variable
    for (var in vars) {
      validate_variable(var, data[[var]])
    }
  }, error = function(e) {
    # If not already formatted nicely, add context
    if (!grepl("^Variables not found|^Variable '", e$message)) {
      stop(paste0("Formula validation error: ", e$message), call. = FALSE)
    } else {
      stop(e$message, call. = FALSE)
    }
  })

  invisible(TRUE)
}

#' Validate an individual variable
#'
#' @description
#' Validates that a single variable meets all requirements for raking.
#' This function checks the variable's type and structure to ensure it can
#' be properly processed during raking.
#'
#' @details
#' The following checks are performed:
#' \itemize{
#'   \item Type check: must be numeric, factor, or character
#'   \item Missing values check: no NA values allowed
#'   \item Factor level check: warns about empty factor levels
#' }
#'
#' @param name Variable name (for error messages)
#' @param var Variable vector to validate
#' @return Invisibly returns TRUE if validation passes, otherwise stops with an error
#' @keywords internal
validate_variable <- function(name, var) {
  # Check type
  if (!is.numeric(var) && !is.factor(var) && !is.character(var)) {
    stop(sprintf(
      "Variable '%s' has unsupported type: %s. Must be numeric, factor, or character.",
      name, class(var)[1]
    ), call. = FALSE)
  }

  # Check for missing values
  if (any(is.na(var))) {
    stop(sprintf(
      "Variable '%s' contains missing values (NA). All variables must be complete.",
      name
    ), call. = FALSE)
  }

  # Check factor levels
  if (is.factor(var)) {
    empty_levels <- levels(var)[!levels(var) %in% unique(var)]
    if (length(empty_levels) > 0) {
      warning(sprintf(
        "Variable '%s' has empty factor levels: %s",
        name, paste(empty_levels, collapse = ", ")
      ), call. = FALSE)
    }
  }

  invisible(TRUE)
}

#' Extract formula additions (for | syntax)
#'
#' @description
#' Extracts additional specifications from a formula using the pipe-like '|' syntax.
#' This allows for specifying additional parameters that apply to the entire formula.
#'
#' @details
#' This feature is currently a placeholder for future expansion. In the future, it
#' will support syntax like `exact(x + y) | weight(w)
#'
#' @param formula A raking formula or term
#' @return A list of additions or NULL
#' @keywords internal
extract_additions <- function(formula) {
  # Currently a stub that can be expanded later
  # For formulas like exact(x + y) | weight(w)
  NULL
}

#' Create a raking formula term
#'
#' @description
#' Creates a raking constraint term that can be combined with other terms using the `+` operator.
#' Each term represents a specific type of constraint (exact, L2, or KL) and includes a formula
#' specifying which variables to include in the constraint.
#'
#' @details
#' Raking terms are the building blocks of the raking formula interface. Each term specifies
#' a single constraint type (exact matching, L2 distance, or KL divergence) and the variables
#' to which that constraint applies.
#'
#' The formula within a term can include:
#' \itemize{
#'   \item Main effects: \code{~ x + y}
#'   \item Interactions: \code{~ x:y}
#'   \item Transformations: \code{~ I(x^2) + log(y)}
#' }
#'
#' Terms can be combined using the \code{+} operator to create complex raking specifications
#' with multiple constraints.
#'
#' @param formula A one-sided formula specifying variables (e.g., \code{~ race + age})
#' @param type The type of constraint ("exact", "l2", "kl")
#' @param ... Additional arguments passed to specific constraint types
#' @return A raking_term object that can be combined with other terms
#' @examples
#' # Create a term directly
#' raking_term(~ race + age, type = "exact")
#'
#' # Typically used via wrapper functions
#' rr_exact(~ race + age)
#' rr_l2(~ race:educ)
#' @export
raking_term <- function(formula, type = c("exact", "l2", "kl"), ...) {
  type <- match.arg(type)

  # Handle formula environment properly
  formula <- if (is.character(formula)) {
    as.formula(paste("~", formula), env = parent.frame())
  } else if (inherits(formula, "formula")) {
    # Preserve the original environment
    formula
  } else {
    stop("Formula must be a formula object or a character string", call. = FALSE)
  }

  # Ensure one-sided formula and convert if needed
  if (length(formula) > 2) {
    formula <- as.formula(paste("~", deparse(formula[[3]])), env = environment(formula))
  }

  structure(
    list(
      formula = formula,
      type = type,
      args = list(...),
      env = environment(formula)  # Store the environment
    ),
    class = "raking_term"  # Use single class for clearer method dispatch
  )
}

#' Create a raking formula using a flexible interface (Component Interface)
#'
#' @description
#' A convenience function for creating raking terms with a flexible interface.
#' This function allows specifying the constraint type as a parameter rather
#' than using the specific constructor functions.
#'
#' This function is part of the component interface. For a more natural formula
#' syntax, consider using the formula-first interface with \code{regrake()}.
#'
#' @details
#' The `rf()` function provides a more flexible interface for creating raking terms.
#' If no type is specified, it defaults to creating an exact constraint.
#' This is particularly useful in programmatic contexts where the constraint type
#' might be determined at runtime.
#'
#' @param formula A formula specifying variables (e.g., \code{~ race + age})
#' @param ... Additional arguments including 'type' to specify the constraint type
#' @return A raking_term object
#' @examples
#' # Component interface with rf()
#' rf(~ race + age)
#' rf(~ race + age, type = "l2")
#'
#' # Formula-first interface (recommended alternative)
#' # regrake(~ race + age, data=df)
#' # regrake(~ race + rr_l2(age), data=df)
#' @export
rf <- function(formula, ...) {
  args <- list(...)

  if (length(args) == 0) {
    return(rr_exact(formula))
  }

  # If type is specified directly
  if (!is.null(args$type)) {
    type <- args$type
    args$type <- NULL

    # Validate type
    type <- match.arg(type, choices = c("exact", "l2", "kl"))

    # Map to rr_ prefixed function
    rr_type <- paste0("rr_", type)

    # Call the appropriate constructor
    return(do.call(rr_type, c(list(formula), args)))
  }

  # Default to exact
  rr_exact(formula, ...)
}

#' Process a raking term
#'
#' @param term A raking term object
#' @param data A data frame
#' @return A processed term specification
#' @keywords internal
process_term <- function(term, data) {
  # Get the environment from the term
  env <- term$env

  # Extract terms properly, preserving attributes
  terms_obj <- terms(term$formula, data = data)

  # Get term labels and handle interactions
  term_labels <- attr(terms_obj, "term.labels")
  term_order <- attr(terms_obj, "order")

  # Extract variables using base R methods
  vars <- all.vars(term$formula[[2]])
  specs <- list()
  var_specs <- list()
  original_vars <- list()

  # Process each variable
  for (var in vars) {
    if (is.factor(data[[var]]) || is.character(data[[var]])) {
      # Convert character to factor if needed
      if (is.character(data[[var]])) {
        data[[var]] <- factor(data[[var]])
      }

      levels <- levels(data[[var]])

      original_vars[[var]] <- list(
        type = "categorical",
        levels = levels
      )

      # Create indicators for all levels (no reference level)
      for (level in levels) {
        var_name <- paste0(var, level)
        var_specs[[var_name]] <- list(
          type = "indicator",
          parent = var,
          level = level
        )
        specs[[var_name]] <- TRUE
      }
    } else {
      # Continuous variable
      var_specs[[var]] <- list(type = "continuous")
      original_vars[[var]] <- list(type = "continuous")
      specs[[var]] <- TRUE
    }
  }

  # Return a more complete specification
  list(
    constraints = list(list(
      type = term$type,
      term_labels = term_labels,
      term_order = term_order,
      specs = specs,
      interaction = any(term_order > 1)
    )),
    variables = var_specs,
    original_variables = original_vars
  )
}

#' Print method for raking_term objects
#'
#' @description
#' Displays a human-readable representation of a raking term.
#' This function presents the constraint type and the formula
#' in a clear, formatted manner.
#'
#' @param x A raking_term object
#' @param ... Additional arguments passed to other methods
#' @return Invisibly returns the object
#' @examples
#' term <- rr_exact(~ race + age)
#' print(term)
#' @export
print.raking_term <- function(x, ...) {
  cat(x$type, "constraint: ", formula2str(x$formula), "\n")
  if (length(x$args) > 0) {
    cat("Additional arguments:\n")
    print(x$args)
  }
  invisible(x)
}

#' Print method for raking specification objects
#'
#' @description
#' Displays a summary of a parsed raking specification.
#' This function shows the number of constraints and a summary
#' of the variables included in the specification.
#'
#' @param x A raking_spec object
#' @param ... Additional arguments passed to other methods
#' @return Invisibly returns the object
#' @examples
#' df <- data.frame(
#'   race = factor(c("white", "black", "hispanic")),
#'   age = c(25, 35, 45)
#' )
#' spec <- parse_raking_formula(rr_exact(~ race) + rr_l2(~ age), df)
#' print(spec)
#' @export
print.raking_spec <- function(x, ...) {
  cat("Raking specification with", length(x$terms), "constraints\n")
  cat("Variables:\n")
  for (var in names(x$original_variables)) {
    var_info <- x$original_variables[[var]]
    if (var_info$type == "categorical") {
      cat(paste0("  ", var, ": categorical with ", length(var_info$levels), " levels\n"))
    } else {
      cat(paste0("  ", var, ": continuous\n"))
    }
  }
  invisible(x)
}

#' Convert a formula to a string representation
#'
#' @description
#' Converts a formula object to a readable string format.
#' This is an internal helper function used by the print methods.
#'
#' @param x A formula object
#' @return A string representation of the formula
#' @keywords internal
formula2str <- function(x) {
  if (length(x) == 2) {
    paste0("~", deparse(x[[2]]))
  } else if (length(x) == 3) {
    paste0(deparse(x[[2]]), " ~ ", deparse(x[[3]]))
  } else {
    as.character(x)
  }
}

# Create a registry for constraint types
raking_constraints <- new.env(parent = emptyenv())

#' Register a constraint handler
#'
#' @description
#' Registers a function to handle a specific constraint type.
#' This allows for extending the system with new constraint types.
#'
#' @param name Name of the constraint type
#' @param handler Function to handle the constraint
#' @return Invisibly returns TRUE
#' @keywords internal
register_constraint <- function(name, handler) {
  raking_constraints[[name]] <- handler
  invisible(TRUE)
}

#' Get a constraint handler
#'
#' @description
#' Retrieves the handler function for a specific constraint type.
#'
#' @param type Type of constraint
#' @return A handler function for the specified constraint type
#' @keywords internal
get_constraint_handler <- function(type) {
  handler <- raking_constraints[[type]]
  if (is.null(handler)) {
    stop("Unknown constraint type: ", type, call. = FALSE)
  }
  handler
}

# Register default handlers
register_constraint("exact", function(x, targets) {
  # Implementation for exact constraints
  # This is a placeholder for the actual implementation
  list(type = "exact", x = x, targets = targets)
})

register_constraint("l2", function(x, targets) {
  # Implementation for L2 constraints
  # This is a placeholder for the actual implementation
  list(type = "l2", x = x, targets = targets)
})

register_constraint("kl", function(x, targets) {
  # Implementation for KL constraints
  # This is a placeholder for the actual implementation
  list(type = "kl", x = x, targets = targets)
})