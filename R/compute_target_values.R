#' Process population data into target proportions
#' @keywords internal
process_population_data <- function(population_data, pop_type, pop_weights = NULL) {
  # Detect format from data structure
  if (all(c("variable", "level", "proportion") %in% names(population_data))) {
    format <- "proportions"
  } else if (!is.null(pop_weights) && pop_weights %in% names(population_data)) {
    format <- "weighted"
  } else {
    format <- "raw"
  }

  # Validate against requested type
  if (format != pop_type) {
    stop(
      "Population data appears to be in '", format, "' format but pop_type = '", pop_type, "'.\n",
      "For '", pop_type, "' format, data should ",
      switch(pop_type,
        proportions = "contain columns: variable, level, proportion",
        weighted = paste0("contain weight column: ", pop_weights),
        raw = "have one row per unit"
      ),
      call. = FALSE
    )
  }

  # Create a population data object with appropriate class
  pop_obj <- structure(
    list(
      data = population_data,
      weights = pop_weights
    ),
    class = c(format, "population_data")
  )

  # Dispatch to appropriate method
  UseMethod("process_population_data", pop_obj)
}

#' @export
process_population_data.proportions <- function(pop_obj, ...) {
  # Already in correct format, just validate
  data <- pop_obj$data

  # Validate autumn format
  if (!all(c("variable", "level", "proportion") %in% names(data))) {
    stop("Population data must contain columns: variable, level, proportion", call. = FALSE)
  }

  # Return as is
  data
}

#' @export
process_population_data.raw <- function(pop_obj, ...) {
  data <- pop_obj$data

  # Convert raw counts to proportions
  # For each variable, compute proportions within groups
  vars <- names(data)
  result <- vector("list", length(vars))
  names(result) <- vars

  for (var in vars) {
    # Skip any metadata columns
    if (!is.factor(data[[var]]) && !is.character(data[[var]])) next

    # Compute counts and convert to proportions
    counts <- table(data[[var]])
    props <- as.numeric(counts) / sum(counts)

    # Add to result in autumn format
    result[[var]] <- tibble::tibble(
      variable = var,
      level = names(counts),
      proportion = props
    )
  }

  # Handle interactions if present
  # TODO: Implement interaction handling for raw data

  # Combine all results
  do.call(rbind, result[!sapply(result, is.null)])
}

#' @export
process_population_data.weighted <- function(pop_obj, ...) {
  data <- pop_obj$data
  weights <- pop_obj$weights

  if (is.null(weights)) {
    stop("weights must be specified for weighted population data", call. = FALSE)
  }

  if (!weights %in% names(data)) {
    stop("weight column '", weights, "' not found in population data", call. = FALSE)
  }

  # Convert weighted counts to proportions
  # For each variable, compute weighted proportions within groups
  vars <- names(data)
  result <- vector("list", length(vars))
  names(result) <- vars

  for (var in vars) {
    # Skip weight column and any metadata
    if (var == weights || (!is.factor(data[[var]]) && !is.character(data[[var]]))) next

    # Compute weighted proportions
    wtd_props <- stats::aggregate(
      data[[weights]],
      list(level = data[[var]]),
      sum
    )
    wtd_props$proportion <- wtd_props$x / sum(wtd_props$x)

    # Add to result in autumn format
    result[[var]] <- tibble::tibble(
      variable = var,
      level = wtd_props$level,
      proportion = wtd_props$proportion
    )
  }

  # Handle interactions if present
  # TODO: Implement interaction handling for weighted data

  # Combine all results
  do.call(rbind, result[!sapply(result, is.null)])
}

#' Compute target values from population data
#'
#' @param population_data A data.frame in autumn format with columns:
#'   - variable: The variable name
#'   - level: The level within the variable
#'   - proportion: The target proportion
#'   For interactions, specify joint distributions by combining variable names with ":"
#'   e.g., "race:age" for a race by age interaction.
#' @param formula_spec A parsed raking formula specification from parse_raking_formula
#' @param pop_type How population data is specified ("raw", "weighted", "proportions")
#' @param pop_weights Column name in population_data containing weights (if pop_type = "weighted")
#'
#' @return A list containing target values for each term in the formula
#' @keywords internal
compute_target_values <- function(population_data, formula_spec, pop_type = "proportions", pop_weights = NULL) {
  # First convert population data to autumn format regardless of input type
  population_data <- process_population_data(population_data, pop_type, pop_weights)

  # Validate autumn format
  if (!all(c("variable", "level", "proportion") %in% names(population_data))) {
    stop("Population data must contain columns: variable, level, proportion", call. = FALSE)
  }

  # Check that all required variables are present in population data
  # For interactions, check both the individual variables and the joint specification
  missing_vars <- character(0)
  for (term in formula_spec$terms) {
    if (is.null(term$interaction)) {
      # Main effect - check variable exists
      if (!term$variables %in% unique(population_data$variable)) {
        missing_vars <- c(missing_vars, term$variables)
      }
    } else {
      # Interaction - check both individual variables and joint specification
      joint_var <- paste(term$variables, collapse = ":")
      if (!joint_var %in% unique(population_data$variable)) {
        # If joint distribution not found, check individual variables
        missing_individual <- setdiff(term$variables, unique(population_data$variable))
        if (length(missing_individual) > 0) {
          missing_vars <- c(missing_vars, missing_individual)
        }
      }
    }
  }

  if (length(missing_vars) > 0) {
    stop("Missing target values for variables: ",
         paste(unique(missing_vars), collapse = ", "), call. = FALSE)
  }

  # Validate proportions sum to 1 within each variable (with tolerance)
  var_sums <- tapply(population_data$proportion, population_data$variable, sum)
  tolerance <- 1e-6
  bad_vars <- names(var_sums)[abs(var_sums - 1) > tolerance]
  if (length(bad_vars) > 0) {
    stop("Proportions must sum to 1 for each variable. Check variables: ",
         paste(bad_vars, collapse = ", "), call. = FALSE)
  }

  # Process each term in the formula
  target_values <- vector("list", length(formula_spec$terms))
  names(target_values) <- vapply(formula_spec$terms, function(t) t$term_id, character(1))

  for (i in seq_along(formula_spec$terms)) {
    term <- formula_spec$terms[[i]]

    if (is.null(term$interaction)) {
      # Main effect - just get the proportions for this variable
      var_data <- population_data[population_data$variable == term$variables, ]
      target_values[[i]] <- list(
        type = term$type,
        values = setNames(var_data$proportion, var_data$level)
      )
    } else {
      # Interaction term - first check if joint distribution is provided
      joint_var <- paste(term$variables, collapse = ":")
      if (joint_var %in% unique(population_data$variable)) {
        # Use provided joint distribution
        var_data <- population_data[population_data$variable == joint_var, ]
        target_values[[i]] <- list(
          type = term$type,
          values = setNames(var_data$proportion, var_data$level)
        )
      } else {
        # Compute joint distribution from marginals (independence assumption)
        warning("Joint distribution for ", joint_var, " not found in population data. ",
                "Computing from marginals assuming independence.", call. = FALSE)
        var_props <- list()
        for (var in term$variables) {
          var_data <- population_data[population_data$variable == var, ]
          var_props[[var]] <- setNames(var_data$proportion, var_data$level)
        }
        joint_props <- expand_joint_distribution(var_props)
        target_values[[i]] <- list(
          type = term$type,
          values = joint_props
        )
      }
    }
  }

  target_values
}

#' Expand marginal distributions into a joint distribution
#' @keywords internal
expand_joint_distribution <- function(marginals) {
  # Start with first variable's levels
  result <- marginals[[1]]
  var1_name <- names(marginals)[1]
  names(result) <- paste0(var1_name, ":", names(result))

  # Multiply by each subsequent variable's levels
  if (length(marginals) > 1) {
    for (i in 2:length(marginals)) {
      var_name <- names(marginals)[i]
      var_props <- marginals[[i]]

      # Create all combinations with current result
      new_result <- numeric()
      for (existing_name in names(result)) {
        for (level_name in names(var_props)) {
          # Build name in format var1:level1:var2:level2
          new_name <- paste0(existing_name, ":", var_name, ":", level_name)
          new_result[new_name] <- result[existing_name] * var_props[level_name]
        }
      }
      result <- new_result
    }
  }

  result
}