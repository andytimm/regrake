#' @title Target Value Computation for Raking
#' @description
#' This file contains functions for computing target values used in raking.
#' The main workflow is:
#' 1. User provides population data in one of several supported formats
#' 2. Data is converted to a standardized "autumn" format (which I think is most elegant)
#' 3. Target values are computed for each term in the raking formula based on the formula specification
#'
#' Supported formats:
#' - proportions: "autumn" style data frame with variable, level, target columns
#' - weighted: Unit-level data with weights column
#' - anesrake: List of named vectors (anesrake package format)
#' - survey: Data frame with margin, category, value columns
#' - survey_design: survey package design object
#' - raw: Unit-level data to compute targets from
#'
#' The autumn format is a standardized data frame with columns:
#' - variable: Name of the variable or interaction (e.g., "age" or "race:age")
#' - level: Level within the variable (for categorical) or statistic name (for continuous, e.g., "mean")
#' - target: Target value for that level (proportion for categorical, value for continuous)
#'
#' @section File Structure:
#' - process_pop_data: Main entry point for format detection and conversion
#' - process_proportions_data: Proportions/autumn format processor
#' - process_*_data functions: Other format processors (weighted, raw, anesrake, survey, survey_design)
#' - compute_target_values: Main function to compute raking targets
#' - expand_joint_distribution: Helper for computing joint distributions

#' Normalize target proportions to sum to 1
#'
#' Checks that target proportions sum to approximately 1 and normalizes them.
#' Silent for tiny deviations (< 0.1%), warns for small deviations (< 5%),
#' errors for large deviations (>= 5%).
#'
#' @param targets Numeric vector of target proportions
#' @param label Variable name for error/warning messages
#' @return Normalized targets summing to exactly 1
#' @keywords internal
normalize_target_sum <- function(targets, label) {
  s <- sum(targets)
  diff <- abs(s - 1)

  if (diff <= 1e-3) {
    return(targets / s)
  }

  if (diff <= 0.05) {
    rlang::warn(
      c(
        paste0(
          "Targets for '", label, "' sum to ",
          format(s, digits = 4), ", normalizing to 1.0."
        ),
        i = "Verify your target values are correct."
      ),
      class = "regrake_target_normalized"
    )
    return(targets / s)
  }

  stop(
    "Targets for variable '", label, "' sum to ", round(s, 4),
    " which is too far from 1.0 to auto-normalize. Please check your targets.",
    call. = FALSE
  )
}

#' Process population data into target values
#' @description
#' Main entry point for converting population data into the autumn format.
#' Detects the input format and dispatches to the appropriate processor.
#'
#' @section Format Detection Rules:
#' - proportions: Has variable, level, target (or proportion) columns
#' - weighted: Has specified weights column
#' - anesrake: List of named numeric vectors
#' - survey: Has margin, category, value columns
#' - survey_design: Inherits from survey.design
#' - raw: Default if no other format matches
#'
#' @param population_data The population data in one of the supported formats
#' @param pop_type The type of population data format
#' @param pop_weights Column name for weights (if pop_type = "weighted")
#' @param formula_spec Parsed formula specification (required for survey_design format)
#'
#' @keywords internal
process_pop_data <- function(population_data, pop_type, pop_weights = NULL, formula_spec = NULL) {
  # Check for either "target" (preferred) or "proportion" (legacy) column
  has_target_col <- "target" %in% names(population_data)
  has_proportion_col <- "proportion" %in% names(population_data)
  has_required_cols <- all(
    c("variable", "level") %in% names(population_data)
  ) &&
    (has_target_col || has_proportion_col)

  # For proportions format, validate required columns first
  if (pop_type == "proportions" && !has_required_cols) {
    stop("must contain columns: variable, level, target", call. = FALSE)
  }

  # Detect format from data structure
  if (has_required_cols) {
    format <- "proportions"
  } else if (!is.null(pop_weights) && pop_weights %in% names(population_data)) {
    format <- "weighted"
  } else if (
    is.list(population_data) &&
      !is.data.frame(population_data) &&
      all(vapply(population_data, is.numeric, logical(1)))
  ) {
    format <- "anesrake"
  } else if (
    is.data.frame(population_data) &&
      all(c("margin", "category", "value") %in% names(population_data))
  ) {
    format <- "survey"
  } else if (inherits(population_data, "survey.design")) {
    format <- "survey_design"
  } else {
    format <- "raw"
  }

  # Validate against requested type
  if (format != pop_type) {
    stop(
      "Population data appears to be in '",
      format,
      "' format but pop_type = '",
      pop_type,
      "'.\n",
      "For '",
      pop_type,
      "' format, data should ",
      switch(
        pop_type,
        proportions = "contain columns: variable, level, target",
        weighted = paste0("contain weight column: ", pop_weights),
        anesrake = "be a list of named numeric vectors (anesrake format)",
        survey = "contain columns: margin, category, value",
        survey_design = "be a survey.design object",
        raw = "have one row per unit"
      ),
      call. = FALSE
    )
  }

  # Process based on format
  switch(
    format,
    proportions = process_proportions_data(population_data),
    weighted = process_weighted_data(population_data, pop_weights),
    anesrake = process_anesrake_data(population_data),
    survey = process_survey_data(population_data),
    survey_design = process_survey_design_data(population_data, formula_spec),
    raw = process_raw_data(population_data)
  )
}

#' Process data already in proportions format
#' @description
#' Validates and standardizes data already in the autumn format.
#' Ensures proper column types and handles factors/characters.
#'
#' @section Validation:
#' - Required columns: variable, level, target (or proportion for backwards compatibility)
#' - Column types: character/factor for variable/level, numeric for target
#'
#' @keywords internal
process_proportions_data <- function(data) {
  # Check for either "target" (preferred) or "proportion" (legacy) column
  has_target <- "target" %in% names(data)
  has_proportion <- "proportion" %in% names(data)

  # Validate autumn format
  if (
    !all(c("variable", "level") %in% names(data)) ||
      !(has_target || has_proportion)
  ) {
    stop("must contain columns: variable, level, target", call. = FALSE)
  }

  # Use target column if present, otherwise use proportion (legacy support)
  value_col <- if (has_target) "target" else "proportion"

  # Validate column types first
  if (
    !is.character(data$variable) &&
      !is.factor(data$variable) ||
      !is.character(data$level) && !is.factor(data$level) ||
      !is.numeric(data[[value_col]]) &&
        !all(suppressWarnings(!is.na(as.numeric(data[[value_col]]))))
  ) {
    stop("Invalid column types", call. = FALSE)
  }

  # Ensure proper types
  data$variable <- as.character(data$variable)
  data$level <- as.character(data$level)

  # Standardize to "target" column name
  data$target <- as.numeric(data[[value_col]])
  if (has_proportion && !has_target) {
    data$proportion <- NULL # Remove legacy column after copying
  }

  # Return with standardized column name
  data
}

#' Process raw unit-level data
#' @description
#' Converts raw data where each row is a unit into target values.
#' Computes proportions for categorical variables and means for continuous.
#'
#' @section Processing Steps:
#' 1. Identify variable types (categorical vs continuous)
#' 2. Categorical: compute counts and convert to proportions
#' 3. Continuous: compute mean (level = "mean")
#' 4. Format in autumn format
#'
#' @keywords internal
process_raw_data <- function(data) {
  # Convert raw counts to target values
  # For each variable, compute proportions within groups
  vars <- names(data)
  result <- vector("list", length(vars))
  names(result) <- vars

  for (var in vars) {
    if (is.numeric(data[[var]])) {
      # Continuous variable: compute mean
      result[[var]] <- tibble::tibble(
        variable = var,
        level = "mean",
        target = mean(data[[var]], na.rm = TRUE)
      )
    } else if (is.factor(data[[var]]) || is.character(data[[var]])) {
      # Categorical variable: compute proportions
      counts <- table(data[[var]])
      props <- as.numeric(counts) / sum(counts)

      result[[var]] <- tibble::tibble(
        variable = var,
        level = names(counts),
        target = props
      )
    }
    # Skip other types (e.g., dates, complex)
  }

  # Combine all results
  do.call(rbind, result[!vapply(result, is.null, logical(1))])
}

#' Process weighted data
#' @description
#' Converts data with sampling weights into target values.
#' Uses weights to compute weighted proportions for categorical variables
#' and weighted means for continuous variables.
#'
#' @section Processing Steps:
#' 1. Validate weight column exists
#' 2. Identify variable types (categorical vs continuous)
#' 3. Categorical: compute weighted proportions
#' 4. Continuous: compute weighted mean (level = "mean")
#' 5. Format in autumn format
#'
#' @keywords internal
process_weighted_data <- function(data, weights) {
  if (is.null(weights)) {
    stop(
      "weights must be specified for weighted population data",
      call. = FALSE
    )
  }

  if (!weights %in% names(data)) {
    stop(
      "weight column '",
      weights,
      "' not found in population data",
      call. = FALSE
    )
  }

  # Convert weighted counts to target values
  # For each variable, compute weighted proportions within groups
  vars <- names(data)
  result <- vector("list", length(vars))
  names(result) <- vars
  wts <- data[[weights]]

  for (var in vars) {
    # Skip weight column
    if (var == weights) {
      next
    }

    if (is.numeric(data[[var]])) {
      # Continuous variable: compute weighted mean
      wtd_mean <- sum(data[[var]] * wts) / sum(wts)
      result[[var]] <- tibble::tibble(
        variable = var,
        level = "mean",
        target = wtd_mean
      )
    } else if (is.factor(data[[var]]) || is.character(data[[var]])) {
      # Categorical variable: compute weighted proportions
      wtd_props <- stats::aggregate(
        wts,
        list(level = data[[var]]),
        sum
      )
      wtd_props$target <- wtd_props$x / sum(wtd_props$x)

      result[[var]] <- tibble::tibble(
        variable = var,
        level = wtd_props$level,
        target = wtd_props$target
      )
    }
    # Skip other types
  }

  # Combine all results
  do.call(rbind, result[!vapply(result, is.null, logical(1))])
}

#' Process anesrake format data
#' @description
#' Converts anesrake-style list of named vectors into autumn format.
#' Each vector represents target values for one variable.
#'
#' @section Validation:
#' - Each element must be a named numeric vector
#' - Values must sum to 1
#'
#' @keywords internal
process_anesrake_data <- function(data) {
  result <- vector("list", length(data))
  for (i in seq_along(data)) {
    var_name <- names(data)[i]
    props <- data[[i]]

    if (!is.numeric(props) || is.null(names(props))) {
      stop(
        "Each element in anesrake format must be a named numeric vector",
        call. = FALSE
      )
    }

    props <- normalize_target_sum(props, var_name)

    result[[i]] <- tibble::tibble(
      variable = var_name,
      level = names(props),
      target = unname(props)
    )
  }

  do.call(rbind, result)
}

#' Process survey format data
#' @description
#' Converts survey-style margin data into autumn format.
#' Handles both main effects and interactions via margin column.
#'
#' @section Processing:
#' - Single variables use margin name as variable
#' - Interactions (with :) are preserved as-is
#' - Validates targets sum to 1 within margins
#'
#' @keywords internal
process_survey_data <- function(data) {
  # Validate required columns
  if (!all(c("margin", "category", "value") %in% names(data))) {
    stop(
      "Survey format requires columns: margin, category, value",
      call. = FALSE
    )
  }

  # Check for and handle two-way margins
  margins <- unique(data$margin)
  result <- vector("list", length(margins))

  for (i in seq_along(margins)) {
    margin <- margins[i]
    margin_data <- data[data$margin == margin, ]

    result[[i]] <- tibble::tibble(
      variable = margin,
      level = margin_data$category,
      target = margin_data$value
    )

    result[[i]]$target <- normalize_target_sum(result[[i]]$target, margin)
  }

  do.call(rbind, result)
}

#' Process survey design object
#' @description
#' Extracts population information from a survey design object.
#' Uses the formula specification from regrake to determine which variables
#' to extract, and the design weights to compute target values.
#'
#' @section Processing Steps:
#' 1. Validate design object and extract variables/weights
#' 2. Use formula_spec to determine which variables are needed
#' 3. For categorical variables: compute weighted proportions
#' 4. For continuous variables: compute weighted mean
#' 5. For interactions: compute joint distribution
#' 6. Format in autumn format
#'
#' @param design A survey.design object from the survey package
#' @param formula_spec A parsed raking formula specification from parse_raking_formula
#'
#' @keywords internal
process_survey_design_data <- function(design, formula_spec) {
  # Validate design object
  if (!inherits(design, "survey.design")) {
    stop("Expected a survey.design object from the survey package", call. = FALSE)
  }

  if (is.null(formula_spec)) {
    stop(
      "formula_spec is required for survey_design population data.\n",
      "Please provide a formula when calling regrake().",
      call. = FALSE
    )
  }

  # Extract data and weights from design
  data <- design$variables
  wts <- weights(design)
  total_weight <- sum(wts)

  # Get all unique variables from formula_spec
  vars <- formula_spec$variables

  # Check that all variables exist in the design
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    stop(
      "Variable(s) not found in survey design: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  # Process main effects for each variable
  result_list <- list()

  for (var in vars) {
    col <- data[[var]]

    if (is.numeric(col) && !is.factor(col)) {
      # Continuous variable: compute weighted mean
      wtd_mean <- sum(col * wts) / total_weight
      result_list[[var]] <- tibble::tibble(
        variable = var,
        level = "mean",
        target = wtd_mean
      )
    } else {
      # Categorical variable: compute weighted proportions
      if (is.character(col)) {
        col <- factor(col)
      }

      # Aggregate weights by level
      level_weights <- tapply(wts, col, sum, default = 0)
      level_props <- level_weights / total_weight

      result_list[[var]] <- tibble::tibble(
        variable = var,
        level = names(level_props),
        target = as.numeric(level_props)
      )
    }
  }

  # Process interactions from formula_spec$terms
  for (term in formula_spec$terms) {
    if (!is.null(term$interaction)) {
      interaction_vars <- term$variables
      joint_var <- paste(interaction_vars, collapse = ":")

      # Skip if we've already processed this interaction
      if (joint_var %in% names(result_list)) {
        next
      }

      # Create combined factor for the interaction
      interaction_data <- lapply(interaction_vars, function(v) {
        col <- data[[v]]
        if (is.character(col)) factor(col) else col
      })
      combined <- interaction(interaction_data, drop = TRUE, sep = ":")

      # Compute weighted proportions for joint distribution
      level_weights <- tapply(wts, combined, sum, default = 0)
      level_props <- level_weights / total_weight

      result_list[[joint_var]] <- tibble::tibble(
        variable = joint_var,
        level = names(level_props),
        target = as.numeric(level_props)
      )
    }
  }

  # Combine all results
  result <- do.call(rbind, result_list)

  # Validate targets sum to 1 for categorical variables
  # (skip continuous variables which have "mean" level)
  var_sums <- tapply(result$target, result$variable, sum)
  categorical_vars <- names(var_sums)[!names(var_sums) %in% names(result_list)[
    vapply(result_list, function(x) any(x$level == "mean"), logical(1))
  ]]

  for (v in categorical_vars) {
    var_rows <- result$variable == v
    result$target[var_rows] <- normalize_target_sum(result$target[var_rows], v)
  }

  result
}

#' Compute target values from population data
#'
#' @description
#' Main function for computing raking target values.
#' Takes population data and a formula specification,
#' returns target values for each term.
#'
#' @section Workflow:
#' 1. Convert population data to autumn format
#' 2. Validate data structure and types
#' 3. Check for duplicates and validate targets
#' 4. Process each formula term:
#'    - Main effects: Extract directly
#'    - Interactions: Use joint distribution or compute from marginals
#'
#' @param population_data A data.frame in autumn format with columns:
#'   - variable: The variable name
#'   - level: The level within the variable (or statistic name for continuous)
#'   - target: The target value (proportion for categorical, value for continuous)
#'   For interactions, specify joint distributions by combining variable names with ":"
#'   e.g., "race:age" for a race by age interaction.
#' @param formula_spec A parsed raking formula specification from parse_raking_formula
#' @param pop_type How population data is specified ("raw", "weighted", "proportions")
#' @param pop_weights Column name in population_data containing weights (if pop_type = "weighted")
#'
#' @return A list containing:
#'   - targets: Named list of target values for each formula term
#'   - variables: Vector of all variables in the data
#'
#' @keywords internal
compute_target_values <- function(
  population_data,
  formula_spec,
  pop_type = "proportions",
  pop_weights = NULL
) {
  # First convert population data to autumn format regardless of input type
  population_data <- process_pop_data(population_data, pop_type, pop_weights, formula_spec)

  # Check for duplicate variable-level combinations within each variable
  dups <- tapply(population_data$level, population_data$variable, duplicated)
  if (any(unlist(dups))) {
    stop("Duplicate variable-level combination", call. = FALSE)
  }

  # Validate targets sum to 1 for each variable (categorical variables only)
  # Skip validation for continuous targets (level contains statistic names like "mean", "var", etc.)
  continuous_stat_levels <- c(
    "mean",
    "var",
    "sd",
    "median",
    "min",
    "max",
    "q10",
    "q25",
    "q50",
    "q75",
    "q90"
  )

  # Identify which variables are continuous (have statistic-like levels)
  is_continuous_var <- tapply(
    population_data$level,
    population_data$variable,
    function(lvls) {
      all(lvls %in% continuous_stat_levels)
    }
  )

  # Only validate and normalize categorical variables (those not identified as continuous)
  categorical_vars <- names(is_continuous_var)[!is_continuous_var]
  if (length(categorical_vars) > 0) {
    for (var_name in categorical_vars) {
      var_rows <- population_data$variable == var_name
      population_data$target[var_rows] <- normalize_target_sum(
        population_data$target[var_rows], var_name
      )
    }
  }

  # Process each term in the formula
  targets <- vector("list", length(formula_spec$terms))
  names(targets) <- vapply(
    formula_spec$terms,
    function(t) {
      paste0(
        t$type,
        "_",
        if (is.null(t$interaction)) {
          t$variables
        } else {
          paste(t$variables, collapse = ":")
        }
      )
    },
    character(1)
  )

  for (i in seq_along(formula_spec$terms)) {
    term <- formula_spec$terms[[i]]

    if (is.null(term$interaction)) {
      # Main effect
      var_data <- population_data[population_data$variable == term$variables, ]
      if (nrow(var_data) == 0) {
        stop(
          "Missing target values for variable: ",
          term$variables,
          call. = FALSE
        )
      }
      targets[[i]] <- setNames(var_data$target, var_data$level)
    } else {
      # Interaction term
      joint_var <- paste(term$variables, collapse = ":")
      var_data <- population_data[population_data$variable == joint_var, ]

      if (nrow(var_data) == 0) {
        stop("Missing target values for variable: ", joint_var, call. = FALSE)
      }
      targets[[i]] <- setNames(var_data$target, var_data$level)
    }
  }

  # Return results
  list(
    targets = targets,
    variables = unique(population_data$variable)
  )
}
