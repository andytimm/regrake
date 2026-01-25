#' @title Target Value Computation for Raking
#' @description
#' This file contains functions for computing target values used in raking.
#' The main workflow is:
#' 1. User provides population data in one of several supported formats
#' 2. Data is converted to a standardized "autumn" format (which I think is most elegant)
#' 3. Target values are computed for each term in the raking formula based on the formula specification
#'
#' Currently implemented formats:
#' - proportions: "autumn" style data frame with variable, level, target columns
#'
#' Future formats (not yet implemented):
#' - weighted: target data with weights
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
#' - process_proportions_data: Currently implemented format processor
#' - process_*_data functions: Future format processors (not yet implemented)
#' - compute_target_values: Main function to compute raking targets
#' - expand_joint_distribution: Helper for computing joint distributions

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
#' @keywords internal
process_pop_data <- function(population_data, pop_type, pop_weights = NULL) {
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
    survey_design = process_survey_design_data(population_data),
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
  do.call(rbind, result[!sapply(result, is.null)])
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
  do.call(rbind, result[!sapply(result, is.null)])
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

    if (abs(sum(props) - 1) > 1e-6) {
      stop(
        "Targets for variable '",
        var_name,
        "' do not sum to 1",
        call. = FALSE
      )
    }

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

    # Check if this is a two-way margin (contains ":")
    if (grepl(":", margin)) {
      # Keep as is - already in interaction format
      result[[i]] <- tibble::tibble(
        variable = margin,
        level = margin_data$category,
        target = margin_data$value
      )
    } else {
      # Single variable margin
      result[[i]] <- tibble::tibble(
        variable = margin,
        level = margin_data$category,
        target = margin_data$value
      )
    }

    # Validate targets sum to 1
    if (abs(sum(result[[i]]$target) - 1) > 1e-6) {
      stop("Targets for margin '", margin, "' do not sum to 1", call. = FALSE)
    }
  }

  do.call(rbind, result)
}

#' Process survey design object
#' @description
#' Extracts population information from a survey design object.
#' Uses design formula and weights to compute target values.
#'
#' @section Processing Steps:
#' 1. Extract formula and create model matrix
#' 2. Use design weights to compute proportions
#' 3. Parse variable names and levels from matrix
#' 4. Format in autumn format
#'
#' @keywords internal
process_survey_design_data <- function(design) {
  # Extract formula from design
  formula <- terms(design)

  # Create model frame, converting characters to factors
  mf <- model.frame(formula, model.frame(design)) %>%
    modify_if(is.character, as.factor)

  # Create model matrix with all levels (no reference level)
  mm <- model.matrix(
    formula,
    mf,
    contrasts.arg = lapply(
      mf[sapply(mf, is.factor)],
      contrasts,
      contrasts = FALSE
    )
  )

  # Get design weights
  wts <- weights(design)

  # Compute weighted proportions
  props <- colSums(mm * wts) / sum(wts)

  # Convert to autumn format
  # Skip intercept column
  var_levels <- colnames(mm)[-1]

  # Parse variable names and levels from matrix column names
  parsed <- strsplit(var_levels, ":")
  result <- vector("list", length(parsed))

  for (i in seq_along(parsed)) {
    parts <- parsed[[i]]
    if (length(parts) == 1) {
      # Main effect
      var_name <- sub("^([^.]+).*", "\\1", parts[1]) # Extract variable name
      level <- sub("^[^.]+\\.", "", parts[1]) # Extract level

      result[[i]] <- tibble::tibble(
        variable = var_name,
        level = level,
        target = props[i + 1] # +1 to skip intercept
      )
    } else {
      # Interaction
      var_name <- paste(sub("^([^.]+).*", "\\1", parts), collapse = ":")
      level <- paste(sub("^[^.]+\\.", "", parts), collapse = ":")

      result[[i]] <- tibble::tibble(
        variable = var_name,
        level = level,
        target = props[i + 1] # +1 to skip intercept
      )
    }
  }

  # Combine and validate
  result <- do.call(rbind, result)

  # Validate targets sum to 1 within each variable
  var_sums <- tapply(result$target, result$variable, sum)
  if (any(abs(var_sums - 1) > 1e-6)) {
    stop("Targets do not sum to 1 for some variables", call. = FALSE)
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
  population_data <- process_pop_data(population_data, pop_type, pop_weights)

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

  # Only validate categorical variables (those not identified as continuous)
  categorical_vars <- names(is_continuous_var)[!is_continuous_var]
  if (length(categorical_vars) > 0) {
    cat_data <- population_data[
      population_data$variable %in% categorical_vars,
    ]
    var_sums <- tapply(cat_data$target, cat_data$variable, sum)
    bad_vars <- names(var_sums)[abs(var_sums - 1) > 1e-6]
    if (length(bad_vars) > 0) {
      stop(
        "Targets for variable '",
        bad_vars[1],
        "' do not sum to 1",
        call. = FALSE
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
