# Validate margin_tol scaling across a wide n × m grid
#
# Goal: test whether margin_tol = X gives consistent ~X% max margin error
# across different sample sizes and constraint counts.
#
# If the scaling eps = margin_tol / sqrt(m + 2n) is correct,
# the achieved max error should be roughly constant across the grid.

devtools::load_all("..")

set.seed(42)

cat("margin_tol Validation Grid\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Problem generator: precise control over m via different variable structures
make_problem <- function(n, target_m) {
  # Build up variables until we reach approximately target_m constraints
  var_specs <- list(
    list(name = "v1", levels = c("A", "B"), probs = c(0.55, 0.45), targets = c(0.50, 0.50)),
    list(name = "v2", levels = c("A", "B", "C"), probs = c(0.4, 0.35, 0.25), targets = c(0.33, 0.34, 0.33)),
    list(name = "v3", levels = c("A", "B", "C", "D"), probs = c(0.3, 0.3, 0.25, 0.15), targets = c(0.25, 0.25, 0.25, 0.25)),
    list(name = "v4", levels = c("A", "B", "C"), probs = c(0.5, 0.3, 0.2), targets = c(0.40, 0.35, 0.25)),
    list(name = "v5", levels = c("A", "B", "C", "D"), probs = c(0.4, 0.3, 0.2, 0.1), targets = c(0.30, 0.30, 0.25, 0.15)),
    list(name = "v6", levels = c("A", "B"), probs = c(0.6, 0.4), targets = c(0.55, 0.45)),
    list(name = "v7", levels = c("A", "B", "C"), probs = c(0.45, 0.35, 0.20), targets = c(0.40, 0.35, 0.25)),
    list(name = "v8", levels = c("A", "B", "C", "D", "E"), probs = c(0.3, 0.25, 0.2, 0.15, 0.1), targets = c(0.25, 0.25, 0.20, 0.18, 0.12)),
    list(name = "v9", levels = c("A", "B"), probs = c(0.52, 0.48), targets = c(0.48, 0.52)),
    list(name = "v10", levels = c("A", "B", "C"), probs = c(0.35, 0.40, 0.25), targets = c(0.30, 0.40, 0.30))
  )

  data <- data.frame(id = 1:n)
  targets_list <- list()
  vars_used <- character(0)
  m_so_far <- 0

  for (i in seq_along(var_specs)) {
    spec <- var_specs[[i]]
    m_this <- length(spec$levels)
    if (m_so_far + m_this > target_m + 2) break  # allow small overshoot

    data[[spec$name]] <- sample(spec$levels, n, replace = TRUE, prob = spec$probs)
    vars_used <- c(vars_used, spec$name)

    for (j in seq_along(spec$levels)) {
      targets_list[[length(targets_list) + 1]] <- data.frame(
        variable = spec$name,
        level = spec$levels[j],
        target = spec$targets[j]
      )
    }
    m_so_far <- m_so_far + m_this
  }

  data$id <- NULL
  targets <- do.call(rbind, targets_list)

  list(data = data, targets = targets, vars = vars_used, m = nrow(targets))
}

# Test grid
n_values <- c(200, 500, 1000, 2000, 5000, 10000)
m_targets <- c(5, 12, 20, 30)
margin_tol_value <- 1e-4

cat(sprintf("Testing with margin_tol = %.0e\n\n", margin_tol_value))

cat(sprintf("%-7s %-4s %-8s %-8s %-8s %-12s %-12s\n",
            "n", "m", "p", "sqrt(p)", "iters", "max_error", "error/tol"))
cat(paste(rep("-", 75), collapse = ""), "\n")

all_results <- data.frame()

for (n in n_values) {
  for (mt in m_targets) {
    prob <- make_problem(n, mt)
    m <- prob$m
    p <- m + 2 * n
    sqrt_p <- sqrt(p)

    f <- as.formula(paste("~", paste0("rr_exact(", prob$vars, ")", collapse = " + ")))

    result <- tryCatch({
      res <- regrake(
        data = prob$data,
        formula = f,
        population_data = prob$targets,
        pop_type = "proportions",
        control = list(margin_tol = margin_tol_value)
      )
      list(ok = TRUE, iters = res$diagnostics$iterations,
           error = max(abs(res$balance$residual)))
    }, error = function(e) {
      list(ok = FALSE, msg = substr(conditionMessage(e), 1, 30))
    })

    if (result$ok) {
      ratio <- result$error / margin_tol_value
      cat(sprintf("%-7d %-4d %-8d %-8.1f %-8d %-12.2e %-12.3f\n",
                  n, m, p, sqrt_p, result$iters, result$error, ratio))

      all_results <- rbind(all_results, data.frame(
        n = n, m = m, p = p, sqrt_p = sqrt_p,
        iters = result$iters, error = result$error, ratio = ratio
      ))
    } else {
      cat(sprintf("%-7d %-4d %-8d %-8.1f ERROR\n", n, m, p, sqrt_p))
    }
  }
  cat("\n")
}

cat(paste(rep("=", 75), collapse = ""), "\n")
cat("SUMMARY\n\n")

cat(sprintf("margin_tol = %.0e\n", margin_tol_value))
cat(sprintf("Achieved max error range: [%.2e, %.2e]\n",
            min(all_results$error), max(all_results$error)))
cat(sprintf("Error/tolerance ratio range: [%.3f, %.3f]\n",
            min(all_results$ratio), max(all_results$ratio)))
cat(sprintf("Mean error/tolerance ratio: %.3f\n", mean(all_results$ratio)))
cat(sprintf("Iteration range: [%d, %d]\n",
            min(all_results$iters), max(all_results$iters)))

cat("\n")
if (max(all_results$ratio) < 1.0) {
  cat("PASS: All achieved errors are below margin_tol across the grid.\n")
  cat("The scaling formula eps = margin_tol / sqrt(m + 2n) works well.\n")
} else {
  cat("MIXED: Some cases exceed margin_tol.\n")
  cat("The scaling formula may need refinement.\n")
  over <- all_results[all_results$ratio > 1.0, ]
  cat(sprintf("  %d / %d cases exceeded tolerance\n", nrow(over), nrow(all_results)))
}
