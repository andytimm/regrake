# Tolerance sweep across problem sizes
#
# Test how tolerance affects iteration count and accuracy
# across various n (samples) and m (constraints) configurations.

library(autumn)
library(anesrake)
devtools::load_all("..")

set.seed(42)

cat("Tolerance Sweep Across Problem Sizes\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Problem configurations
configs <- list(
  list(n = 500, vars = 2, desc = "small/narrow"),
  list(n = 500, vars = 4, desc = "small/medium"),
  list(n = 1000, vars = 2, desc = "medium/narrow"),
  list(n = 1000, vars = 4, desc = "medium/medium"),
  list(n = 1000, vars = 6, desc = "medium/wide"),
  list(n = 2000, vars = 4, desc = "large/medium"),
  list(n = 5000, vars = 4, desc = "xlarge/medium")
)

tolerances <- c(1e-5, 1e-4, 5e-4)

# Generate data with k categorical variables
make_data <- function(n, k) {
  var_specs <- list(
    list(name = "sex", levels = c("M", "F"), probs = c(0.55, 0.45),
         targets = c(0.49, 0.51)),
    list(name = "age", levels = c("18-29", "30-44", "45-59", "60+"),
         probs = c(0.25, 0.28, 0.27, 0.20),
         targets = c(0.20, 0.25, 0.28, 0.27)),
    list(name = "region", levels = c("NE", "MW", "S", "W"),
         probs = c(0.18, 0.22, 0.38, 0.22),
         targets = c(0.17, 0.21, 0.38, 0.24)),
    list(name = "education", levels = c("HS", "SC", "BA", "Grad"),
         probs = c(0.30, 0.28, 0.25, 0.17),
         targets = c(0.27, 0.29, 0.26, 0.18)),
    list(name = "income", levels = c("Low", "Mid", "High"),
         probs = c(0.35, 0.45, 0.20),
         targets = c(0.30, 0.45, 0.25)),
    list(name = "race", levels = c("W", "B", "H", "A", "O"),
         probs = c(0.60, 0.12, 0.18, 0.06, 0.04),
         targets = c(0.58, 0.13, 0.19, 0.06, 0.04))
  )

  data <- data.frame(id = 1:n)
  targets_list <- list()

  for (i in seq_len(min(k, length(var_specs)))) {
    spec <- var_specs[[i]]
    data[[spec$name]] <- sample(spec$levels, n, replace = TRUE, prob = spec$probs)

    for (j in seq_along(spec$levels)) {
      targets_list[[length(targets_list) + 1]] <- data.frame(
        variable = spec$name,
        level = spec$levels[j],
        target = spec$targets[j]
      )
    }
  }

  data$id <- NULL
  targets <- do.call(rbind, targets_list)

  list(data = data, targets = targets, vars = names(data))
}

compute_margin_errors <- function(data, weights, targets) {
  errors <- numeric(nrow(targets))
  for (i in seq_len(nrow(targets))) {
    var_name <- targets$variable[i]
    lev <- targets$level[i]
    mask <- data[[var_name]] == lev
    errors[i] <- abs(sum(weights[mask]) / sum(weights) - targets$target[i])
  }
  errors
}

# Run experiments
cat(sprintf("%-16s | %-30s | %-30s\n", "Config", "Autumn (IPF)", "Anesrake (IPF)"))
cat(paste(rep("-", 80), collapse = ""), "\n")

for (cfg in configs) {
  prob <- make_data(cfg$n, cfg$vars)
  m <- nrow(prob$targets)

  # Autumn
  autumn_time <- system.time({
    autumn_result <- autumn::harvest(prob$data, prob$targets, verbose = FALSE)
  })[["elapsed"]]
  autumn_errors <- compute_margin_errors(prob$data, autumn_result$weights, prob$targets)

  # Anesrake - convert format
  anesrake_targets <- list()
  for (v in prob$vars) {
    sub <- prob$targets[prob$targets$variable == v, ]
    vec <- setNames(sub$target, sub$level)
    anesrake_targets[[v]] <- vec
  }
  prob$data$caseid <- seq_len(nrow(prob$data))

  anesrake_result <- tryCatch({
    anesrake_time <- system.time({
      res <- suppressWarnings(anesrake::anesrake(
        anesrake_targets,
        prob$data,
        caseid = prob$data$caseid,
        verbose = FALSE,
        type = "nolim"
      ))
    })[["elapsed"]]
    anesrake_errors <- compute_margin_errors(prob$data, res$weightvec, prob$targets)
    list(ok = TRUE, time = anesrake_time, error = max(anesrake_errors))
  }, error = function(e) {
    list(ok = FALSE)
  })

  autumn_str <- sprintf("%.4fs, %.2e (%.3f%%)", autumn_time, max(autumn_errors), max(autumn_errors) * 100)
  anesrake_str <- if (anesrake_result$ok) {
    sprintf("%.4fs, %.2e (%.3f%%)", anesrake_result$time, anesrake_result$error, anesrake_result$error * 100)
  } else {
    "FAILED"
  }

  cat(sprintf("%-16s | %-30s | %-30s\n", cfg$desc, autumn_str, anesrake_str))
}

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("Regrake (ADMM) at various tolerances:\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

for (cfg in configs) {
  cat(sprintf("%s (n=%d, m~%d):\n", cfg$desc, cfg$n, cfg$vars * 3))
  cat(sprintf("  %-10s %8s %10s %12s\n", "Tolerance", "Iters", "Time (s)", "Max Error"))
  cat(paste(rep("-", 50), collapse = ""), "\n")

  prob <- make_data(cfg$n, cfg$vars)
  f <- as.formula(paste("~", paste0("rr_exact(", prob$vars, ")", collapse = " + ")))

  for (tol in tolerances) {
    regrake_result <- tryCatch({
      regrake_time <- system.time({
        result <- regrake(
          data = prob$data,
          formula = f,
          population_data = prob$targets,
          pop_type = "proportions",
          control = list(eps_abs = tol, eps_rel = tol)
        )
      })[["elapsed"]]

      list(ok = TRUE, time = regrake_time, error = max(abs(result$balance$residual)),
           iters = result$diagnostics$iterations)
    }, error = function(e) {
      list(ok = FALSE, msg = substr(conditionMessage(e), 1, 50))
    })

    if (regrake_result$ok) {
      cat(sprintf("  %-10s %8d %10.4f %12.2e\n",
                  format(tol, scientific = TRUE),
                  regrake_result$iters,
                  regrake_result$time,
                  regrake_result$error))
    } else {
      cat(sprintf("  %-10s %s\n", format(tol, scientific = TRUE), regrake_result$msg))
    }
  }
  cat("\n")
}

cat("FINDINGS:\n")
cat("  1. IPF (autumn/anesrake): <0.01s, 0-1% max error for most cases\n")
cat("  2. ADMM at 1e-5: 70-200 iters, 0.01-0.08s, <0.001% max error\n")
cat("  3. ADMM at 1e-4: 33-50 iters, 0.01-0.05s, <0.01% max error (best tradeoff)\n")
cat("  4. ADMM at 5e-4: 15-20 iters but sometimes exits too early (zero weights)\n")
cat("\n")
cat("RECOMMENDATION: Default eps_abs/eps_rel = 1e-4 would be a good 'pollster-friendly' default\n")
cat("This gives ~3x speedup vs 1e-5 while maintaining excellent accuracy.\n")
