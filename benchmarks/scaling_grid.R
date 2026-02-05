# Scaling Grid Experiment
#
# Investigate how ADMM tolerance scales with both n (samples) and m (constraints)
# to refine our understanding of the relationship:
#   effective_tolerance ~ f(n, m) * eps_abs
#
# The ADMM formula uses p = m + 2n, so we expect sqrt(p) ≈ sqrt(2n) for small m.
# But does m matter more as it grows?

devtools::load_all("..")

set.seed(42)

cat("Scaling Grid: n × m\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Generate data with k categorical variables (each ~3-4 levels)
make_problem <- function(n, num_vars) {
  var_specs <- list(
    list(name = "v1", levels = c("A", "B"), probs = c(0.55, 0.45), targets = c(0.50, 0.50)),
    list(name = "v2", levels = c("A", "B", "C"), probs = c(0.4, 0.35, 0.25), targets = c(0.33, 0.34, 0.33)),
    list(name = "v3", levels = c("A", "B", "C", "D"), probs = c(0.3, 0.3, 0.25, 0.15), targets = c(0.25, 0.25, 0.25, 0.25)),
    list(name = "v4", levels = c("A", "B", "C"), probs = c(0.5, 0.3, 0.2), targets = c(0.40, 0.35, 0.25)),
    list(name = "v5", levels = c("A", "B", "C", "D"), probs = c(0.4, 0.3, 0.2, 0.1), targets = c(0.30, 0.30, 0.25, 0.15)),
    list(name = "v6", levels = c("A", "B"), probs = c(0.6, 0.4), targets = c(0.55, 0.45))
  )

  data <- data.frame(id = 1:n)
  targets_list <- list()
  vars_used <- character(0)

  for (i in seq_len(min(num_vars, length(var_specs)))) {
    spec <- var_specs[[i]]
    data[[spec$name]] <- sample(spec$levels, n, replace = TRUE, prob = spec$probs)
    vars_used <- c(vars_used, spec$name)

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
  m <- nrow(targets)

  list(data = data, targets = targets, vars = vars_used, m = m)
}

# Grid: n × num_vars
n_values <- c(500, 1000, 2000, 5000, 10000)
var_counts <- c(2, 4, 6)  # roughly m = 5, 12, 19

# Fixed tolerance for comparison
eps <- 1e-4

cat("Testing with eps_abs = eps_rel = 1e-4\n")
cat("p = m + 2n (total ADMM residual dimension)\n\n")

cat(sprintf("%-6s %-4s %-6s %-8s %-8s %-10s %-12s\n",
            "n", "vars", "m", "p", "sqrt(p)", "iters", "max_error"))
cat(paste(rep("-", 70), collapse = ""), "\n")

results <- list()

for (n in n_values) {
  for (nv in var_counts) {
    prob <- make_problem(n, nv)
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
        control = list(eps_abs = eps, eps_rel = eps)
      )
      list(ok = TRUE, iters = res$diagnostics$iterations,
           error = max(abs(res$balance$residual)))
    }, error = function(e) {
      list(ok = FALSE, msg = substr(conditionMessage(e), 1, 30))
    })

    if (result$ok) {
      cat(sprintf("%-6d %-4d %-6d %-8d %-8.1f %-10d %-12.2e\n",
                  n, nv, m, p, sqrt_p, result$iters, result$error))
      results[[length(results) + 1]] <- list(
        n = n, nv = nv, m = m, p = p, sqrt_p = sqrt_p,
        iters = result$iters, error = result$error, ok = TRUE
      )
    } else {
      cat(sprintf("%-6d %-4d %-6d %-8d %-8.1f %-10s\n",
                  n, nv, m, p, sqrt_p, "ERROR"))
      results[[length(results) + 1]] <- list(
        n = n, nv = nv, m = m, p = p, sqrt_p = sqrt_p,
        ok = FALSE
      )
    }
  }
  cat("\n")
}

# Analysis
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("ANALYSIS\n\n")

# Group by n to see effect of m at fixed n
cat("Effect of m at fixed n:\n")
cat("(If m doesn't matter much, iterations should be similar within each n)\n\n")

for (n in n_values) {
  subset <- Filter(function(r) r$n == n && r$ok, results)
  if (length(subset) > 0) {
    iters <- sapply(subset, function(r) r$iters)
    ms <- sapply(subset, function(r) r$m)
    cat(sprintf("  n=%5d: m=%s -> iters=%s (range: %d)\n",
                n,
                paste(ms, collapse = "/"),
                paste(iters, collapse = "/"),
                max(iters) - min(iters)))
  }
}

cat("\n")
cat("Group by sqrt(p) to see if it's the key predictor:\n\n")

# Group results by sqrt(p) ranges
ranges <- list(
  c(30, 35),
  c(43, 47),
  c(62, 66),
  c(98, 102),
  c(140, 145)
)

for (rng in ranges) {
  subset <- Filter(function(r) r$ok && r$sqrt_p >= rng[1] && r$sqrt_p <= rng[2], results)
  if (length(subset) > 0) {
    cat(sprintf("  sqrt(p) in [%d-%d]:\n", rng[1], rng[2]))
    for (r in subset) {
      cat(sprintf("    n=%5d, m=%2d: %3d iters, error=%.2e\n",
                  r$n, r$m, r$iters, r$error))
    }
  }
}

cat("\n")
cat("CONCLUSION:\n")
cat("If iters scale with sqrt(p), the proposed margin_tol formula should be:\n")
cat("  eps_abs = margin_tol / sqrt(p)  where p = m + 2n\n")
cat("\n")
cat("This accounts for both sample size AND constraint count.\n")
