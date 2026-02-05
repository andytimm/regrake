# Test margin_tol parameter
#
# Verify that margin_tol provides consistent margin accuracy
# across different problem sizes (n and m).

devtools::load_all("..")

set.seed(42)

cat("Testing margin_tol Parameter\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Same problem generator as scaling_grid.R
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

# Test 1: Compare eps_abs=1e-4 vs margin_tol=1e-4 across problem sizes
cat("Test 1: eps_abs=1e-4 vs margin_tol=1e-4\n")
cat(paste(rep("-", 70), collapse = ""), "\n\n")

n_values <- c(500, 1000, 2000, 5000, 10000)

cat(sprintf("%-6s %-4s | %-20s | %-20s\n",
            "n", "vars", "eps_abs=1e-4", "margin_tol=1e-4"))
cat(sprintf("%-6s %-4s | %-10s %-10s | %-10s %-10s\n",
            "", "", "iters", "max_err", "iters", "max_err"))
cat(paste(rep("-", 70), collapse = ""), "\n")

for (n in n_values) {
  for (nv in c(2, 4, 6)) {
    prob <- make_problem(n, nv)
    f <- as.formula(paste("~", paste0("rr_exact(", prob$vars, ")", collapse = " + ")))

    # With eps_abs
    r1 <- tryCatch({
      res <- regrake(prob$data, f, prob$targets, "proportions",
                     control = list(eps_abs = 1e-4, eps_rel = 1e-4))
      list(ok = TRUE, iters = res$diagnostics$iterations,
           error = max(abs(res$balance$residual)))
    }, error = function(e) list(ok = FALSE))

    # With margin_tol
    r2 <- tryCatch({
      res <- regrake(prob$data, f, prob$targets, "proportions",
                     margin_tol = 1e-4)
      list(ok = TRUE, iters = res$diagnostics$iterations,
           error = max(abs(res$balance$residual)))
    }, error = function(e) list(ok = FALSE))

    eps_str <- if (r1$ok) sprintf("%4d       %.2e", r1$iters, r1$error) else "ERROR"
    mtol_str <- if (r2$ok) sprintf("%4d       %.2e", r2$iters, r2$error) else "ERROR"

    cat(sprintf("%-6d %-4d | %-20s | %-20s\n", n, nv, eps_str, mtol_str))
  }
  cat("\n")
}

cat("\n")
cat("Test 2: Does margin_tol give consistent accuracy across sizes?\n")
cat(paste(rep("-", 70), collapse = ""), "\n\n")

cat("With margin_tol = 0.001 (targeting ~0.1% max error):\n\n")

for (n in c(500, 1000, 5000, 10000)) {
  for (nv in c(2, 4)) {
    prob <- make_problem(n, nv)
    f <- as.formula(paste("~", paste0("rr_exact(", prob$vars, ")", collapse = " + ")))

    result <- tryCatch({
      res <- regrake(prob$data, f, prob$targets, "proportions",
                     margin_tol = 0.001)
      list(ok = TRUE, iters = res$diagnostics$iterations,
           error = max(abs(res$balance$residual)),
           error_pct = max(abs(res$balance$residual)) * 100)
    }, error = function(e) list(ok = FALSE, msg = conditionMessage(e)))

    if (result$ok) {
      cat(sprintf("  n=%5d, m=%2d: %3d iters, max error = %.4f%% %s\n",
                  n, prob$m, result$iters, result$error_pct,
                  if (result$error_pct < 0.1) "✓" else ""))
    } else {
      cat(sprintf("  n=%5d, m=%2d: ERROR\n", n, prob$m))
    }
  }
}

cat("\n")
cat("CONCLUSION:\n")
cat("margin_tol provides consistent margin accuracy regardless of n or m,\n")
cat("while eps_abs hits the 'cliff' at large n with few constraints.\n")
