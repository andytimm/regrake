# Compare regrake vs autumn vs anesrake on simulated raking problems
#
# Tests:
#   1. Accuracy: do all three hit the same targets with exact constraints?
#   2. Speed: unbounded and bounded raking across problem sizes
#   3. Multi-dimensional: sex, age, region, education
#
# Usage:
#   cd benchmarks
#   Rscript -e "setwd('c:/Users/timma/Documents/R packages/regrake/benchmarks'); devtools::load_all('..', quiet=TRUE); source('vs_other_packages.R'); main()"

library(autumn)
library(anesrake)

# ── Data generation ──────────────────────────────────────────────────────

make_survey_data <- function(n, seed = 42) {
  set.seed(seed)
  data.frame(
    caseid = seq_len(n),
    sex = sample(
      c("M", "F"), n,
      replace = TRUE, prob = c(0.55, 0.45)
    ),
    age = sample(
      c("18-34", "35-54", "55+"), n,
      replace = TRUE, prob = c(0.45, 0.30, 0.25)
    ),
    region = sample(
      c("NE", "MW", "S", "W"), n,
      replace = TRUE, prob = c(0.20, 0.22, 0.38, 0.20)
    ),
    education = sample(
      c("HS", "Some college", "BA+"), n,
      replace = TRUE, prob = c(0.35, 0.30, 0.35)
    )
  )
}

# Population targets (same format works for regrake proportions)
pop_targets <- data.frame(
  variable = c(
    rep("sex", 2),
    rep("age", 3),
    rep("region", 4),
    rep("education", 3)
  ),
  level = c(
    "M", "F",
    "18-34", "35-54", "55+",
    "NE", "MW", "S", "W",
    "HS", "Some college", "BA+"
  ),
  target = c(
    0.49, 0.51,
    0.30, 0.35, 0.35,
    0.18, 0.21, 0.38, 0.23,
    0.28, 0.29, 0.43
  )
)

# Convert to anesrake format
make_anesrake_targets <- function(pop_targets) {
  vars <- unique(pop_targets$variable)
  tgt <- list()
  for (v in vars) {
    rows <- pop_targets[pop_targets$variable == v, ]
    vals <- rows$target
    names(vals) <- rows$level
    tgt[[v]] <- vals
  }
  tgt
}

# ── Wrappers ─────────────────────────────────────────────────────────────

run_regrake <- function(data, vars, pop_targets, bounds = NULL, bounds_method = "soft",
                        exact_tol = NULL) {
  # Build formula from variable names
  terms <- paste0("rr_exact(", vars, ")")
  f <- as.formula(paste("~", paste(terms, collapse = " + ")))

  # Filter targets to requested variables
  pt <- pop_targets[pop_targets$variable %in% vars, ]

  args <- list(
    data = data,
    formula = f,
    population_data = pt,
    pop_type = "proportions"
  )
  if (!is.null(bounds)) {
    args$bounds <- bounds
    args$bounds_method <- bounds_method
  }
  if (!is.null(exact_tol)) {
    args$exact_tol <- exact_tol
  }

  result <- suppressMessages(do.call(regrake, args))
  result$weights
}

run_autumn <- function(data, vars, pop_targets, max_weight = Inf) {
  pt <- pop_targets[pop_targets$variable %in% vars, ]
  # autumn uses 'target' column name — same as our format
  r <- autumn::harvest(
    data[, vars, drop = FALSE],
    pt,
    max_weight = max_weight,
    verbose = FALSE
  )
  r$weights
}

run_anesrake <- function(data, vars, pop_targets, cap = 50) {
  tgt <- make_anesrake_targets(pop_targets[pop_targets$variable %in% vars, ])

  # anesrake needs factors
  d <- data
  for (v in vars) d[[v]] <- factor(d[[v]])

  r <- anesrake::anesrake(
    tgt, d,
    caseid = d$caseid,
    verbose = FALSE,
    cap = cap
  )
  r$weightvec
}

# ── Accuracy comparison ─────────────────────────────────────────────────

check_accuracy <- function(data, vars, pop_targets) {
  cat(sprintf("\nAccuracy: %d vars (%s), n=%d\n",
              length(vars), paste(vars, collapse = ", "), nrow(data)))
  cat(paste(rep("-", 60), collapse = ""), "\n")

  w_regrake <- run_regrake(data, vars, pop_targets)
  w_autumn <- run_autumn(data, vars, pop_targets)
  w_anesrake <- run_anesrake(data, vars, pop_targets)

  pt <- pop_targets[pop_targets$variable %in% vars, ]

  cat(sprintf("  %-12s %10s %10s %10s %10s\n",
              "level", "target", "regrake", "autumn", "anesrake"))

  for (v in unique(pt$variable)) {
    for (lev in pt$level[pt$variable == v]) {
      mask <- data[[v]] == lev
      tgt <- pt$target[pt$variable == v & pt$level == lev]
      a_regrake <- sum(w_regrake[mask]) / sum(w_regrake)
      a_autumn <- sum(w_autumn[mask]) / sum(w_autumn)
      a_anesrake <- sum(w_anesrake[mask]) / sum(w_anesrake)
      cat(sprintf("  %-12s %10.6f %10.6f %10.6f %10.6f\n",
                  lev, tgt, a_regrake, a_autumn, a_anesrake))
    }
  }

  # Max absolute difference from targets
  max_diff <- function(w) {
    diffs <- numeric(0)
    for (v in unique(pt$variable)) {
      for (lev in pt$level[pt$variable == v]) {
        mask <- data[[v]] == lev
        tgt <- pt$target[pt$variable == v & pt$level == lev]
        achieved <- sum(w[mask]) / sum(w)
        diffs <- c(diffs, abs(achieved - tgt))
      }
    }
    max(diffs)
  }

  cat(sprintf("\n  Max diff from targets: regrake=%.2e, autumn=%.2e, anesrake=%.2e\n",
              max_diff(w_regrake), max_diff(w_autumn), max_diff(w_anesrake)))

  # Weight distribution comparison
  cat(sprintf("  Weight range: regrake=[%.3f,%.3f], autumn=[%.3f,%.3f], anesrake=[%.3f,%.3f]\n",
              min(w_regrake), max(w_regrake),
              min(w_autumn), max(w_autumn),
              min(w_anesrake), max(w_anesrake)))

  # Kish deff
  kish <- function(w) 1 + var(w) / mean(w)^2
  cat(sprintf("  Kish deff: regrake=%.3f, autumn=%.3f, anesrake=%.3f\n",
              kish(w_regrake), kish(w_autumn), kish(w_anesrake)))
}

# ── Speed comparison ─────────────────────────────────────────────────────

time_it <- function(expr, reps = 3) {
  pf <- parent.frame()
  times <- numeric(reps)
  for (i in seq_len(reps)) {
    times[i] <- system.time(eval(expr, envir = pf))[["elapsed"]]
  }
  median(times)
}

speed_comparison <- function(data, vars, pop_targets, label, reps = 3) {
  cat(sprintf("\n%s: %d vars, n=%d\n", label, length(vars), nrow(data)))
  cat(paste(rep("-", 60), collapse = ""), "\n")

  # Unbounded
  t_regrake <- time_it(quote(run_regrake(data, vars, pop_targets)), reps)
  t_autumn <- time_it(quote(run_autumn(data, vars, pop_targets)), reps)
  t_anesrake <- time_it(quote(run_anesrake(data, vars, pop_targets)), reps)

  cat(sprintf("  Unbounded:  regrake=%.3fs  autumn=%.3fs  anesrake=%.3fs\n",
              t_regrake, t_autumn, t_anesrake))
  cat(sprintf("              regrake/autumn=%.1fx  regrake/anesrake=%.1fx\n",
              t_regrake / max(t_autumn, 0.001), t_regrake / max(t_anesrake, 0.001)))

  # Bounded (cap at 3x)
  t_regrake_soft <- time_it(quote(
    run_regrake(data, vars, pop_targets, bounds = c(0.3, 3), bounds_method = "soft")
  ), reps)
  t_regrake_hard <- time_it(quote(
    run_regrake(data, vars, pop_targets, bounds = c(0.3, 3), bounds_method = "hard")
  ), reps)
  t_autumn_b <- time_it(quote(run_autumn(data, vars, pop_targets, max_weight = 3)), reps)
  t_anesrake_b <- time_it(quote(run_anesrake(data, vars, pop_targets, cap = 3)), reps)

  cat(sprintf("  Bounded(3): regrake_soft=%.3fs  regrake_hard=%.3fs  autumn=%.3fs  anesrake=%.3fs\n",
              t_regrake_soft, t_regrake_hard, t_autumn_b, t_anesrake_b))

  list(
    label = label,
    n = nrow(data), nvars = length(vars),
    unbounded = c(regrake = t_regrake, autumn = t_autumn, anesrake = t_anesrake),
    bounded = c(regrake_soft = t_regrake_soft, regrake_hard = t_regrake_hard,
                autumn = t_autumn_b, anesrake = t_anesrake_b)
  )
}

# ── Main ─────────────────────────────────────────────────────────────────

main <- function() {
  cat("regrake vs autumn vs anesrake\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")

  # ── 1. Accuracy checks ──

  data_1k <- make_survey_data(1000)

  check_accuracy(data_1k, c("sex"), pop_targets)
  check_accuracy(data_1k, c("sex", "age"), pop_targets)
  check_accuracy(data_1k, c("sex", "age", "region"), pop_targets)
  check_accuracy(data_1k, c("sex", "age", "region", "education"), pop_targets)

  # ── 2. Speed comparisons ──

  cat("\n\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("Speed Comparison\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")

  results <- list()

  # Small problem: 2 vars, varying n
  for (n in c(500, 1000, 5000, 10000)) {
    d <- make_survey_data(n)
    r <- speed_comparison(d, c("sex", "age"), pop_targets,
                          sprintf("2 vars, n=%d", n))
    results[[length(results) + 1]] <- r
  }

  # Fixed n=1000, varying number of variables
  for (nvars in 2:4) {
    vars <- c("sex", "age", "region", "education")[1:nvars]
    r <- speed_comparison(data_1k, vars, pop_targets,
                          sprintf("%d vars, n=1000", nvars))
    results[[length(results) + 1]] <- r
  }

  # Larger problem
  data_10k <- make_survey_data(10000)
  r <- speed_comparison(data_10k, c("sex", "age", "region", "education"),
                        pop_targets, "4 vars, n=10000")
  results[[length(results) + 1]] <- r

  # ── 3. Effect of exact_tol on speed ──
  #
  # autumn/anesrake default to ~1% tolerance (convergence = c(pct = 0.01) for autumn,

  # convcrit = 0.01 for anesrake). regrake with exact constraints uses ADMM tolerance
  # (eps_abs/eps_rel = 1e-6 by default). Test if using exact_tol = 0.01 speeds things up.

  cat("\n\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("Tolerance Comparison: regrake exact vs exact_tol=0.01\n")
  cat("(autumn/anesrake default to ~1% tolerance)\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")

  vars4 <- c("sex", "age", "region", "education")
  reps <- 3
  tol_results <- list()

  for (n in c(500, 1000, 5000, 10000)) {
    test_data <- make_survey_data(n)

    t_exact <- time_it(quote(
      run_regrake(test_data, vars4, pop_targets)
    ), reps)

    t_tol01 <- time_it(quote(
      run_regrake(test_data, vars4, pop_targets, exact_tol = 0.01)
    ), reps)

    t_autumn <- time_it(quote(
      run_autumn(test_data, vars4, pop_targets)
    ), reps)

    t_anesrake <- time_it(quote(
      run_anesrake(test_data, vars4, pop_targets)
    ), reps)

    cat(sprintf("\n  n=%d: regrake_exact=%.3fs  regrake_tol01=%.3fs  autumn=%.3fs  anesrake=%.3fs\n",
                n, t_exact, t_tol01, t_autumn, t_anesrake))
    cat(sprintf("         speedup from tol: %.1fx  vs autumn: %.1fx  vs anesrake: %.1fx\n",
                t_exact / max(t_tol01, 0.001),
                t_tol01 / max(t_autumn, 0.001),
                t_tol01 / max(t_anesrake, 0.001)))

    tol_results[[as.character(n)]] <- list(
      n = n,
      regrake_exact = t_exact, regrake_tol01 = t_tol01,
      autumn = t_autumn, anesrake = t_anesrake
    )
  }

  # ── 4. Bounded raking across cap values (realistic range: 3-10) ──

  cat("\n\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("Bounded Speed: Varying Cap (4 vars, n=1000)\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")

  caps <- c(3, 5, 10)
  cap_results <- list()

  cat(sprintf("\n  %-6s %8s %8s %8s %8s\n", "cap", "rg_soft", "rg_hard", "autumn", "anesrake"))
  cat("  ", paste(rep("-", 46), collapse = ""), "\n")

  for (cap in caps) {
    lower <- 1 / cap

    t_soft <- suppressWarnings(time_it(quote(
      run_regrake(data_1k, vars4, pop_targets,
                  bounds = c(lower, cap), bounds_method = "soft")
    ), reps))

    t_hard <- suppressWarnings(time_it(quote(
      run_regrake(data_1k, vars4, pop_targets,
                  bounds = c(lower, cap), bounds_method = "hard")
    ), reps))

    t_autumn <- time_it(quote(
      run_autumn(data_1k, vars4, pop_targets, max_weight = cap)
    ), reps)

    t_anesrake <- time_it(quote(
      run_anesrake(data_1k, vars4, pop_targets, cap = cap)
    ), reps)

    cat(sprintf("  %-6d %7.3fs %7.3fs %7.3fs %7.3fs\n",
                cap, t_soft, t_hard, t_autumn, t_anesrake))

    cap_results[[as.character(cap)]] <- list(
      cap = cap,
      regrake_soft = t_soft, regrake_hard = t_hard,
      autumn = t_autumn, anesrake = t_anesrake
    )
  }

  # ── 4. Summary table ──

  cat("\n\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("Summary Table\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")

  cat(sprintf("\n%-20s %8s %8s %8s %8s\n",
              "Scenario", "regrake", "autumn", "anesrake", "R/autumn"))
  cat(paste(rep("-", 60), collapse = ""), "\n")

  cat("Unbounded:\n")
  for (r in results) {
    cat(sprintf("  %-18s %7.3fs %7.3fs %7.3fs %7.1fx\n",
                r$label,
                r$unbounded["regrake"],
                r$unbounded["autumn"],
                r$unbounded["anesrake"],
                r$unbounded["regrake"] / max(r$unbounded["autumn"], 0.001)))
  }

  cat("\nBounded (cap=3):\n")
  cat(sprintf("  %-18s %8s %8s %8s %8s\n",
              "", "rg_soft", "rg_hard", "autumn", "anesrake"))
  for (r in results) {
    cat(sprintf("  %-18s %7.3fs %7.3fs %7.3fs %7.3fs\n",
                r$label,
                r$bounded["regrake_soft"],
                r$bounded["regrake_hard"],
                r$bounded["autumn"],
                r$bounded["anesrake"]))
  }

  cat("\nTolerance/cap comparisons - see sections 3 and 4 above\n")

  invisible(results)
}

if (!interactive()) {
  main()
}
