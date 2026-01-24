"""
Generate test cases and run both Python implementations (rswjax and original rsw)
to create reference results for R comparison.

Usage:
    python generate_test_cases.py
"""

import os
import json
import time
import numpy as np
import pandas as pd
from pathlib import Path

# Try to import both implementations
try:
    import rswjax
    from rswjax import EqualityLoss, LeastSquaresLoss, KLLoss
    from rswjax import ZeroRegularizer, EntropyRegularizer, KLRegularizer
    from rswjax import rsw
    HAS_RSWJAX = True
except ImportError:
    print("Warning: rswjax not installed. Install with: pip install -e ../ai_context/rswjax")
    HAS_RSWJAX = False

try:
    # Add rsw_original to path
    import sys
    sys.path.insert(0, str(Path(__file__).parent.parent / "ai_context" / "rsw_original"))
    from rsw import EqualityLoss as OrigEqualityLoss
    from rsw import LeastSquaresLoss as OrigLeastSquaresLoss
    from rsw import ZeroRegularizer as OrigZeroRegularizer
    from rsw import EntropyRegularizer as OrigEntropyRegularizer
    from rsw.solver import admm as orig_admm
    HAS_ORIGINAL_RSW = True
except ImportError as e:
    print(f"Note: Original rsw package not available ({e}). Only comparing against rswjax.")
    HAS_ORIGINAL_RSW = False

OUTPUT_DIR = Path("test_cases")
OUTPUT_DIR.mkdir(exist_ok=True)


def save_test_case(name, F, losses_spec, regularizer_spec, lam, results):
    """Save test case inputs and results to files."""
    case_dir = OUTPUT_DIR / name
    case_dir.mkdir(exist_ok=True)

    # Save design matrix
    np.savetxt(case_dir / "F.csv", F, delimiter=",")

    # Save losses specification as JSON
    with open(case_dir / "losses.json", "w") as f:
        json.dump(losses_spec, f, indent=2)

    # Save regularizer specification
    with open(case_dir / "regularizer.json", "w") as f:
        json.dump(regularizer_spec, f, indent=2)

    # Save lambda
    with open(case_dir / "lambda.txt", "w") as f:
        f.write(str(lam))

    # Save results
    for impl_name, result in results.items():
        if result is not None:
            np.savetxt(case_dir / f"weights_{impl_name}.csv", result["weights"], delimiter=",")
            np.savetxt(case_dir / f"achieved_{impl_name}.csv", result["achieved"], delimiter=",")

    # Save timing information
    timing = {}
    for impl_name, result in results.items():
        if result is not None and "time" in result:
            timing[f"{impl_name}_time"] = result["time"]
    if timing:
        with open(case_dir / "timing.json", "w") as f:
            json.dump(timing, f, indent=2)


def run_rswjax(F, losses, regularizer, lam):
    """Run rswjax solver and return results with timing."""
    if not HAS_RSWJAX:
        return None

    # Create a dummy dataframe (rswjax expects this interface)
    df = pd.DataFrame(F.T)

    # Run solver with timing
    start = time.perf_counter()
    w, achieved, sol = rsw(df, funs=None, losses=losses, regularizer=regularizer,
                           lam=lam, verbose=False, maxiter=5000, eps_abs=1e-6, eps_rel=1e-6)
    elapsed = time.perf_counter() - start

    return {
        "weights": np.array(w),
        "achieved": np.concatenate(achieved),
        "time": elapsed
    }


def run_original_rsw(F, losses, regularizer, lam):
    """Run original rsw solver and return results with timing."""
    if not HAS_ORIGINAL_RSW:
        return None

    from scipy import sparse

    # Convert to sparse matrix (original rsw expects this)
    F_sparse = sparse.csr_matrix(F)

    # Run solver with timing
    start = time.perf_counter()
    sol = orig_admm(F_sparse, losses, regularizer, lam,
                    verbose=False, maxiter=5000, eps_abs=1e-6, eps_rel=1e-6)
    elapsed = time.perf_counter() - start

    return {
        "weights": np.array(sol["w_best"]),
        "achieved": F @ sol["w_best"],
        "time": elapsed
    }


def generate_basic_equality():
    """Test case 01: Basic equality constraints on categorical variable."""
    np.random.seed(42)
    n = 1000

    # Two-category variable with unequal sample proportions
    # Sample: 60% cat A, 40% cat B
    # Target: 50% each
    categories = np.random.choice([0, 1], size=n, p=[0.6, 0.4])

    # Design matrix: indicator for each category
    F = np.zeros((2, n))
    F[0, categories == 0] = 1
    F[1, categories == 1] = 1

    # Target proportions
    targets = np.array([0.5, 0.5])

    losses_spec = [{"type": "equality", "target": targets.tolist()}]
    regularizer_spec = {"type": "entropy", "limit": None}
    lam = 1.0

    results = {}
    if HAS_RSWJAX:
        losses = [EqualityLoss(targets)]
        regularizer = EntropyRegularizer()
        results["rswjax"] = run_rswjax(F, losses, regularizer, lam)

    if HAS_ORIGINAL_RSW:
        orig_losses = [OrigEqualityLoss(targets)]
        orig_regularizer = OrigEntropyRegularizer()
        results["rsw_original"] = run_original_rsw(F, orig_losses, orig_regularizer, lam)

    save_test_case("01_basic_equality", F, losses_spec, regularizer_spec, lam, results)
    print("Generated: 01_basic_equality")


def generate_least_squares():
    """Test case 02: Least squares loss."""
    np.random.seed(43)
    n = 1000

    # Three categories
    categories = np.random.choice([0, 1, 2], size=n, p=[0.5, 0.3, 0.2])

    F = np.zeros((3, n))
    for i in range(3):
        F[i, categories == i] = 1

    # Target proportions (different from sample)
    targets = np.array([0.33, 0.33, 0.34])

    losses_spec = [{"type": "least_squares", "target": targets.tolist()}]
    regularizer_spec = {"type": "entropy", "limit": None}
    lam = 1.0

    results = {}
    if HAS_RSWJAX:
        losses = [LeastSquaresLoss(targets)]
        regularizer = EntropyRegularizer()
        results["rswjax"] = run_rswjax(F, losses, regularizer, lam)

    if HAS_ORIGINAL_RSW:
        orig_losses = [OrigLeastSquaresLoss(targets)]
        orig_regularizer = OrigEntropyRegularizer()
        results["rsw_original"] = run_original_rsw(F, orig_losses, orig_regularizer, lam)

    save_test_case("02_least_squares", F, losses_spec, regularizer_spec, lam, results)
    print("Generated: 02_least_squares")


def generate_mixed_losses():
    """Test case 03: Mixed equality and least squares."""
    np.random.seed(44)
    n = 1000

    # Two variables: sex (2 levels), age (3 levels)
    sex = np.random.choice([0, 1], size=n, p=[0.6, 0.4])
    age = np.random.choice([0, 1, 2], size=n, p=[0.5, 0.3, 0.2])

    # Design matrix: sex indicators + age indicators
    F = np.zeros((5, n))
    F[0, sex == 0] = 1
    F[1, sex == 1] = 1
    F[2, age == 0] = 1
    F[3, age == 1] = 1
    F[4, age == 2] = 1

    # Targets
    sex_targets = np.array([0.5, 0.5])
    age_targets = np.array([0.3, 0.4, 0.3])

    losses_spec = [
        {"type": "equality", "target": sex_targets.tolist()},
        {"type": "least_squares", "target": age_targets.tolist()}
    ]
    regularizer_spec = {"type": "entropy", "limit": None}
    lam = 1.0

    results = {}
    if HAS_RSWJAX:
        losses = [EqualityLoss(sex_targets), LeastSquaresLoss(age_targets)]
        regularizer = EntropyRegularizer()
        results["rswjax"] = run_rswjax(F, losses, regularizer, lam)

    if HAS_ORIGINAL_RSW:
        orig_losses = [OrigEqualityLoss(sex_targets), OrigLeastSquaresLoss(age_targets)]
        orig_regularizer = OrigEntropyRegularizer()
        results["rsw_original"] = run_original_rsw(F, orig_losses, orig_regularizer, lam)

    save_test_case("03_mixed_losses", F, losses_spec, regularizer_spec, lam, results)
    print("Generated: 03_mixed_losses")


def generate_entropy_regularizer():
    """Test case 04: Entropy regularizer with limit."""
    np.random.seed(45)
    n = 1000

    categories = np.random.choice([0, 1], size=n, p=[0.7, 0.3])

    F = np.zeros((2, n))
    F[0, categories == 0] = 1
    F[1, categories == 1] = 1

    targets = np.array([0.5, 0.5])

    losses_spec = [{"type": "equality", "target": targets.tolist()}]
    regularizer_spec = {"type": "entropy", "limit": 5.0}
    lam = 1.0

    results = {}
    if HAS_RSWJAX:
        losses = [EqualityLoss(targets)]
        regularizer = EntropyRegularizer(limit=5.0)
        results["rswjax"] = run_rswjax(F, losses, regularizer, lam)

    if HAS_ORIGINAL_RSW:
        orig_losses = [OrigEqualityLoss(targets)]
        orig_regularizer = OrigEntropyRegularizer(limit=5.0)
        results["rsw_original"] = run_original_rsw(F, orig_losses, orig_regularizer, lam)

    save_test_case("04_entropy_reg_limit", F, losses_spec, regularizer_spec, lam, results)
    print("Generated: 04_entropy_reg_limit")


def generate_zero_regularizer():
    """Test case 05: Zero regularizer."""
    np.random.seed(46)
    n = 1000

    categories = np.random.choice([0, 1], size=n, p=[0.6, 0.4])

    F = np.zeros((2, n))
    F[0, categories == 0] = 1
    F[1, categories == 1] = 1

    targets = np.array([0.5, 0.5])

    losses_spec = [{"type": "equality", "target": targets.tolist()}]
    regularizer_spec = {"type": "zero"}
    lam = 1.0

    results = {}
    if HAS_RSWJAX:
        losses = [EqualityLoss(targets)]
        regularizer = ZeroRegularizer()
        results["rswjax"] = run_rswjax(F, losses, regularizer, lam)

    if HAS_ORIGINAL_RSW:
        orig_losses = [OrigEqualityLoss(targets)]
        orig_regularizer = OrigZeroRegularizer()
        results["rsw_original"] = run_original_rsw(F, orig_losses, orig_regularizer, lam)

    save_test_case("05_zero_reg", F, losses_spec, regularizer_spec, lam, results)
    print("Generated: 05_zero_reg")


def generate_larger_scale():
    """Test case 06: Larger scale problem."""
    np.random.seed(47)
    n = 10000

    # 5 binary variables
    vars_data = []
    for i in range(5):
        p = 0.3 + i * 0.1  # Different proportions
        vars_data.append(np.random.choice([0, 1], size=n, p=[p, 1-p]))

    # Design matrix: 10 rows (2 indicators per variable)
    F = np.zeros((10, n))
    for i, var in enumerate(vars_data):
        F[2*i, var == 0] = 1
        F[2*i + 1, var == 1] = 1

    # All targets 50/50
    targets = np.array([0.5, 0.5] * 5)

    losses_spec = [{"type": "equality", "target": targets.tolist()}]
    regularizer_spec = {"type": "entropy", "limit": None}
    lam = 1.0

    results = {}
    if HAS_RSWJAX:
        losses = [EqualityLoss(targets)]
        regularizer = EntropyRegularizer()
        results["rswjax"] = run_rswjax(F, losses, regularizer, lam)

    if HAS_ORIGINAL_RSW:
        orig_losses = [OrigEqualityLoss(targets)]
        orig_regularizer = OrigEntropyRegularizer()
        results["rsw_original"] = run_original_rsw(F, orig_losses, orig_regularizer, lam)

    save_test_case("06_larger_scale", F, losses_spec, regularizer_spec, lam, results)
    print("Generated: 06_larger_scale")


def generate_interaction():
    """Test case 07: Two-way interaction."""
    np.random.seed(48)
    n = 1000

    # Two variables
    sex = np.random.choice([0, 1], size=n, p=[0.6, 0.4])
    age = np.random.choice([0, 1], size=n, p=[0.7, 0.3])

    # Marginals + joint distribution (4 interaction cells)
    F = np.zeros((8, n))
    # Sex marginals
    F[0, sex == 0] = 1
    F[1, sex == 1] = 1
    # Age marginals
    F[2, age == 0] = 1
    F[3, age == 1] = 1
    # Interaction (sex x age)
    F[4, (sex == 0) & (age == 0)] = 1
    F[5, (sex == 0) & (age == 1)] = 1
    F[6, (sex == 1) & (age == 0)] = 1
    F[7, (sex == 1) & (age == 1)] = 1

    # Targets
    sex_targets = np.array([0.5, 0.5])
    age_targets = np.array([0.6, 0.4])
    interaction_targets = np.array([0.3, 0.2, 0.3, 0.2])  # joint distribution

    losses_spec = [
        {"type": "equality", "target": sex_targets.tolist()},
        {"type": "equality", "target": age_targets.tolist()},
        {"type": "equality", "target": interaction_targets.tolist()}
    ]
    regularizer_spec = {"type": "entropy", "limit": None}
    lam = 1.0

    results = {}
    if HAS_RSWJAX:
        losses = [
            EqualityLoss(sex_targets),
            EqualityLoss(age_targets),
            EqualityLoss(interaction_targets)
        ]
        regularizer = EntropyRegularizer()
        results["rswjax"] = run_rswjax(F, losses, regularizer, lam)

    if HAS_ORIGINAL_RSW:
        orig_losses = [
            OrigEqualityLoss(sex_targets),
            OrigEqualityLoss(age_targets),
            OrigEqualityLoss(interaction_targets)
        ]
        orig_regularizer = OrigEntropyRegularizer()
        results["rsw_original"] = run_original_rsw(F, orig_losses, orig_regularizer, lam)

    save_test_case("07_interaction", F, losses_spec, regularizer_spec, lam, results)
    print("Generated: 07_interaction")


def generate_different_lambdas():
    """Test case 08: Same problem with different lambda values."""
    np.random.seed(49)
    n = 1000

    categories = np.random.choice([0, 1], size=n, p=[0.7, 0.3])

    F = np.zeros((2, n))
    F[0, categories == 0] = 1
    F[1, categories == 1] = 1

    targets = np.array([0.5, 0.5])

    for lam in [0.1, 1.0, 10.0]:
        losses_spec = [{"type": "equality", "target": targets.tolist()}]
        regularizer_spec = {"type": "entropy", "limit": None}

        results = {}
        if HAS_RSWJAX:
            losses = [EqualityLoss(targets)]
            regularizer = EntropyRegularizer()
            results["rswjax"] = run_rswjax(F, losses, regularizer, lam)

        if HAS_ORIGINAL_RSW:
            orig_losses = [OrigEqualityLoss(targets)]
            orig_regularizer = OrigEntropyRegularizer()
            results["rsw_original"] = run_original_rsw(F, orig_losses, orig_regularizer, lam)

        name = f"08_lambda_{str(lam).replace('.', '_')}"
        save_test_case(name, F, losses_spec, regularizer_spec, lam, results)
        print(f"Generated: {name}")


if __name__ == "__main__":
    print("Generating test cases...")
    print(f"rswjax available: {HAS_RSWJAX}")
    print(f"original rsw available: {HAS_ORIGINAL_RSW}")
    print()

    generate_basic_equality()
    generate_least_squares()
    generate_mixed_losses()
    generate_entropy_regularizer()
    generate_zero_regularizer()
    generate_larger_scale()
    generate_interaction()
    generate_different_lambdas()

    print("\nDone! Test cases saved to test_cases/")
