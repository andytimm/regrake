import jax.numpy as jnp
import jax
import numpy as np
import scipy.sparse as sparse
import qdldl
from rswjax.losses import *
from rswjax.regularizers import *
from rswjax.losses import *
from rswjax.regularizers import *

# Jax default of 32 is insufficiently accurate.
# It's possible mixed precision could be viable, but not worth the squeeze right now.
jax.config.update("jax_enable_x64", True)

# I've tried a few different other simplex projection algorithms which have better theoretical
# time complexity, but in practice, this is the faster than the algorithms of
# Blondel (https://mblondel.org/publications/mblondel-icpr2014.pdf),
# Condat (https://hal.science/hal-01056171v2/document), or
# Dai/Chen (https://arxiv.org/abs/2204.08153).
# Numpy is ~5x faster than a JAX translation of this function too. 
def _projection_simplex(v, z=1):
    n_features = v.shape[0]
    u = np.sort(v)[::-1]
    cssv = np.cumsum(u) - z
    ind = np.arange(n_features) + 1
    cond = u - cssv / ind > 0
    rho = ind[cond][-1]
    theta = cssv[cond][-1] / float(rho)
    w = np.maximum(v - theta, 0)
    return w

@jit
def compute_norms_and_epsilons(f, w, w_old, y, z, u, F, rho, eps_abs, eps_rel):
    # Norm calculations
    s = rho * jnp.concatenate([
        F @ w - f,
            w - w_old,
            w - w_old
    ])
    r = jnp.concatenate([
        f - F @ w,
        w - w,
        w - w
    ])
    s_norm = jnp.linalg.norm(s)
    r_norm = jnp.linalg.norm(r)

    # Epsilon calculations
    p = F.shape[0] + 2 * w.size
    Ax_k_norm = jnp.linalg.norm(jnp.concatenate([f, w, w]))
    Bz_k_norm = jnp.linalg.norm(jnp.concatenate([w, w, w]))
    ATy_k_norm = jnp.linalg.norm(rho * jnp.concatenate([y, z, u]))
    eps_pri = jnp.sqrt(p) * eps_abs + eps_rel * jnp.maximum(Ax_k_norm, Bz_k_norm)
    eps_dual = jnp.sqrt(p) * eps_abs + eps_rel * ATy_k_norm

    return s_norm, r_norm, eps_pri, eps_dual

def admm(F, losses, reg, lam, rho=50, maxiter=5000, warm_start={}, verbose=False,
         eps_abs=1e-5, eps_rel=1e-5):
    m, n = F.shape
    ms = [l.m for l in losses]

    # Default warm start values
    f = warm_start.get("f", jnp.array(F.mean(axis=1)).flatten())
    w = warm_start.get("w", jnp.ones(n) / n)
    w_bar = warm_start.get("w_bar", jnp.ones(n) / n)
    w_tilde = warm_start.get("w_tilde", jnp.ones(n) / n)
    y = warm_start.get("y", jnp.zeros(m))
    z = warm_start.get("z", jnp.zeros(n))
    u = warm_start.get("u", jnp.zeros(n))

    # Constructing and factorizing the Q matrix with scipy and qdldl
    F_sparse = sparse.csc_matrix(F)
 
    Q = sparse.bmat([
        [2 * sparse.eye(n), F_sparse.T],
        [F_sparse, -sparse.eye(m)]
    ])
    factor = qdldl.Solver(Q)

    w_best = None
    best_objective_value = float("inf")

    if verbose:
            print(u'Iteration     | ||r||/\u03B5_pri | ||s||/\u03B5_dual')

    for k in range(maxiter):
        ct_cum = 0
        
        # It might be possible to rewrite the update block to be jittable, but it'd require
        # a significant refactoring to precalculate all the needed loss evals/proxes and
        # pass them to a jittable update f. 
        for l in losses:
            f = f.at[ct_cum:ct_cum + l.m].set(l.prox(F[ct_cum:ct_cum + l.m] @ w -
                                                    y[ct_cum:ct_cum + l.m], 1 / rho))
            ct_cum += l.m

        w_tilde = reg.prox(w - z, lam / rho)
        w_bar = _projection_simplex(w - u)

        rhs_np = np.concatenate([
            np.array(F.T @ (f + y) + w_tilde + z + w_bar + u),
            np.zeros(m)
        ])
        w_new_np = factor.solve(rhs_np)[:n]
        w_new = jnp.array(w_new_np)
        
        w_old = w
        w = w_new

        y = y + f - F @ w
        z = z + w_tilde - w
        u = u + w_bar - w

        s_norm, r_norm, eps_pri, eps_dual = compute_norms_and_epsilons(
    f, w, w_old, y, z, u, F, rho, eps_abs, eps_rel)

        

        if verbose and k % 50 == 0:
            print(f'It {k:03d} / {maxiter:03d} | {r_norm / eps_pri:8.5e} | {s_norm / eps_dual:8.5e}')

        if isinstance(reg, BooleanRegularizer):
            ct_cum = 0
            objective = 0.
            for l in losses:
                objective += l.evaluate(F[ct_cum:ct_cum + l.m] @ w_tilde)
                ct_cum += l.m
            if objective < best_objective_value:
                if verbose:
                    print(f"Found better objective value: {best_objective_value:3.5f} -> {objective:3.5f}")
                best_objective_value = objective
                w_best = w_tilde

        if r_norm <= eps_pri and s_norm <= eps_dual:
            break
        
        if np.isnan(r_norm) or np.isnan(s_norm):
            raise ValueError("r_norm or s_norm have NaNed out, usually indicating a poorly formulated\
                            optimization problem. A common cause with in ADMM is excessively high or\
                            low values of Lambda or Rho; please sanity check your problem.")

    if not isinstance(reg, BooleanRegularizer):
        w_best = w_bar

    return {
        "f": np.array(f),
        "w": np.array(w),
        "w_bar": np.array(w_bar),
        "w_tilde": np.array(w_tilde),
        "y": np.array(y),
        "z": np.array(z),
        "u": np.array(u),
        "w_best": np.array(w_best) if w_best is not None else None
    }