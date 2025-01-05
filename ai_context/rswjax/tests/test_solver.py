import numpy as np
import jax.numpy as jnp
import cvxpy as cp
import rswjax
from rswjax.solver import _projection_simplex

# The function shouldn't have changed in conversion to jax at all, so test that
def projection_simplex_numpy(v, z=1):
    n_features = v.shape[0]
    u = np.sort(v)[::-1]
    cssv = np.cumsum(u) - z
    ind = np.arange(n_features) + 1
    cond = u - cssv / ind > 0
    rho = ind[cond][-1]
    theta = cssv[cond][-1] / float(rho)
    w = np.maximum(v - theta, 0)
    return w

def test_projection_simplex():
    np.random.seed(605)  
    v = np.random.randn(10)  

    v_jax = jnp.array(v)
    projected_jax = _projection_simplex(v_jax)

    projected_numpy = projection_simplex_numpy(v)

    # Convert JAX array to NumPy for comparison
    projected_jax_numpy = np.array(projected_jax)

    np.testing.assert_allclose(projected_jax_numpy, projected_numpy, atol=1e-5)

def test_solver():
    n = 100
    m = 20
    F = np.random.randn(m, n)
    fdes1 = np.random.randn(m // 2)
    fdes2 = np.random.randn(m // 2)
    losses = [rswjax.LeastSquaresLoss(fdes1), rswjax.InequalityLoss(
        fdes2, -1 * np.ones(m // 2), 1 * np.ones(m // 2))]
    reg = rswjax.EntropyRegularizer()
    sol = rswjax.admm(F, losses, reg, 1, verbose=True)
    
    w = cp.Variable(n)
    cp.Problem(cp.Minimize(.5 * cp.sum_squares(F[:m // 2] @ w - fdes1) - cp.sum(cp.entr(w))),
               [cp.sum(w) == 1, w >= 0, cp.max(cp.abs(F[m // 2:] @ w - fdes2)) <= 1]).solve(solver=cp.ECOS)
    np.testing.assert_allclose(w.value, sol["w"], atol=1e-3)