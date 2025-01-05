import pytest
import cvxpy as cp
import numpy as np
import rswjax
from rswjax.losses import _entropy_prox

@pytest.fixture
def setup_data():
    np.random.seed(605)
    m = 10
    f = np.random.randn(m)
    fdes = np.random.randn(m)
    lam = 1
    lower = np.array([-.3])
    upper = np.array([.3])
    return m, f, fdes, lam, lower, upper

def test_equality_loss(setup_data):
    m, f, fdes, lam, _, _ = setup_data
    equality = rswjax.EqualityLoss(fdes)
    fhat = cp.Variable(m)

    cp.Problem(cp.Minimize(1 / lam * cp.sum_squares(fhat - f)),
                [fhat == fdes]).solve()
    np.testing.assert_allclose(fhat.value, equality.prox(f,lam))

def test_inequality_loss(setup_data):
    m, f, fdes, lam, lower, upper = setup_data
    inequality = rswjax.InequalityLoss(fdes, lower, upper)

    fhat = cp.Variable(m)
    cp.Problem(cp.Minimize(1 / lam * cp.sum_squares(fhat - f)),
               [lower <= fhat - fdes, fhat - fdes <= upper]).solve()
    np.testing.assert_allclose(fhat.value, inequality.prox(f, lam))

def test_least_squares_loss(setup_data):
    m, f, fdes, lam, _, _ = setup_data
    d = np.random.uniform(0, 1, size=m)
    lstsq = rswjax.LeastSquaresLoss(fdes, d)
    fhat = cp.Variable(m)

    cp.Problem(cp.Minimize(1 / 2 * cp.sum_squares(cp.multiply(d, fhat - fdes)) +
                            1 / (2 * lam) * cp.sum_squares(fhat - f))).solve()
    np.testing.assert_allclose(fhat.value, lstsq.prox(f, lam))

def test_entropy_prox(setup_data):
    m, f, fdes, lam, _, _ = setup_data
    f = np.random.uniform(0, 1, size=m)
    f /= f.sum()
    fdes = np.random.uniform(0, 1, size=m)
    fdes /= fdes.sum()
    fhat = cp.Variable(m)

    cp.Problem(cp.Minimize(cp.sum(-cp.entr(fhat)) +
                           1 / (2 * lam) * cp.sum_squares(fhat - f))).solve(solver=cp.ECOS)
    np.testing.assert_allclose(
        fhat.value, _entropy_prox(f, lam), atol=1e-5)
    
def test_kl_loss(setup_data):
    m, f, fdes, lam, _, _ = setup_data
    f = np.random.uniform(0, 1, size=m)
    f /= f.sum()
    fdes = np.random.uniform(0, 1, size=m)
    fdes /= fdes.sum()
    kl = rswjax.KLLoss(fdes, scale=.5)
    fhat = cp.Variable(m, nonneg=True)

    cp.Problem(cp.Minimize(.5 * (cp.sum(-cp.entr(fhat) - cp.multiply(fhat, np.log(fdes)))) +
                        1 / (2 * lam) * cp.sum_squares(fhat - f))).solve(solver=cp.ECOS)
    np.testing.assert_allclose(fhat.value, kl.prox(f, lam), atol=1e-5)
