import pytest
import cvxpy as cp
import numpy as np
import rswjax

@pytest.fixture
def setup_data():
    np.random.seed(605)
    w = np.random.randn(10)
    prior = np.random.uniform(10)
    prior /= 10
    lam = .5
    return w, prior, lam


def test_zero_regularizer(setup_data):
    w, _, _ = setup_data
    zero_reg = rswjax.ZeroRegularizer()
    np.testing.assert_allclose(zero_reg.prox(w, .5), w)

def test_entropy_regularizer(setup_data):
    w, _, lam = setup_data
    entropy_reg = rswjax.EntropyRegularizer()
    what = cp.Variable(10)
    cp.Problem(cp.Minimize(-cp.sum(cp.entr(what)) + 1 /
                           (2 * lam) * cp.sum_squares(what - w))).solve(solver=cp.ECOS)
    np.testing.assert_allclose(what.value, entropy_reg.prox(w, lam), atol=1e-4)

def test_kl_regularizer(setup_data):
    w, prior, lam = setup_data
    kl_reg = rswjax.KLRegularizer(prior)
    what = cp.Variable(10)
    cp.Problem(cp.Minimize(-cp.sum(cp.entr(what)) - cp.sum(cp.multiply(what, np.log(prior))) + 1 /
                           (2 * lam) * cp.sum_squares(what - w))).solve(solver=cp.ECOS)
    np.testing.assert_allclose(what.value, kl_reg.prox(w, lam), atol=1e-4)

def test_boolean_regularizer(setup_data):
    w, _, _ = setup_data
    k = 3
    regularizer = rswjax.BooleanRegularizer(k)
    result = regularizer.prox(w, None)
    expected = np.zeros_like(w)
    idx = np.argsort(w)[-k:]
    expected[idx] = 1.0 / k
    np.testing.assert_allclose(result, expected, atol=1e-4)

def test_sum_squares_regularizer(setup_data):
    w, _, lam = setup_data
    sum_squares_reg = rswjax.SumSquaresRegularizer()
    what = cp.Variable(10)
    # Formulate the optimization problem for the sum of squares regularizer
    cp.Problem(cp.Minimize(cp.sum_squares(what) + 1 /
                           (2 * lam) * cp.sum_squares(what - w))).solve(solver=cp.ECOS)
    # Compare the CVXPY solution with the proximal operator result
    np.testing.assert_allclose(what.value, sum_squares_reg.prox(w, lam), atol=1e-4)
