import jax.numpy as jnp
from jax import jit
from jax.scipy.special import kl_div
from numbers import Number
from ott.math.utils import lambertw

class Loss:
    def prox(self, f, lam):
        raise NotImplementedError
    
    # Not all losses must implement an evaluate function, but it's needed for more complex losses. 

@jit
def prox_equality(f, fdes):
    return fdes

class EqualityLoss(Loss):
    def __init__(self, fdes):
        if isinstance(fdes, Number):
            fdes = jnp.array([fdes])
        self.fdes = fdes
        self.m = fdes.size

    def prox(self, f, lam):
        return prox_equality(f, self.fdes)
    
def jit_prox_inequality(fdes, lower, upper):
    def prox(f, lam):
        return jnp.clip(f, fdes + lower, fdes + upper)
    return jit(prox)

class InequalityLoss(Loss):
    def __init__(self, fdes, lower, upper):
        if isinstance(fdes, Number):
            fdes = jnp.array([fdes])
        assert (lower <= upper).all()
        self.fdes = fdes
        self.m = fdes.size
        self.lower = lower
        self.upper = upper
        self.prox = jit_prox_inequality(fdes, lower, upper)

@jit
def jit_prox_ls(f, lam, diag_weight, fdes):
    return (diag_weight**2 * fdes + f / lam) / (diag_weight**2 + 1 / lam)

@jit
def jit_evaluate_ls(f, diag_weight, fdes):
    return jnp.sum(jnp.square(diag_weight * (f - fdes)))

class LeastSquaresLoss(Loss):

    def __init__(self, fdes, diag_weight=None):
        if isinstance(fdes, Number):
            fdes = jnp.array([fdes])
        self.fdes = fdes
        self.m = fdes.size
        if diag_weight is None:
            diag_weight = 1.
        self.diag_weight = diag_weight

    def prox(self, f, lam):
        return jit_prox_ls(f, lam, self.diag_weight, self.fdes)

    def evaluate(self, f):
        return jit_evaluate_ls(f, self.diag_weight, self.fdes)


@jit
def _entropy_prox(f, lam):
    return lam * jnp.real(lambertw(jnp.exp(f / lam - 1) / lam))

class KLLoss(Loss):

    def __init__(self, fdes, scale=1):
        if isinstance(fdes, Number):
            fdes = jnp.array([fdes])
        self.fdes = fdes
        self.m = fdes.size
        self.scale = scale

    def prox(self, f, lam):
        return _entropy_prox(f + lam * self.scale * jnp.log(self.fdes), lam * self.scale)

    def evaluate(self, f):
        return self.scale * jnp.sum(kl_div(f, self.fdes))