from jax import jit, lax
import jax.numpy as jnp
from functools import partial
from rswjax.losses import *

@jit
def prox_zero_regularizer(w, lam):
    return lax.stop_gradient(w)

class ZeroRegularizer():
    def __init__(self):
        pass

    def prox(self, w, lam):
        return prox_zero_regularizer(w, lam)

@jit
def prox_entropy_regularizer(w, lam, limit, w_size):
    what = lam * jnp.real(lambertw(jnp.exp(w / lam - 1) / lam))
    if limit is not None:
        what = jnp.clip(what, 1 / (limit * w_size), limit / w_size)
    return lax.stop_gradient(what)

class EntropyRegularizer():
    def __init__(self, limit=None):
        if limit is not None and limit <= 1:
            raise ValueError(f"limit is {limit:.3f}. It must be > 1.")
        self.limit = limit

    def prox(self, w, lam):
        return prox_entropy_regularizer(w, lam, self.limit, w.size)

@jit
def prox_kl_regularizer(w, lam, prior, limit):
    return prox_entropy_regularizer(w + lam * jnp.log(prior), lam, limit, w.size)

class KLRegularizer():
    def __init__(self, prior, limit=None):
        self.prior = prior
        self.limit = limit

    def prox(self, w, lam):
        return prox_kl_regularizer(w, lam, self.prior, self.limit)

    
# No personal use for this, so probably won't implement unless requested
class CardinalityRegularizer():

    def __init__(self, k):
        raise NotImplementedError
        self.k = k

    def prox(self, w, lam):
        out = jnp.copy(w)
        idx = jnp.argsort(w)[:-self.k]
        out[idx] = 0.
        return out

@partial(jit, static_argnums=2)
def prox_boolean_regularizer(w, lam, k):
    # Ensure k is a static value for JIT compilation
    top_k_values, top_k_indices = lax.top_k(w, k)

    # Create a mask of zeros
    mask = jnp.zeros_like(w)

    # Update the mask to set top k positions to 1. / k
    mask = mask.at[top_k_indices].set(1. / k)

    return lax.stop_gradient(mask)

class BooleanRegularizer():
    def __init__(self, k):
        self.k = k

    def prox(self, w, lam):
        return prox_boolean_regularizer(w, lam, self.k)

@jit
def prox_sum_squares(w, lam):
    """
    Proximal operator for the sum of squares regularizer.

    Should allow regularization similar to Ben-Michael et al. (2023),
    who use this regularizer in their multilevel calibration weights optimizer.
    """
    return w / (1 + 2 * lam)

class SumSquaresRegularizer():
    def __init__(self):
        pass

    def prox(self, w, lam):
        return prox_sum_squares(w, lam)