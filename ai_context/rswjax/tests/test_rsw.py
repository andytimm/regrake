import pytest
import pandas as pd
import numpy as np
import rswjax

@pytest.fixture
def dataframe():
    np.random.seed(605)
    n = 100
    age = np.random.randint(20, 30, size=n) * 1.
    sex = np.random.choice([0., 1.], p=[.4, .6], size=n)
    height = np.random.normal(5, 1, size=n)

    return pd.DataFrame({
        "age": age,
        "sex": sex,
        "height": height
    })

@pytest.fixture
def funs():
    return [
        lambda x: x.age,
        lambda x: x.sex == 0 if not np.isnan(x.sex) else np.nan,
        lambda x: x.height
    ]

@pytest.fixture
def regularizer_entropy():
    return rswjax.EntropyRegularizer()

@pytest.fixture
def regularizer_kl(dataframe):
    fdes = np.random.uniform(0, 1, size=100)
    fdes /= fdes.sum()
    return rswjax.KLRegularizer(prior=fdes)

@pytest.fixture
def losses_equality():
    return [rswjax.EqualityLoss(25), rswjax.EqualityLoss(.5), rswjax.EqualityLoss(5.3)]

@pytest.fixture
def losses_kl():
    return [rswjax.EqualityLoss(25), rswjax.EqualityLoss(.5), rswjax.KLLoss(5.3, scale=1)]

def test_entropy_weights(dataframe, funs, losses_equality, regularizer_entropy):
    rswjax.rsw(dataframe, funs, losses_equality, regularizer_entropy, 1., verbose=True)

def test_kl_loss(dataframe, losses_kl, regularizer_entropy):
    rswjax.rsw(dataframe, None, losses_kl, regularizer_entropy, 1., eps_abs=1e-8, verbose=True)

def test_kl_reg(dataframe, funs, losses_equality, regularizer_kl):
    rswjax.rsw(dataframe, funs, losses_equality, regularizer_kl, 1., verbose=True)

def test_array_inputs(dataframe, funs, regularizer_entropy):
    array_of_targets = np.array([25, .5, 5.3])
    losses = [rswjax.EqualityLoss(array_of_targets.flatten())]
    rswjax.rsw(dataframe, funs, losses, regularizer_entropy, 1., eps_abs=1e-8, verbose=True)

def test_bool_weights(dataframe, funs):
    losses = [rswjax.LeastSquaresLoss(25), rswjax.LeastSquaresLoss(.5), rswjax.LeastSquaresLoss(5.3)]
    regularizer = rswjax.BooleanRegularizer(5)
    rswjax.rsw(dataframe, funs, losses, regularizer, 1., verbose=True)

def test_nans(dataframe, funs, losses_equality, regularizer_entropy):
    df = dataframe.copy()
    for i, j in zip(np.random.randint(50, size=25), np.random.randint(3, size=25)):
        df.iat[i, j] = np.nan
    rswjax.rsw(df, funs, losses_equality, regularizer_entropy, 1.)
