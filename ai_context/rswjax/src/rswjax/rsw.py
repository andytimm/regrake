import numpy as np
import time
from rswjax.solver import admm

#Convenience function for providing user info on fit quality
def max_pct_difference(list1, list2):

    array1 = np.array(list1)
    array2 = np.array(list2)

    difference = np.abs(array1 - array2)

    with np.errstate(divide='ignore', invalid='ignore'):
        percent_difference = np.where(array1 != 0, (difference / array1) * 100, np.nan)

    max_percent_diff = np.nanmax(percent_difference)

    return max_percent_diff

def rsw(df, funs, losses, regularizer, lam=1, **kwargs):
    """Optimal representative sample weighting.

    Arguments:
        - df: Pandas dataframe
        - funs: functions to apply to each row of df. Function warns if len(losses) != ncols.
        - losses: list of losses, each one of rswjax.EqualityLoss, rswjax.InequalityLoss, rswjax.LeastSquaresLoss,
            or rswjax.KLLoss.
        - regularizer: One of rswjax.ZeroRegularizer, rswjax.EntropyRegularizer,
            or rswjax.KLRegularizer, rswjax.BooleanRegularizer
        - lam (optional): Regularization hyper-parameter (default=1).
        - kwargs (optional): additional arguments to be sent to solver. For example: verbose=True,
            maxiter=5000, rho=50, eps_rel=1e-5, eps_abs=1e-5.

    Returns:
        - w: Final sample weights.
        - out: Final induced expected values as a list of numpy arrays.
        - sol: Dictionary of final ADMM variables. Can be ignored.
    """
    if funs is not None:
        F = []
        for f in funs:
            F += [df.apply(f, axis=1)]
        F = np.array(F, dtype=float)
    else:
        F = np.array(df).T
    m, n = F.shape

    # remove nans by changing F
    rows_nan, cols_nan = np.where(np.isnan(F))
    desireds = [l.fdes for l in losses]
    desired = np.concatenate(desireds)
    if rows_nan.size > 0:
        for i in np.unique(rows_nan):
            F[i, cols_nan[rows_nan == i]] = desired[i]


    total_fdes_length = sum(len(loss.fdes) for loss in losses)

    if m > total_fdes_length:
        print("Warning! A loss is not defined for all columns, which is usually an error. "
            "Please double check inputs and outputs for issues closely.")

    if m < total_fdes_length:
        print("Warning! More losses are passed than columns, which is usually an error. "
            "Please double check inputs and outputs for issues closely.")

    tic = time.time()
    sol = admm(F, losses, regularizer, lam, **kwargs)
    toc = time.time()
    if kwargs.get("verbose", False):
        print("ADMM took %3.5f seconds" % (toc - tic))

    out = []
    means = F @ sol["w_best"]
    ct = 0
    for m in [l.m for l in losses]:
        out += [means[ct:ct + m]]
        ct += m

    if kwargs.get("verbose"):
         if total_fdes_length != len(sol["f"]):
            print("losses/columns were not same length, not printing pct difference.")
         else:
            max_diff = max_pct_difference(out, [l.fdes for l in losses])
            print(f"The largest pct difference between desired and achieved weighted values is {max_diff:.2f}%.")

        
    return sol["w_best"], out, sol