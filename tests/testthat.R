# Limit BLAS/LAPACK threads to avoid CRAN "CPU time N times elapsed time" notes
Sys.setenv(
  OMP_NUM_THREADS = 1,
  OPENBLAS_NUM_THREADS = 1,
  MKL_NUM_THREADS = 1,
  VECLIB_MAXIMUM_THREADS = 1,
  BLAS_NUM_THREADS = 1
)

library(testthat)
library(regrake)

test_check("regrake")
