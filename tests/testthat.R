# Limit BLAS/LAPACK threads to avoid CRAN "CPU time N times elapsed time" notes.
# Set all three env vars: different OpenBLAS builds read different ones.
# OMP_THREAD_LIMIT is an OpenMP hard cap; OPENBLAS_NUM_THREADS and OMP_NUM_THREADS
# are read by OpenBLAS at library load time.
Sys.setenv("OMP_THREAD_LIMIT" = 2, "OPENBLAS_NUM_THREADS" = 2, "OMP_NUM_THREADS" = 2)

library(testthat)
library(regrake)

test_check("regrake")
