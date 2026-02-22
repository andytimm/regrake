# Limit BLAS/LAPACK threads to avoid CRAN "CPU time N times elapsed time" notes.
# OMP_THREAD_LIMIT is an OpenMP hard cap enforced at runtime, unlike
# OMP_NUM_THREADS which OpenBLAS reads only at library load time (before R
# code runs) and then ignores.
Sys.setenv("OMP_THREAD_LIMIT" = 2)

library(testthat)
library(regrake)

test_check("regrake")
