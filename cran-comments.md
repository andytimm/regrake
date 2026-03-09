## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Resubmission notes

Changes since previous submission:

* Addressed "CPU time > 2.5 times elapsed time" NOTE by limiting BLAS threads
  to 1 on CRAN (`RhpcBLASctl::blas_set_num_threads(1L)`) and skipping
  heavier integration tests. I believe the CPU/elapsed ratio was caused by implicit
  BLAS parallelism in sparse matrix operations.
* Spell-check NOTE: `Barratt`, `et`, `al` are an author citation (Barratt et al. 2021);
  `microdata` is a standard survey methodology term. None are typos.
* Fixed CVXR test compatibility for win-builder, given 1.8.1 API changes.
* DESCRIPTION Title and License format.

## Platforms tested

* Local: R 4.5.2 on Windows 11 (0 errors, 0 warnings, 0 notes)
* win-builder (R-devel): 0 errors, 0 warnings, 1 note (spelling)
* R-hub ubuntu-release (R 4.5.2 on Ubuntu 24.04): 0 errors, 0 warnings, 0 notes
