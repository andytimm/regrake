## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Resubmission notes

This is a resubmission. Changes since the previous submission:

* Added `microdata` to `inst/WORDLIST` to address spell-check NOTE. The other
  flagged words (`Barratt`, `et`, `al`) were already in the WORDLIST but may
  not have been present in the previous tarball.

* Addressed "CPU time > 2.5 times elapsed time" NOTE on Debian (OpenBLAS):
  - Added `RhpcBLASctl` to Suggests and call `blas_set_num_threads(2L)` in
    `tests/testthat/helper.R` and the vignette setup chunk. Unlike environment
    variables (which OpenBLAS reads only at library load time), this makes a
    direct runtime API call to limit threads to 2 regardless of when BLAS was
    initialized.
  - Expanded env var coverage in `tests/testthat.R` to set `OMP_THREAD_LIMIT`,
    `OPENBLAS_NUM_THREADS`, and `OMP_NUM_THREADS` (belt-and-suspenders for
    different OpenBLAS builds).

## Platforms tested

* Local: R 4.5.2 on Windows 11 (0 errors, 0 warnings, 0 notes)
* win-builder (R-devel): 0 errors, 0 warnings, 1 note (new submission)

