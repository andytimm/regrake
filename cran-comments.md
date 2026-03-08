## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Resubmission notes

Changes since previous submission:

* Addressed "CPU time > 2.5 times elapsed time" NOTE via `RhpcBLASctl::blas_set_num_threads(2L)`.
* Spell-check NOTE: all flagged words (`Barratt`, `et`, `al`, `microdata`) are
  in `inst/WORDLIST`; not certain why this isn't sufficient to whitelist them, but none are typos.
* Fixed CVXR test compatibility for win-builder, given 1.8.1 API changes.
* DESCRIPTION Title and License format.

## Platforms tested

* Local: R 4.5.2 on Windows 11 (0 errors, 0 warnings, 0 notes)
* win-builder (R-devel): 0 errors, 0 warnings, 1 note (new submission + spelling)
