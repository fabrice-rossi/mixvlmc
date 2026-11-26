## Update of an existing package

This is an update of mixvlmc (0.2.2 -> 0.3.0). It introduces a class for
discrete time series, a new representation of context trees with a LaTeX
output integrated with knitr, and fixes numerous bugs (see NEWS.md).

## Test failure on the MKL check of 0.2.2

The tests of mixvlmc 0.2.2 fail on the "tests-MKL" additional check
(test-tune_covlmc.R:72). The cut off values computed by `cutoff.covlmc()` were
one ulp below p-values stored in the model, while `prune.covlmc()` refits
logistic models and recomputes these p-values. As MKL results are not bitwise
reproducible, the recomputed p-values could fall below the cut off values, so
that a pruning step had no effect and `tune_covlmc()` gave different results
for equivalent settings. The cut off values now include a relative margin
(about 7e-9) that absorbs these rounding errors. A new test emulates this
behaviour by perturbing the log likelihoods of the logistic models.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local Ubuntu 24.04.5 LTS, R 4.6.2

## Reverse dependencies

There are currently no reverse dependencies on CRAN.
