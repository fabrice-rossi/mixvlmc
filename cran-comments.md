## Resubmission

This is a resubmission of mixvlmc 0.3.0, which did not pass the incoming checks
because of the overall check time on Windows (r-devel-windows-x86_64: "Overall
checktime 11 min > 10 min"). In this version (0.3.1), the slowest tests (mostly
exhaustive comparisons with slow reference implementations and between the two
backends of the package) are skipped on CRAN. The overall check time on the
R-hub Windows (R devel) platform went from 11 min to 8 min.

## Update of an existing package

This is an update of mixvlmc (0.2.2 -> 0.3.1). It introduces a class for
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
* GitHub Actions: Windows (R release), macOS aarch64 (R release),
  Ubuntu (R devel, release, oldrel-1)
* R-hub: Windows (R devel), Fedora Linux with MKL (R devel)

## Reverse dependencies

There are currently no reverse dependencies on CRAN.
