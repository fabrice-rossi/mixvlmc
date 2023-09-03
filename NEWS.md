# mixvlmc (development version)

## New features
* `contexts()` can now report the positions of each context in the original time
  series
* the log likelihood calculations performed by `logLik()` and `loglikelihood()` 
  have been revised, expanded to include three possible definitions of the 
  likelihood function, and documented in a new vignette 
  (`vignette("likelihood")`)
* `tune_vlmc()` and `tune_covlmc()` can be used with the different likelihood 
  function definitions
* results of `tune_vlmc()` and `tune_covlmc()` can be plotted using base R graphics 
  (issue #36)
* `tune_covlmc()` can trim the best model (and the initial one) is asked to  
* `cutoff()` uses a new `tolerance` parameter to avoid reporting cut off values
  that are almost identical due to numerical imprecision 
* `trim.covlmc()` implements simple trimming for VGAM based objects (issue #48)
* `simulate.vlmc()` implements a user specified burn in period (issue #40)
* `simulate.vlmc()` and `simulate.covlmc()` now handle the random generator state
  as does `stats::simulate()` (issue #56)
  
## Minor improvements and bug fixes
* Improved the test coverage (issue #54)
* Fixed several bugs related to degenerate VLMC (issue #55)
* Fixed a bug in the likelihood calculation for internal context nodes
* Improved type consistency by reporting counts as integers in `contexts()` results
* Fixed documentation: all models can be adjusted on time series of logical values
* Synchronized `ctx_tree()` documentation and its default value
* Synchronized `contexts()` documentation and its default value
* Added more trimming for COVLMC models in `trim.covlmc()`

# mixvlmc 0.1.1

* Added missing documentation about the return values of `simulate.vlmc()` and
  `simulate.covlmc()`

# mixvlmc 0.1.0

* Added a `NEWS.md` file to track changes to the package.
