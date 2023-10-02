# mixvlmc (development version)

## New features
* new functions `predict.vlmc()` and `predict.covlmc()` can be used to make one 
  step ahead predictions of a time series based on a (CO)VLMC model (issue #46).
  Those function are documented in  a new vignette (`vignette("prediction")`)
* `contexts()` can now report the positions of each context in the original time
  series
* nodes of context trees can be extracted individually as `ctx_node` objects 
  using the `find_sequence()` function. A collection of new functions can be
  used to manipulate the nodes and gain fine grain information on the 
  corresponding sequences (issue #50). 
* the log likelihood calculations performed by `logLik.vlmc()`, `logLik.covlmc()`
  `loglikelihood()` and `loglikelihood.covlmc()` have been revised, expanded to 
  include three possible definitions of the likelihood function, and documented 
  in a new vignette (`vignette("likelihood")`)
* `tune_vlmc()` and `tune_covlmc()` can be used with the different likelihood 
  function definitions
* results of `tune_vlmc()` and `tune_covlmc()` can be plotted using base R 
  graphics or ggplot2 (issue #36)
* `tune_covlmc()` can trim the best model (and the initial one) if asked to  
* `cutoff()` uses a new `tolerance` parameter to avoid reporting cut off values
  that are almost identical due to numerical imprecision 
* `trim.covlmc()` implements simple trimming for VGAM based objects (issue #48)
* `simulate.vlmc()` implements a user specified burn in period (issue #40)
* `simulate.vlmc()` and `simulate.covlmc()` now handle the random generator state
  as does `stats::simulate()` (issue #56)
  
## Minor improvements and bug fixes
* Improved the test coverage (issue #54)
* Fixed several bugs in internal sampling methods that made `simulate.covlmc()`
  unreliable for state spaces with three or more states
* Fixed a bug in `simulate.covlmc()` that occurred in contexts with a longer 
  self memory compared to their covariate memory
* Fixed several bugs in `metrics.vlmc()` and align the results with the ones
  obtained by using direct calculation on the results of `predict.vlmc()`
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
