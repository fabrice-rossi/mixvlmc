# mixvlmc 0.2.1
This release has no visible changes and fixes internal issues:

* missing package in Suggests
* double definition of a C++ internal function

# mixvlmc 0.2.0

## Breaking changes
* the `mode` parameter of `cutoff.vlmc()` has been renamed to `scale`
* the `type` parameter has been removed from `contexts()` variants and the 
  default result format has been modified significantly
* the `counts` parameter from `contexts.vlmc()` and `contexts.covlmc()` has
  been replaced by a logical parameter named `local`
* `contexts()` sequences are now reported by default in temporal order

## New features
### C++ backend
The main major new feature of this version is the inclusion of a C++ 
implementation of context tree and VLMC construction. A new option 
`mixvlmc.backend` can be used to switch globally from the original `"R"` 
implementation to the new `"C++"` back end. A new `backend` parameter has
been added to `ctx_tree()`, `vlmc()` and `tune_vlmc()` to enable local back end
selection. 

The C++ implementation is significantly faster than the R implementation, at 
least by a factor 10. While it has been thoroughly tested, it is still 
considered experimental notably because it does not apply to COVLMC (setting the 
global  option to `"C++"` has not effect on COVLMC model construction). For 
context trees and VLMC, results should not depend on the back end, at least 
within numerical precision. The only notable difference is the ordering of the 
contexts which differs between back ends: in a call to `contexts()`, the first 
context for the R back end will generally not be the first context for the C++ 
back end. 

### Other new features
* `contexts()` can now report the positions of each context in the original time
  series
* nodes of context trees can be extracted individually as `ctx_node` objects 
  using the `find_sequence()` function. A collection of new functions can be
  used to manipulate the nodes and gain fine grain information on the 
  corresponding sequences (issue #50). 
* by default, `contexts()` reports now contexts as a list of `ctx_node` objects
* new functions `predict.vlmc()` and `predict.covlmc()` can be used to make one 
  step ahead predictions of a time series based on a (CO)VLMC model (issue #46).
  Those function are documented in  a new vignette (`vignette("prediction")`)
* the log likelihood calculations performed by `logLik.vlmc()`, `logLik.covlmc()`
  `loglikelihood()` and `loglikelihood.covlmc()` have been revised, expanded to 
  include three possible definitions of the likelihood function, and documented 
  in a new vignette (`vignette("likelihood")`)
* `tune_vlmc()` and `tune_covlmc()` can be used with the different likelihood 
  function definitions
* results of `tune_vlmc()` and `tune_covlmc()` can be plotted using base R 
  graphics or ggplot2 (issue #36)
* `tune_covlmc()` can trim the best model (and the initial one) if asked to  
* `trim.covlmc()` implements simple trimming for VGAM based objects (issue #48)
* `cutoff()` uses a new `tolerance` parameter to avoid reporting cut off values
  that are almost identical due to numerical imprecision 
* `simulate.vlmc()` implements a user specified burn in period (issue #40)
* `simulate.vlmc()` and `simulate.covlmc()` now handle the random generator 
  state as does `stats::simulate()` (issue #56)
  
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
* Improved type consistency by reporting counts as integers in `contexts()` 
  results
* Fixed documentation: all models can be adjusted on time series of logical 
  values
* Synchronized `ctx_tree()` documentation and its default value
* Synchronized `contexts()` documentation and its default value
* Added more trimming for COVLMC models in `trim.covlmc()`

# mixvlmc 0.1.1

* Added missing documentation about the return values of `simulate.vlmc()` and
  `simulate.covlmc()`

# mixvlmc 0.1.0

* Added a `NEWS.md` file to track changes to the package.
