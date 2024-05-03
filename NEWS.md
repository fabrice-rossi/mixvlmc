# mixvlmc (development version)
## Breaking changes
* `draw()` has now a second parameter `format` with no default. This may break
  codes that used the fact that the `control` parameter was previously the 
  second one. 
* the `digits` parameter of `draw.covlmc()` has been removed and replaced by a
  similar parameter in `draw_control()`
* defaults for `draw.covlmc()` have changed (issue #66):
  * `p_value` is now `FALSE` by default
  * when `p_value` is `TRUE` the p-values are represented between separators 
    given by the `open_p_value` and `close_p_value` members of the `control`
    list. Defaults are `<` and `>`
  * the intercept is separated from other coefficients by the characters
    specified in the `intercept_sep` member of the `control` list. Defaults
    to `" & "`
  * the characters used to separate levels from models is now `" ~ "`
  * temporal blocks are now separated by default by the `time_sep` member
    of the `control` list. The default value is set to `" | "` and the
    `time_sep` parameter has been removed from `draw.covlmc()`
  * the new parameter `constant_as_prob` is set to `TRUE` which modifies also
    the default display
* the interface of `draw_control()` has been significantly changed (issue #66)
  which should break most customisation code at the level of characters used
  by `draw()` to display a model

## New features
### Discrete time series classes (`dts` and `dts_list`)
In order to ease the introduction of multiple time series support, a single
discrete time series can now be represented by the `dts` class, via the `dts()`
function (see issue #76). All functions that use discrete time series now accept
objects of this class in addition to simple vectors of a supported type
(`integer`, `factor`, `character` and `logical`). This applies to model
estimation functions such as `vlmc()` or `covlmc()`, to model selection functions
(e.g. `tune_vlmc()`) but also to functions that use new data such
`loglikelihood()` and `predict.vlmc()`.

A collection of discrete time series built on the same state set can be 
represented by a `dts_list` object, via the `dts_list()` function (see 
issue #76). A `dts_list` object provides a list like interface with standard
functions such as `length()` and extraction operations. It is created from a 
`list` of objects that can be interpreted as discrete time series. 

### Model representation (with `draw()`)
A major change of `draw()` is the support of multiple output formats. This is
done via a `format` parameter. It supports currently:

* the format of previous versions of `mixvlmc` with `format="text"`. This is
  the default text based representation.
* a new LaTeX export with `format="latex"` based on the LaTeX package `forest` 
  (<https://ctan.org/pkg/forest>) as per issue #66. This can be used to include
  (CO)VLMC models in LaTeX document, for instance when using rmarkdown or quarto.

In addition, text based model representation has been improved (as per 
issue #66) as follows:

* a new global option `mixvlmc.charset` can be used to select the characters
  used by `draw()` when `format="text"` between pure ASCII and UTF-8
* when possible, the text based representation will default to UTF-8 symbols to 
  provide a cleaner display of the context tree
* full customisation of the character set has moved from `draw_control()` to 
  `charset_ascii()` and `charset_utf8()`
* the `draw.covlmc()` function uses arguably better default parameters 
  (described above)
* when a logistic model does not use the covariates, it is now represented 
  by `draw.covlmc()` as a VLMC node, that is with the probability distribution
  on the state space that it represents

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
