# mixvlmc (development version)

# mixvlmc 0.3.1
This release introduces a class for discrete time series, a new
representation of context trees with a LaTeX output integrated with knitr, and
fixes numerous bugs. Several changes break compatibility with previous versions,
in particular some bug fixes change the models obtained with VLMC with
covariates (see the breaking changes below).

## Breaking changes
### Model representation (with `draw()`)
* `draw()` returns a representation of the context tree (an object of class
  `mixvlmc_draw`) instead of the context tree itself. The representation is
  printed automatically, so that `draw(model)` displays the tree as before in
  the console, but not any more when the result is not printed, for instance
  inside a function or a loop (use `print(draw(model))`)
* `draw()` has now a second parameter `format` (`"text"` when it is missing,
  see below). Passing the `control` list as the second argument of `draw()` is
  deprecated (it is still supported with a warning)
* the `digits` parameter of `draw.covlmc()` is deprecated and replaced by a
  similar parameter in `draw_control()`
* `draw()` methods report an error when they are given unused arguments, for
  instance an option of `draw_control()` given directly to `draw()` (they
  were silently ignored)
* defaults for `draw.covlmc()` have changed (issue #66):
  * `p_value` is now `FALSE` by default
  * when `p_value` is `TRUE` the p-values are represented between separators 
    given by the `open_p_value` and `close_p_value` members of the `control`
    list. Defaults are `<` and `>`
  * the intercept is separated from other coefficients by the characters
    specified in the `intercept_sep` member of the `control` list. Defaults
    to `" & "`
  * the characters used to separate levels from models are now `" ~ "`
  * temporal blocks are now separated by default by the `time_sep` member
    of the `control` list. The default value is set to `" | "` and the
    `time_sep` parameter of `draw.covlmc()` is deprecated
  * the new parameter `constant_as_prob` is `TRUE` by default, which also
    modifies the default display
* the interface of `draw_control()` has been significantly changed (issue #66)
  which should break most customisation code at the level of characters used
  by `draw()` to display a model
* with the ASCII charset (`charset_ascii()`), the intermediate children of a
  node are introduced by `+--` like the first one (they were introduced by
  `'--` like the last one)
* in the LaTeX format of `draw.covlmc()`, logical covariates are named with
  their level (`day[TRUE]`), consistently with factors and with the text format
* the counts of `draw.ctx_tree()` (with `frequency = "detailed"`) are
  separated by `", "` as in `draw.vlmc()` (they were separated by `","`)

### Estimation and use of models
* the C++ backend is the default backend of `ctx_tree()`, `vlmc()` and
  `tune_vlmc()` (option `mixvlmc.backend`) and is not considered experimental
  any more. It gives the same models as the R backend, which remains
  available with `backend = "R"` or `options(mixvlmc.backend = "R")`, e.g. to
  process context trees in R. `covlmc()` is not concerned
* the package now depends on R >= 4.1.0 (this was already implied by its
  dependencies)
* the degrees of freedom of the likelihood ratio tests used by `covlmc()` and
  `prune.covlmc()` take into account the number of levels of factor covariates
  (a factor with L levels has L-1 parameters per time step, not 1). This
  changes the models obtained with factors with at least 3 levels
* unused levels of factor covariates are ignored by `covlmc()` (they used to
  change the fitted model)
* the `multinom` engine of `covlmc()` detects rank deficient models (e.g. 
  collinear covariates) consistently with the `glm` engine
* the number of parameters of a `covlmc` (used by AIC and BIC, in particular in
  `tune_covlmc()`) does not depend on the computing engine any more (the
  `multinom` engine, and the `glm` engine with two states, counted the unused
  levels of factor covariates, while the `glm` engine with three states or
  more ignored the states absent from the data of a context). Each context
  counts one intercept per state but one, as for VLMC, and the coefficients of
  the covariates actually estimated by the logistic model, which ignores the
  states that do not follow the context and the unused levels of factors (see
  `loglikelihood.covlmc()`). The likelihood ratio tests still use the nominal
  number of parameters
* the models associated with the root of the context tree of a `covlmc` (depth 0
  models, extended model used for the first observation) use all the
  observations (the first one was ignored)
* `loglikelihood()` returns `-Inf` on new data containing a transition that has
  a zero probability according to the model (the R backend of `vlmc()` ignored
  such transitions)
* `print()` and `summary()` of `tune_vlmc()` and `tune_covlmc()` results report
  the log likelihood used for model selection
* with the C++ backend, `tune_vlmc()` increases `max_depth` as documented 
  (and as with the R backend) and `max_depth = 0` means a depth 0 model (it
  meant no limit)
* with the C++ backend, `simulate()` with `sample = "fast"` (the default) gives
  results that do not depend on the platform any more: for a given seed, they
  differ from the ones of previous versions
* `metrics()` reports an error when it is applied to a node of a VLMC that is
  not a context (it returned `NaN` values), as it does for VLMC with covariates
* methods that do not use their `...` argument now report an error when
  additional arguments are given (issue #64). Misspelled arguments such as
  `simulate(model, 100, burn_in = 10)` or `loglikelihood(model, new_data = x)`
  were silently ignored. This applies also to the `type` argument of
  `contexts()`, which was removed in mixvlmc 0.2.0 and ignored since then

## New features
### Discrete time series class (`dts`)
In order to ease the introduction of multiple time series support, a single
discrete time series can now be represented by the `dts` class, via the `dts()`
function (see issue #76). All functions that use discrete time series now accept
objects of this class in addition to simple vectors of a supported type
(`numeric`, `factor`, `character` and `logical`). This applies to model
estimation functions such as `vlmc()` or `covlmc()`, to model selection functions
(e.g. `tune_vlmc()`) but also to functions that use new data such as
`loglikelihood()` and `predict.vlmc()`.

### Model representation (with `draw()`)
A major change of `draw()` is the support of multiple output formats. This is
done via a `format` parameter. It currently supports:

* the format of previous versions of `mixvlmc` with `format="text"`. This is
  the default text based representation.
* a new LaTeX export with `format="latex"` based on the LaTeX package `forest` 
  (<https://ctan.org/pkg/forest>) as per issue #66. This can be used to include
  (CO)VLMC models in LaTeX documents, for instance when using rmarkdown or quarto.

In addition, the text based model representation has been improved (as per 
issue #66) as follows:

* a new global option `mixvlmc.charset` can be used to select the characters
  used by `draw()` when `format="text"` between pure ASCII and UTF-8
* when possible, the text based representation defaults to UTF-8 symbols to 
  provide a cleaner display of the context tree
* full customisation of the character set has moved from `draw_control()` to 
  `charset_ascii()` and `charset_utf8()`
* the `draw.covlmc()` function uses arguably better default parameters 
  (described above)
* when a logistic model does not use the covariates, it is now represented 
  by `draw.covlmc()` as a VLMC node, that is with the probability distribution
  on the state space that it represents

The result of `draw()` integrates with knitr (issue #77): in an R Markdown or
Quarto document that produces LaTeX (e.g. a PDF document), `draw(model, "latex")`
renders the tree and adds the `forest` package to the preamble of the document.
`format()` and `as.character()` give the representation as a character vector.

## Minor improvements
* `summary()` of a VLMC is much faster with the R backend (about 13 times on
  a model with 9,000 contexts)
* the computations of the C++ backend (construction of the context trees and
  simulation) can be interrupted
* `tune_covlmc()` has a `control` parameter to specify the control
  parameters of the models (see `covlmc_control()`), e.g. `pseudo_obs`
* the progress of `tune_vlmc()` and `tune_covlmc()` (with `verbose > 0`) is
  reported with `message()` (instead of being printed), so that it can be
  silenced with `suppressMessages()`
* `vlmc()`, `cutoff()` and `prune()` are faster with the R backend (about 40%
  for `vlmc()`)
* `covlmc()` and `tune_covlmc()` are faster (about 15%) and the models they
  return use much less memory (about a third), as the ROC curves of the logistic
  models are not computed and stored any more (their AUC is computed directly)
* `cutoff()` applied to a node of a VLMC with covariates returns the p-values
  of the likelihood ratio tests associated with the node (it returned a
  meaningless VLMC cut off)
* `metrics()` supports trimmed VLMC with covariates (it reported an error): the
  accuracy and the confusion matrix are computed from the ones of the
  contexts, while the AUC is `NA` (with a warning)
* a `covlmc` records the computing engine used to estimate it (option
  `mixvlmc.predictive`) and `prune.covlmc()` uses it to estimate new logistic
  models, with a warning if the option has been changed in between (the
  current engine was used, possibly mixing engines in a model)
* `print()` and `summary()` describe a `covlmc` as a "VLMC with covariates"
  (instead of "VLMC with covariate")

## Bug fixes
### Input validation
* `prune.covlmc()` checks that `alpha` is in (0, 1] and warns when it is
  larger than the `alpha` of the model
* `covlmc_control()` and `covlmc()` check that `pseudo_obs` is a positive
  number, and `covlmc()` rejects a covariate data frame without columns
* missing values and empty time series are rejected (they crashed R with the R
  backend), as well as missing values in covariates
* states that do not appear in the time series (e.g. unused levels of a 
  factor) are supported
* `min_size`, `max_depth` and the `ignore` parameter of `loglikelihood()` are
  validated (`min_size = 0` led to an exponential computation time)
* time series with a single state (e.g. a constant series or a factor with a
  single level) are rejected by the estimation functions (they led to a
  degenerate model with the R backend and to an error with the C++ backend)

### VLMC
* the AUC of a context of a VLMC (`metrics()` of a node and
  `contexts(metrics = TRUE)`) is `NA`: as the predictions of a context are
  constant, it was always 0.5 or `NA`
* the time series produced by `simulate()` (for VLMC and VLMC with
  covariates) do not print their class as an attribute any more
* `children()` and `merged_with()` report the states of models fitted on a
  factor (they reported their integer codes when the sequence of the node had
  been given as a character vector, e.g. to `find_sequence()`)
* `tune_vlmc()` and `tune_covlmc()` increase a null `max_depth` instead of
  warning that they cannot find a suitable value
* `contexts()` with `local = TRUE` reports correct positions and supports
  `metrics = TRUE` (R backend)
* positions of the empty context are 0 based in all cases and are not 
  reported any more after `trim()`
* `simulate()` discards the correct number of values during the burn in period
  for depth 0 models (R backend)
* `loglikelihood()` with `ignore` and without `newdata` works with the C++
  backend
* `contexts(frequency = "detailed")` names the frequencies by the states for
  factors with the C++ backend
* the documentation of `loglikelihood()` states correctly that the default
  likelihood function is the truncated one

### VLMC with covariates
* the cut off values given by `cutoff.covlmc()` (with `raw = FALSE`) are
  smaller than the p-values by a relative margin, so that pruning with them
  has an effect even when the p-values recomputed by `prune.covlmc()` differ in
  the last digits from the ones stored in the model, as with some linear
  algebra libraries (e.g. MKL). `tune_covlmc()` could report models identical
  to the previous one and return different results for different values of
  `max_depth`
* `children()` of a node of a VLMC with covariates returns nodes of VLMC with
  covariates (they were nodes of context trees, without the logistic models
  and the merged contexts)
* the coefficients of constant models (contexts followed by a single state)
  with three states or more are expressed with respect to the first state, as
  for the other models: they were expressed with respect to the last state,
  so that `draw()` (with `constant_as_prob = FALSE`) and `contexts()` (with
  `model = "coef"`) reported them under the wrong states
* the logistic models of a `covlmc` do not keep references to the functions
  that fitted them: saved models (e.g. with `saveRDS()`) were several times
  larger than the models themselves, up to 20 times for the models returned
  by `tune_covlmc()`
* the logistic models without covariates (intercept only) obey the
  `mixvlmc.maxit` option with the `glm` and `multinom` engines
* the `mixvlmc.maxit` option is used by `VGAM::vglm()` (three states or more
  with the `glm` engine): VGAM replaced it by its own default (31 iterations),
  and models stopped at this limit could be reported as not converged
* the likelihood of degenerate `multinom` models is not NA any more (which
  crashed `covlmc()`) and NaN likelihoods with zero probabilities are avoided,
  including in ignored observations
* `metrics()` works with constant models and at least 3 states, and per context
  metrics of degenerate `vglm` and `multinom` models are correct
* `metrics()` and `predict()` work on depth 0 models
* `multinom` models with many parameters are supported (`MaxNWts`)
* `cutoff()` ignores correctly intercept only leaves with at least 3 states
* `find_sequence()` handles correctly merged contexts
* `metrics()` on a node which is not a context gives a clear error

### C++ backend
* out of bounds accesses (which led to fake contexts in `ctx_tree()` with
  `min_size = 1` and to crashes in `draw()`) and memory leaks are fixed
* long repeats in the time series do not overflow the stack any more
* pruned models are restored correctly after serialisation (e.g. after 
  `saveRDS()`/`readRDS()`), as well as positions and models obtained with 
  `as_vlmc()` from a context tree. Notice that pruned models saved with
  previous versions are still restored incorrectly
* `is_context()` and local counts are correct for all nodes of a context tree
* the depth of a context tree does not include the end of the time series
* context trees of time series shorter than `min_size` are reduced to their
  root (they caused an error), and `cutoff()` works on such models
* `trim()` removes positions from context trees
* `simulate()` uses `base::sample.int()` even if another function with the same
  name is defined in the global environment

### Documentation
* the bootstrap example of `vignette("sampling")` used no burn in period, as
  the `burnin` argument of `simulate()` was misspelled
* the vignettes and the README have been proofread: in particular, the text of
  `vignette("prediction")` and `vignette("sampling")` now agrees with the
  models they select, and `vignette("variable-length-markov-chains")` gives the
  correct number of parameters of a Markov chain of order m

# mixvlmc 0.2.2
This is a bug fix release (see issue #79 for the main bugs):

* fix a bug in the C++ suffix tree implementation (subscript out of bounds in
  rare cases)
* avoid NaN production in `loglikelihood()` and related functions for the 
  `multinom` predictive engine
* handle situations when a merged or local model has more degrees of freedom
  that the models it is supposed to replace (in `covlmc`)
* improve the interaction with the `VGAM` package in corner cases with 
  degenerate models

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
