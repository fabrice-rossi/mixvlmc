# Changelog

## mixvlmc 0.3.0

This release introduces a class for discrete time series, a new
representation of context trees with a LaTeX output integrated with
knitr, and fixes numerous bugs. Several changes break compatibility with
previous versions, in particular some bug fixes change the models
obtained with VLMC with covariates (see the breaking changes below).

### Breaking changes

#### Model representation (with `draw()`)

- [`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
  returns a representation of the context tree (an object of class
  `mixvlmc_draw`) instead of the context tree itself. The representation
  is printed automatically, so that `draw(model)` displays the tree as
  before in the console, but not any more when the result is not
  printed, for instance inside a function or a loop (use
  `print(draw(model))`)
- [`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
  has now a second parameter `format` (`"text"` when it is missing, see
  below). Passing the `control` list as the second argument of
  [`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
  is deprecated (it is still supported with a warning)
- the `digits` parameter of
  [`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md)
  is deprecated and replaced by a similar parameter in
  [`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw_control.md)
- [`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
  methods report an error when they are given unused arguments, for
  instance an option of
  [`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw_control.md)
  given directly to
  [`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
  (they were silently ignored)
- defaults for
  [`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md)
  have changed (issue
  [\#66](https://github.com/fabrice-rossi/mixvlmc/issues/66)):
  - `p_value` is now `FALSE` by default
  - when `p_value` is `TRUE` the p-values are represented between
    separators given by the `open_p_value` and `close_p_value` members
    of the `control` list. Defaults are `<` and `>`
  - the intercept is separated from other coefficients by the characters
    specified in the `intercept_sep` member of the `control` list.
    Defaults to `" & "`
  - the characters used to separate levels from models are now `" ~ "`
  - temporal blocks are now separated by default by the `time_sep`
    member of the `control` list. The default value is set to `" | "`
    and the `time_sep` parameter of
    [`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md)
    is deprecated
  - the new parameter `constant_as_prob` is `TRUE` by default, which
    also modifies the default display
- the interface of
  [`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw_control.md)
  has been significantly changed (issue
  [\#66](https://github.com/fabrice-rossi/mixvlmc/issues/66)) which
  should break most customisation code at the level of characters used
  by
  [`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
  to display a model
- with the ASCII charset
  ([`charset_ascii()`](https://fabrice-rossi.github.io/mixvlmc/reference/charset_ascii.md)),
  the intermediate children of a node are introduced by `+--` like the
  first one (they were introduced by `'--` like the last one)
- in the LaTeX format of
  [`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md),
  logical covariates are named with their level (`day[TRUE]`),
  consistently with factors and with the text format
- the counts of
  [`draw.ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.ctx_tree.md)
  (with `frequency = "detailed"`) are separated by `", "` as in
  [`draw.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.vlmc.md)
  (they were separated by `","`)

#### Estimation and use of models

- the C++ backend is the default backend of
  [`ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/reference/ctx_tree.md),
  [`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/vlmc.md)
  and
  [`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_vlmc.md)
  (option `mixvlmc.backend`) and is not considered experimental any
  more. It gives the same models as the R backend, which remains
  available with `backend = "R"` or `options(mixvlmc.backend = "R")`,
  e.g. to process context trees in R.
  [`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
  is not concerned
- the package now depends on R \>= 4.1.0 (this was already implied by
  its dependencies)
- the degrees of freedom of the likelihood ratio tests used by
  [`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
  and
  [`prune.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/prune.covlmc.md)
  take into account the number of levels of factor covariates (a factor
  with L levels has L-1 parameters per time step, not 1). This changes
  the models obtained with factors with at least 3 levels
- unused levels of factor covariates are ignored by
  [`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
  (they used to change the fitted model)
- the `multinom` engine of
  [`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
  detects rank deficient models (e.g.  collinear covariates)
  consistently with the `glm` engine
- the number of parameters of a `covlmc` (used by AIC and BIC, in
  particular in
  [`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md))
  does not depend on the computing engine any more (the `multinom`
  engine, and the `glm` engine with two states, counted the unused
  levels of factor covariates, while the `glm` engine with three states
  or more ignored the states absent from the data of a context). Each
  context counts one intercept per state but one, as for VLMC, and the
  coefficients of the covariates actually estimated by the logistic
  model, which ignores the states that do not follow the context and the
  unused levels of factors (see
  [`loglikelihood.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/loglikelihood.covlmc.md)).
  The likelihood ratio tests still use the nominal number of parameters
- the models associated with the root of the context tree of a `covlmc`
  (depth 0 models, extended model used for the first observation) use
  all the observations (the first one was ignored)
- [`loglikelihood()`](https://fabrice-rossi.github.io/mixvlmc/reference/loglikelihood.md)
  returns `-Inf` on new data containing a transition that has a zero
  probability according to the model (the R backend of
  [`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/vlmc.md)
  ignored such transitions)
- [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) of
  [`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_vlmc.md)
  and
  [`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)
  results report the log likelihood used for model selection
- with the C++ backend,
  [`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_vlmc.md)
  increases `max_depth` as documented (and as with the R backend) and
  `max_depth = 0` means a depth 0 model (it meant no limit)
- with the C++ backend,
  [`simulate()`](https://rdrr.io/r/stats/simulate.html) with
  `sample = "fast"` (the default) gives results that do not depend on
  the platform any more: for a given seed, they differ from the ones of
  previous versions
- [`metrics()`](https://fabrice-rossi.github.io/mixvlmc/reference/metrics.md)
  reports an error when it is applied to a node of a VLMC that is not a
  context (it returned `NaN` values), as it does for VLMC with
  covariates
- methods that do not use their `...` argument now report an error when
  additional arguments are given (issue
  [\#64](https://github.com/fabrice-rossi/mixvlmc/issues/64)).
  Misspelled arguments such as `simulate(model, 100, burn_in = 10)` or
  `loglikelihood(model, new_data = x)` were silently ignored. This
  applies also to the `type` argument of
  [`contexts()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.md),
  which was removed in mixvlmc 0.2.0 and ignored since then

### New features

#### Discrete time series class (`dts`)

In order to ease the introduction of multiple time series support, a
single discrete time series can now be represented by the `dts` class,
via the
[`dts()`](https://fabrice-rossi.github.io/mixvlmc/reference/dts.md)
function (see issue
[\#76](https://github.com/fabrice-rossi/mixvlmc/issues/76)). All
functions that use discrete time series now accept objects of this class
in addition to simple vectors of a supported type (`numeric`, `factor`,
`character` and `logical`). This applies to model estimation functions
such as
[`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/vlmc.md) or
[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md),
to model selection functions
(e.g. [`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_vlmc.md))
but also to functions that use new data such as
[`loglikelihood()`](https://fabrice-rossi.github.io/mixvlmc/reference/loglikelihood.md)
and
[`predict.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/predict.vlmc.md).

#### Model representation (with `draw()`)

A major change of
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md) is
the support of multiple output formats. This is done via a `format`
parameter. It currently supports:

- the format of previous versions of `mixvlmc` with `format="text"`.
  This is the default text based representation.
- a new LaTeX export with `format="latex"` based on the LaTeX package
  `forest` (<https://ctan.org/pkg/forest>) as per issue
  [\#66](https://github.com/fabrice-rossi/mixvlmc/issues/66). This can
  be used to include (CO)VLMC models in LaTeX documents, for instance
  when using rmarkdown or quarto.

In addition, the text based model representation has been improved (as
per issue [\#66](https://github.com/fabrice-rossi/mixvlmc/issues/66)) as
follows:

- a new global option `mixvlmc.charset` can be used to select the
  characters used by
  [`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
  when `format="text"` between pure ASCII and UTF-8
- when possible, the text based representation defaults to UTF-8 symbols
  to provide a cleaner display of the context tree
- full customisation of the character set has moved from
  [`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw_control.md)
  to
  [`charset_ascii()`](https://fabrice-rossi.github.io/mixvlmc/reference/charset_ascii.md)
  and
  [`charset_utf8()`](https://fabrice-rossi.github.io/mixvlmc/reference/charset_utf8.md)
- the
  [`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md)
  function uses arguably better default parameters (described above)
- when a logistic model does not use the covariates, it is now
  represented by
  [`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md)
  as a VLMC node, that is with the probability distribution on the state
  space that it represents

The result of
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
integrates with knitr (issue
[\#77](https://github.com/fabrice-rossi/mixvlmc/issues/77)): in an R
Markdown or Quarto document that produces LaTeX (e.g. a PDF document),
`draw(model, "latex")` renders the tree and adds the `forest` package to
the preamble of the document.
[`format()`](https://rdrr.io/r/base/format.html) and
[`as.character()`](https://rdrr.io/r/base/character.html) give the
representation as a character vector.

### Minor improvements

- [`summary()`](https://rdrr.io/r/base/summary.html) of a VLMC is much
  faster with the R backend (about 13 times on a model with 9,000
  contexts)
- the computations of the C++ backend (construction of the context trees
  and simulation) can be interrupted
- [`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)
  has a `control` parameter to specify the control parameters of the
  models (see
  [`covlmc_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc_control.md)),
  e.g. `pseudo_obs`
- the progress of
  [`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_vlmc.md)
  and
  [`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)
  (with `verbose > 0`) is reported with
  [`message()`](https://rdrr.io/r/base/message.html) (instead of being
  printed), so that it can be silenced with
  [`suppressMessages()`](https://rdrr.io/r/base/message.html)
- [`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/vlmc.md),
  [`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/reference/cutoff.md)
  and
  [`prune()`](https://fabrice-rossi.github.io/mixvlmc/reference/prune.md)
  are faster with the R backend (about 40% for
  [`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/vlmc.md))
- [`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
  and
  [`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)
  are faster (about 15%) and the models they return use much less memory
  (about a third), as the ROC curves of the logistic models are not
  computed and stored any more (their AUC is computed directly)
- [`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/reference/cutoff.md)
  applied to a node of a VLMC with covariates returns the p-values of
  the likelihood ratio tests associated with the node (it returned a
  meaningless VLMC cut off)
- [`metrics()`](https://fabrice-rossi.github.io/mixvlmc/reference/metrics.md)
  supports trimmed VLMC with covariates (it reported an error): the
  accuracy and the confusion matrix are computed from the ones of the
  contexts, while the AUC is `NA` (with a warning)
- a `covlmc` records the computing engine used to estimate it (option
  `mixvlmc.predictive`) and
  [`prune.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/prune.covlmc.md)
  uses it to estimate new logistic models, with a warning if the option
  has been changed in between (the current engine was used, possibly
  mixing engines in a model)
- [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) describe a `covlmc`
  as a “VLMC with covariates” (instead of “VLMC with covariate”)

### Bug fixes

#### Input validation

- [`prune.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/prune.covlmc.md)
  checks that `alpha` is in (0, 1\] and warns when it is larger than the
  `alpha` of the model
- [`covlmc_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc_control.md)
  and
  [`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
  check that `pseudo_obs` is a positive number, and
  [`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
  rejects a covariate data frame without columns
- missing values and empty time series are rejected (they crashed R with
  the R backend), as well as missing values in covariates
- states that do not appear in the time series (e.g. unused levels of a
  factor) are supported
- `min_size`, `max_depth` and the `ignore` parameter of
  [`loglikelihood()`](https://fabrice-rossi.github.io/mixvlmc/reference/loglikelihood.md)
  are validated (`min_size = 0` led to an exponential computation time)
- time series with a single state (e.g. a constant series or a factor
  with a single level) are rejected by the estimation functions (they
  led to a degenerate model with the R backend and to an error with the
  C++ backend)

#### VLMC

- the AUC of a context of a VLMC
  ([`metrics()`](https://fabrice-rossi.github.io/mixvlmc/reference/metrics.md)
  of a node and `contexts(metrics = TRUE)`) is `NA`: as the predictions
  of a context are constant, it was always 0.5 or `NA`
- the time series produced by
  [`simulate()`](https://rdrr.io/r/stats/simulate.html) (for VLMC and
  VLMC with covariates) do not print their class as an attribute any
  more
- [`children()`](https://fabrice-rossi.github.io/mixvlmc/reference/children.md)
  and
  [`merged_with()`](https://fabrice-rossi.github.io/mixvlmc/reference/merged_with.md)
  report the states of models fitted on a factor (they reported their
  integer codes when the sequence of the node had been given as a
  character vector, e.g. to
  [`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.md))
- [`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_vlmc.md)
  and
  [`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)
  increase a null `max_depth` instead of warning that they cannot find a
  suitable value
- [`contexts()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.md)
  with `local = TRUE` reports correct positions and supports
  `metrics = TRUE` (R backend)
- positions of the empty context are 0 based in all cases and are not
  reported any more after
  [`trim()`](https://fabrice-rossi.github.io/mixvlmc/reference/trim.md)
- [`simulate()`](https://rdrr.io/r/stats/simulate.html) discards the
  correct number of values during the burn in period for depth 0 models
  (R backend)
- [`loglikelihood()`](https://fabrice-rossi.github.io/mixvlmc/reference/loglikelihood.md)
  with `ignore` and without `newdata` works with the C++ backend
- `contexts(frequency = "detailed")` names the frequencies by the states
  for factors with the C++ backend
- the documentation of
  [`loglikelihood()`](https://fabrice-rossi.github.io/mixvlmc/reference/loglikelihood.md)
  states correctly that the default likelihood function is the truncated
  one

#### VLMC with covariates

- [`children()`](https://fabrice-rossi.github.io/mixvlmc/reference/children.md)
  of a node of a VLMC with covariates returns nodes of VLMC with
  covariates (they were nodes of context trees, without the logistic
  models and the merged contexts)
- the coefficients of constant models (contexts followed by a single
  state) with three states or more are expressed with respect to the
  first state, as for the other models: they were expressed with respect
  to the last state, so that
  [`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
  (with `constant_as_prob = FALSE`) and
  [`contexts()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.md)
  (with `model = "coef"`) reported them under the wrong states
- the logistic models of a `covlmc` do not keep references to the
  functions that fitted them: saved models (e.g. with
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html)) were several times
  larger than the models themselves, up to 20 times for the models
  returned by
  [`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)
- the logistic models without covariates (intercept only) obey the
  `mixvlmc.maxit` option with the `glm` and `multinom` engines
- the `mixvlmc.maxit` option is used by
  [`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) (three states
  or more with the `glm` engine): VGAM replaced it by its own default
  (31 iterations), and models stopped at this limit could be reported as
  not converged
- the likelihood of degenerate `multinom` models is not NA any more
  (which crashed
  [`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md))
  and NaN likelihoods with zero probabilities are avoided, including in
  ignored observations
- [`metrics()`](https://fabrice-rossi.github.io/mixvlmc/reference/metrics.md)
  works with constant models and at least 3 states, and per context
  metrics of degenerate `vglm` and `multinom` models are correct
- [`metrics()`](https://fabrice-rossi.github.io/mixvlmc/reference/metrics.md)
  and [`predict()`](https://rdrr.io/r/stats/predict.html) work on depth
  0 models
- `multinom` models with many parameters are supported (`MaxNWts`)
- [`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/reference/cutoff.md)
  ignores correctly intercept only leaves with at least 3 states
- [`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.md)
  handles correctly merged contexts
- [`metrics()`](https://fabrice-rossi.github.io/mixvlmc/reference/metrics.md)
  on a node which is not a context gives a clear error

#### C++ backend

- out of bounds accesses (which led to fake contexts in
  [`ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/reference/ctx_tree.md)
  with `min_size = 1` and to crashes in
  [`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md))
  and memory leaks are fixed
- long repeats in the time series do not overflow the stack any more
- pruned models are restored correctly after serialisation (e.g. after
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html)/[`readRDS()`](https://rdrr.io/r/base/readRDS.html)),
  as well as positions and models obtained with
  [`as_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/as_vlmc.md)
  from a context tree. Notice that pruned models saved with previous
  versions are still restored incorrectly
- [`is_context()`](https://fabrice-rossi.github.io/mixvlmc/reference/is_context.md)
  and local counts are correct for all nodes of a context tree
- the depth of a context tree does not include the end of the time
  series
- context trees of time series shorter than `min_size` are reduced to
  their root (they caused an error), and
  [`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/reference/cutoff.md)
  works on such models
- [`trim()`](https://fabrice-rossi.github.io/mixvlmc/reference/trim.md)
  removes positions from context trees
- [`simulate()`](https://rdrr.io/r/stats/simulate.html) uses
  [`base::sample.int()`](https://rdrr.io/r/base/sample.html) even if
  another function with the same name is defined in the global
  environment

#### Documentation

- the bootstrap example of
  [`vignette("sampling")`](https://fabrice-rossi.github.io/mixvlmc/articles/sampling.md)
  used no burn in period, as the `burnin` argument of
  [`simulate()`](https://rdrr.io/r/stats/simulate.html) was misspelled
- the vignettes and the README have been proofread: in particular, the
  text of
  [`vignette("prediction")`](https://fabrice-rossi.github.io/mixvlmc/articles/prediction.md)
  and
  [`vignette("sampling")`](https://fabrice-rossi.github.io/mixvlmc/articles/sampling.md)
  now agrees with the models they select, and
  [`vignette("variable-length-markov-chains")`](https://fabrice-rossi.github.io/mixvlmc/articles/variable-length-markov-chains.md)
  gives the correct number of parameters of a Markov chain of order m

## mixvlmc 0.2.2

CRAN release: 2025-05-26

This is a bug fix release (see issue
[\#79](https://github.com/fabrice-rossi/mixvlmc/issues/79) for the main
bugs):

- fix a bug in the C++ suffix tree implementation (subscript out of
  bounds in rare cases)
- avoid NaN production in
  [`loglikelihood()`](https://fabrice-rossi.github.io/mixvlmc/reference/loglikelihood.md)
  and related functions for the `multinom` predictive engine
- handle situations when a merged or local model has more degrees of
  freedom that the models it is supposed to replace (in `covlmc`)
- improve the interaction with the `VGAM` package in corner cases with
  degenerate models

## mixvlmc 0.2.1

CRAN release: 2023-10-31

This release has no visible changes and fixes internal issues:

- missing package in Suggests
- double definition of a C++ internal function

## mixvlmc 0.2.0

CRAN release: 2023-10-28

### Breaking changes

- the `mode` parameter of
  [`cutoff.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/cutoff.vlmc.md)
  has been renamed to `scale`
- the `type` parameter has been removed from
  [`contexts()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.md)
  variants and the default result format has been modified significantly
- the `counts` parameter from
  [`contexts.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.vlmc.md)
  and
  [`contexts.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.covlmc.md)
  has been replaced by a logical parameter named `local`
- [`contexts()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.md)
  sequences are now reported by default in temporal order

### New features

#### C++ backend

The main major new feature of this version is the inclusion of a C++
implementation of context tree and VLMC construction. A new option
`mixvlmc.backend` can be used to switch globally from the original `"R"`
implementation to the new `"C++"` back end. A new `backend` parameter
has been added to
[`ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/reference/ctx_tree.md),
[`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/vlmc.md)
and
[`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_vlmc.md)
to enable local back end selection.

The C++ implementation is significantly faster than the R
implementation, at least by a factor 10. While it has been thoroughly
tested, it is still considered experimental notably because it does not
apply to COVLMC (setting the global option to `"C++"` has not effect on
COVLMC model construction). For context trees and VLMC, results should
not depend on the back end, at least within numerical precision. The
only notable difference is the ordering of the contexts which differs
between back ends: in a call to
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.md),
the first context for the R back end will generally not be the first
context for the C++ back end.

#### Other new features

- [`contexts()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.md)
  can now report the positions of each context in the original time
  series
- nodes of context trees can be extracted individually as `ctx_node`
  objects using the
  [`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.md)
  function. A collection of new functions can be used to manipulate the
  nodes and gain fine grain information on the corresponding sequences
  (issue [\#50](https://github.com/fabrice-rossi/mixvlmc/issues/50)).
- by default,
  [`contexts()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.md)
  reports now contexts as a list of `ctx_node` objects
- new functions
  [`predict.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/predict.vlmc.md)
  and
  [`predict.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/predict.covlmc.md)
  can be used to make one step ahead predictions of a time series based
  on a (CO)VLMC model (issue
  [\#46](https://github.com/fabrice-rossi/mixvlmc/issues/46)). Those
  function are documented in a new vignette
  ([`vignette("prediction")`](https://fabrice-rossi.github.io/mixvlmc/articles/prediction.md))
- the log likelihood calculations performed by
  [`logLik.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/logLik.vlmc.md),
  [`logLik.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/logLik.covlmc.md)
  [`loglikelihood()`](https://fabrice-rossi.github.io/mixvlmc/reference/loglikelihood.md)
  and
  [`loglikelihood.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/loglikelihood.covlmc.md)
  have been revised, expanded to include three possible definitions of
  the likelihood function, and documented in a new vignette
  ([`vignette("likelihood")`](https://fabrice-rossi.github.io/mixvlmc/articles/likelihood.md))
- [`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_vlmc.md)
  and
  [`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)
  can be used with the different likelihood function definitions
- results of
  [`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_vlmc.md)
  and
  [`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)
  can be plotted using base R graphics or ggplot2 (issue
  [\#36](https://github.com/fabrice-rossi/mixvlmc/issues/36))
- [`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)
  can trim the best model (and the initial one) if asked to  
- [`trim.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/trim.covlmc.md)
  implements simple trimming for VGAM based objects (issue
  [\#48](https://github.com/fabrice-rossi/mixvlmc/issues/48))
- [`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/reference/cutoff.md)
  uses a new `tolerance` parameter to avoid reporting cut off values
  that are almost identical due to numerical imprecision
- [`simulate.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/simulate.vlmc.md)
  implements a user specified burn in period (issue
  [\#40](https://github.com/fabrice-rossi/mixvlmc/issues/40))
- [`simulate.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/simulate.vlmc.md)
  and
  [`simulate.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/simulate.covlmc.md)
  now handle the random generator state as does
  [`stats::simulate()`](https://rdrr.io/r/stats/simulate.html) (issue
  [\#56](https://github.com/fabrice-rossi/mixvlmc/issues/56))

### Minor improvements and bug fixes

- Improved the test coverage (issue
  [\#54](https://github.com/fabrice-rossi/mixvlmc/issues/54))
- Fixed several bugs in internal sampling methods that made
  [`simulate.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/simulate.covlmc.md)
  unreliable for state spaces with three or more states
- Fixed a bug in
  [`simulate.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/simulate.covlmc.md)
  that occurred in contexts with a longer self memory compared to their
  covariate memory
- Fixed several bugs in
  [`metrics.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/metrics.vlmc.md)
  and align the results with the ones obtained by using direct
  calculation on the results of
  [`predict.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/predict.vlmc.md)
- Fixed several bugs related to degenerate VLMC (issue
  [\#55](https://github.com/fabrice-rossi/mixvlmc/issues/55))
- Fixed a bug in the likelihood calculation for internal context nodes
- Improved type consistency by reporting counts as integers in
  [`contexts()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.md)
  results
- Fixed documentation: all models can be adjusted on time series of
  logical values
- Synchronized
  [`ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/reference/ctx_tree.md)
  documentation and its default value
- Synchronized
  [`contexts()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.md)
  documentation and its default value
- Added more trimming for COVLMC models in
  [`trim.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/trim.covlmc.md)

## mixvlmc 0.1.1

CRAN release: 2023-07-10

- Added missing documentation about the return values of
  [`simulate.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/simulate.vlmc.md)
  and
  [`simulate.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/simulate.covlmc.md)

## mixvlmc 0.1.0

- Added a `NEWS.md` file to track changes to the package.
