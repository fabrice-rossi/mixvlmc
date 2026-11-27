# Package index

## Discrete time series

Build specialized representation of discrete time series.

- [`dts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/dts.md)
  : Convert a vector to a discrete time series
- [`is_dts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/is_dts.md)
  : Test if the object is a discrete time series
- [`dts_data()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/dts_data.md)
  : Extract the plain discrete time series from a dts object
- [`print(`*`<dts>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/print.dts.md)
  : Print a discrete time series
- [`` `[`( ``*`<dts>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/dts-operations.md)
  [`` `[[`( ``*`<dts>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/dts-operations.md)
  [`length(`*`<dts>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/dts-operations.md)
  [`rev(`*`<dts>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/dts-operations.md)
  : Basic operations on discrete time series
- [`states()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/states.md)
  : State space of an object

## Context trees

Build context trees and extract information from them.

- [`ctx_tree(`*`<default>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/ctx_tree.default.md)
  : Build a context tree for a discrete time series

- [`ctx_tree(`*`<dts>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/ctx_tree.dts.md)
  : Build a context tree for a discrete time series

- [`context_number()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/context_number.md)
  : Number of contexts of a context tree

- [`contexts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.md)
  : Contexts of a context tree

- [`contexts(`*`<ctx_tree>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.ctx_tree.md)
  [`contexts(`*`<ctx_tree_cpp>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.ctx_tree.md)
  : Contexts of a context tree

- [`depth()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/depth.md)
  : Depth of a context tree

- [`draw(`*`<ctx_tree_cpp>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.ctx_tree.md)
  [`draw(`*`<ctx_tree>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.ctx_tree.md)
  : Text based representation of a context tree

- [`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw_control.md)
  :

  Control parameters for
  [`draw()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.md)

- [`print(`*`<contexts>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/print.contexts.md)
  : Print a context list

- [`states()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/states.md)
  : State space of an object

- [`summary(`*`<ctx_tree>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/summary.ctx_tree.md)
  [`print(`*`<summary.ctx_tree>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/summary.ctx_tree.md)
  [`summary(`*`<ctx_tree_cpp>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/summary.ctx_tree.md)
  [`print(`*`<summary.ctx_tree_cpp>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/summary.ctx_tree.md)
  : Summary of a context tree

## Context tree nodes

Extract nodes from context trees and manipulate them.

- [`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.md)
  : Find the node of a sequence in a context tree
- [`find_sequence(`*`<covlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.covlmc.md)
  : Find the node of a sequence in a COVLMC context tree
- [`as_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/as_sequence.md)
  : Extract the sequence encoded by a node
- [`length(`*`<ctx_node>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/length.ctx_node.md)
  : Length of the sequence of a node
- [`children()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/children.md)
  : Find the children nodes of a node in a context tree
- [`covariate_memory()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/covariate_memory.md)
  : Covariate memory length for a COVLMC context
- [`counts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/counts.md)
  : Report the distribution of values that follow occurrences of a
  sequence
- [`cutoff(`*`<ctx_node>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.ctx_node.md)
  : Cut off value for pruning a node in the context tree of a VLMC
- [`cutoff(`*`<ctx_node_covlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.ctx_node_covlmc.md)
  : Cut off values for pruning a node in the context tree of a COVLMC
- [`is_context()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/is_context.md)
  : Report the nature of a node in a context tree
- [`is_merged()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/is_merged.md)
  : Merging status of a COVLMC context
- [`is_reversed()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/is_reversed.md)
  : Report the ordering convention of the node
- [`merged_with()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/merged_with.md)
  : Merged contexts in a COVLMC
- [`metrics(`*`<ctx_node>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/metrics.ctx_node.md)
  : Predictive quality metrics for a node of a context tree
- [`metrics(`*`<ctx_node_covlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/metrics.ctx_node_covlmc.md)
  : Predictive quality metrics for a node of a COVLMC context tree
- [`model()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/model.md)
  : Logistic model of a COVLMC context
- [`parent()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/parent.md)
  : Find the parent of a node in a context tree
- [`positions()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/positions.md)
  : Report the positions of a sequence associated with a node
- [`rev(`*`<ctx_node>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/rev.ctx_node.md)
  : Reverse Sequence

## Variable Length Markov Chain (VLMC)

Estimate VLMC from a time series and extract information from them.

- [`vlmc(`*`<default>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/vlmc.default.md)
  : Fit a Variable Length Markov Chain (VLMC)

- [`vlmc(`*`<dts>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/vlmc.dts.md)
  : Fit a Variable Length Markov Chain (VLMC)

- [`context_number()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/context_number.md)
  : Number of contexts of a context tree

- [`contexts(`*`<vlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.vlmc.md)
  [`contexts(`*`<vlmc_cpp>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.vlmc.md)
  : Contexts of a VLMC

- [`depth()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/depth.md)
  : Depth of a context tree

- [`draw(`*`<vlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.vlmc.md)
  [`draw(`*`<vlmc_cpp>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.vlmc.md)
  : Text based representation of a VLMC

- [`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw_control.md)
  :

  Control parameters for
  [`draw()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.md)

- [`logLik(`*`<vlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/logLik.vlmc.md)
  [`logLik(`*`<vlmc_cpp>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/logLik.vlmc.md)
  : Log-Likelihood of a VLMC

- [`loglikelihood()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/loglikelihood.md)
  : Log-Likelihood of a VLMC

- [`metrics(`*`<vlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/metrics.vlmc.md)
  [`print(`*`<metrics.vlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/metrics.vlmc.md)
  : Predictive quality metrics for VLMC

- [`predict(`*`<vlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/predict.vlmc.md)
  [`predict(`*`<vlmc_cpp>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/predict.vlmc.md)
  : Next state prediction in a discrete time series for a VLMC

- [`states()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/states.md)
  : State space of an object

- [`summary(`*`<vlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/summary.vlmc.md)
  [`print(`*`<summary.vlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/summary.vlmc.md)
  [`summary(`*`<vlmc_cpp>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/summary.vlmc.md)
  [`print(`*`<summary.vlmc_cpp>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/summary.vlmc.md)
  : Summary of a VLMC

## Variable Length Markov Chain with covariates (COVLMC)

Estimate COVLMC from a time series and covariates. Extract information
from them.

- [`covlmc(`*`<default>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/covlmc.default.md)
  : Fit a Variable Length Markov Chain with Covariates (COVLMC)

- [`covlmc(`*`<dts>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/covlmc.dts.md)
  : Fit a Variable Length Markov Chain with Covariates (COVLMC)

- [`covlmc_control()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/covlmc_control.md)
  : Control for COVLMC fitting

- [`context_number(`*`<covlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/context_number.covlmc.md)
  : Number of contexts of a VLMC with covariates

- [`contexts(`*`<covlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.covlmc.md)
  : Contexts of a VLMC with covariates

- [`covariate_depth()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/covariate_depth.md)
  : Maximal covariate memory of a VLMC with covariates

- [`depth()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/depth.md)
  : Depth of a context tree

- [`draw(`*`<covlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.covlmc.md)
  : Text based representation of a VLMC with covariates

- [`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw_control.md)
  :

  Control parameters for
  [`draw()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.md)

- [`logLik(`*`<covlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/logLik.covlmc.md)
  : Log-Likelihood of a VLMC with covariates

- [`loglikelihood(`*`<covlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/loglikelihood.covlmc.md)
  : Log-Likelihood of a VLMC with covariates

- [`metrics(`*`<covlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/metrics.covlmc.md)
  [`print(`*`<metrics.covlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/metrics.covlmc.md)
  : Predictive quality metrics for VLMC with covariates

- [`predict(`*`<covlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/predict.covlmc.md)
  : Next state prediction in a discrete time series for a VLMC with
  covariates

- [`states()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/states.md)
  : State space of an object

- [`summary(`*`<covlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/summary.covlmc.md)
  [`print(`*`<summary.covlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/summary.covlmc.md)
  : Summary of a VLMC with covariates

## Model selection for (CO)VLMC

Functions to adjust automatically or manually the complexity of a
(CO)VLMC to the time series.

### VLMC

- [`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_vlmc.md)
  : Fit an optimal Variable Length Markov Chain (VLMC)
- [`plot(`*`<tune_vlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/plot.tune_vlmc.md)
  [`plot(`*`<tune_covlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/plot.tune_vlmc.md)
  : Plot the results of automatic (CO)VLMC complexity selection
- [`autoplot(`*`<tune_vlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/autoplot.tune_vlmc.md)
  : Create a complete ggplot for the results of automatic VLMC
  complexity selection
- [`summary(`*`<tune_vlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/summary.tune_vlmc.md)
  [`print(`*`<summary.tune_vlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/summary.tune_vlmc.md)
  : Summary of the model selection of a VLMC
- [`cutoff(`*`<vlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.vlmc.md)
  [`cutoff(`*`<vlmc_cpp>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.vlmc.md)
  : Cut off values for pruning the context tree of a VLMC
- [`prune()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/prune.md)
  : Prune a Variable Length Markov Chain (VLMC)

### COVLMC

- [`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_covlmc.md)
  : Fit an optimal Variable Length Markov Chain with Covariates (COVLMC)
- [`plot(`*`<tune_vlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/plot.tune_vlmc.md)
  [`plot(`*`<tune_covlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/plot.tune_vlmc.md)
  : Plot the results of automatic (CO)VLMC complexity selection
- [`autoplot(`*`<tune_covlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/autoplot.tune_covlmc.md)
  : Create a complete ggplot for the results of automatic COVLMC
  complexity selection
- [`summary(`*`<tune_covlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/summary.tune_covlmc.md)
  [`print(`*`<summary.tune_covlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/summary.tune_covlmc.md)
  : Summary of the model selection of a VLMC with covariates
- [`cutoff(`*`<covlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.covlmc.md)
  : Cut off values for pruning the context tree of a VLMC with
  covariates
- [`prune(`*`<covlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/prune.covlmc.md)
  : Prune a Variable Length Markov Chain with covariates

## Simulation

Functions to simulate new time series from a (CO)VLMC model

- [`simulate(`*`<covlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/simulate.covlmc.md)
  : Simulate a discrete time series for a VLMC with covariates
- [`simulate(`*`<vlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/simulate.vlmc.md)
  : Simulate a discrete time series for a VLMC
- [`simulate(`*`<vlmc_cpp>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/simulate.vlmc_cpp.md)
  : Simulate a discrete time series for a VLMC

## Global options

- [`mixvlmc`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/mixvlmc-package.md)
  [`mixvlmc-package`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/mixvlmc-package.md)
  : mixvlmc: Variable Length Markov Chains with Covariates

## Data sets

- [`globalearthquake`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/globalearthquake.md)
  : Significant Earthquake Dataset
- [`powerconsumption`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/powerconsumption.md)
  : Individual household electric power consumption

## Conversions and other low level manipulation of models

- [`as_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/as_covlmc.md)
  : Convert an object to a Variable Length Markov Chain with covariates
  (COVLMC)
- [`as_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/as_sequence.md)
  : Extract the sequence encoded by a node
- [`as_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/as_vlmc.md)
  : Convert an object to a Variable Length Markov Chain (VLMC)
- [`as_vlmc(`*`<ctx_tree_cpp>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/as_vlmc.ctx_tree_cpp.md)
  : Convert an object to a Variable Length Markov Chain (VLMC)
- [`is_context()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/is_context.md)
  : Report the nature of a node in a context tree
- [`is_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/is_covlmc.md)
  : Test if the object is a VLMC with covariates
- [`is_ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/is_ctx_tree.md)
  : Test if the object is a context tree
- [`is_dts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/is_dts.md)
  : Test if the object is a discrete time series
- [`is_merged()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/is_merged.md)
  : Merging status of a COVLMC context
- [`is_reversed()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/is_reversed.md)
  : Report the ordering convention of the node
- [`is_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/is_vlmc.md)
  : Test if the object is a VLMC model
- [`trim()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/trim.md)
  : Trim a context tree
- [`trim(`*`<covlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/trim.covlmc.md)
  : Trim a COVLMC
- [`trim(`*`<vlmc>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/trim.vlmc.md)
  : Trim a VLMC
- [`trim(`*`<vlmc_cpp>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/trim.vlmc_cpp.md)
  : Trim a VLMC

## Low level customisation

- [`charset_utf8()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/charset_utf8.md)
  : UTF-8 character set for context tree text representation
- [`charset_ascii()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/charset_ascii.md)
  : ASCII character set for context tree text representation
- [`print(`*`<mixvlmc_draw>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/print.mixvlmc_draw.md)
  [`format(`*`<mixvlmc_draw>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/print.mixvlmc_draw.md)
  [`as.character(`*`<mixvlmc_draw>`*`)`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/print.mixvlmc_draw.md)
  : Print and convert the representation of a context tree

## Generics

Generic S3 methods defined by the package

- [`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/covlmc.md)
  : Fit a Variable Length Markov Chain with Covariates (COVLMC)
- [`ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/ctx_tree.md)
  : Build a context tree for a discrete time series
- [`cutoff()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/cutoff.md)
  : Cut off values for VLMC-like models
- [`draw()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/draw.md)
  : Text based representation of a context tree
- [`metrics()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/metrics.md)
  : Predictive quality metrics for context based models
- [`states()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/states.md)
  : State space of an object
- [`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/vlmc.md)
  : Fit a Variable Length Markov Chain (VLMC)
