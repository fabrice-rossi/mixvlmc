# mixvlmc (development version)

## New features
* `contexts` can now report the positions of each context in the original time
  series
* the log likelihood calculations performed by `logLik` and `loglikelihood` have
  been revised, expanded to include three possible definitions of the likelihood 
  function, and documented in a new vignette
* `tune_vlmc` and `tune_covlmc` can be used with the different likelihood 
  function definitions
* results of `tune_vlmc` and `tune_covlmc` can be plotted using base R graphics
* `cutoff` uses a new `tolerance` parameter to avoid reporting cut off values
  that are almost identical due to numerical imprecision 
  
## Minor improvements and bug fixes
* Fixed several bugs related to degenerate VLMC (issue #55)
* Fixed a bug in the likelihood calculation for internal context nodes
* Improved type consistency by reporting counts as integers in `contexts` results
* Fixed documentation: all models can be adjusted on time series of logical values
* Synchronized `ctx_tree` documentation and its default value
* Synchronized `contexts` documentation and its default value
* Added more trimming for COVLMC models in `trim.covlmc`

# mixvlmc 0.1.1

* Added missing documentation about the return values of `simulate.vlmc` and
  `simulate.covlmc`

# mixvlmc 0.1.0

* Added a `NEWS.md` file to track changes to the package.
