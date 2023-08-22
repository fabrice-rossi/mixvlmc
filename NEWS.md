# mixvlmc (development version)

## New features
* `contexts` can now report the positions of each context in the original time
  series
* the log likelihood calculations performed by `logLik` and `loglikelihood` have
  been revised, expanded to include three possible definitions of the likelihood 
  function, and documented in a new vignette
  
## Minor improvements and bug fixes
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
