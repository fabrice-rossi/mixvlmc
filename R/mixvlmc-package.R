#' @section Package options:
#'
#' Mixvlmc uses the following [options()]:
#'
#' - `mixvlmc.maxit`: maximum number of iterations in model fitting for [covlmc()]
#' - `mixvlmc.predictive`: specifies the computing engine used for model fitting
#'    for [covlmc()]. Two values are supported:
#'    - `"glm"` (default value): [covlmc()] uses [stats::glm()] with a binomial
#'       link ([stats::binomial()]) for a two values state space, and [VGAM::vglm()]
#'       with a multinomial link ([VGAM::multinomial()]) for a state space with
#'       three or more values;
#'    - `"multinom"`: [covlmc()] uses [nnet::multinom()] in all cases.
#'
#'    The first option `"glm"` is recommended as both [stats::glm()] and [VGAM::vglm()]
#'    are able to detect and deal with degeneracy in the data set.
#'
#' @docType package
"_PACKAGE"

#'
## usethis namespace: start
#' @import Rcpp
#' @importFrom Rcpp evalCpp
#' @importFrom stats simulate
#' @importFrom stats predict
#' @useDynLib mixvlmc
## usethis namespace: end
NULL
#>
