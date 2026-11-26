#' @section Package options:
#'
#' mixvlmc uses the following [options()], which are set when the package is
#' loaded (unless they are already set):
#'
#' - `mixvlmc.maxit`: maximum number of iterations in model fitting for
#'    [covlmc()] and related functions such as [tune_covlmc()] (default value:
#'    100). It is used by all the computing engines (see
#'    `mixvlmc.predictive`).
#' - `mixvlmc.predictive`: specifies the computing engine used for model fitting
#'    for [covlmc()] and related functions. Two values are supported:
#'    - `"glm"` (default value): [covlmc()] uses [stats::glm()] with a binomial
#'       link ([stats::binomial()]) for a two-value state space, and
#'       [VGAM::vglm()] with a multinomial link ([VGAM::multinomial()]) for a
#'       state space with three or more values;
#'    - `"multinom"`: [covlmc()] uses [nnet::multinom()] in all cases.
#'
#'    The first option `"glm"` is recommended as both [stats::glm()] and
#'    [VGAM::vglm()] are able to detect and deal with degeneracy in the data
#'    set. The results of the two options can differ, see the "Logistic models"
#'    section of [covlmc()].
#' - `mixvlmc.backend`: specifies the implementation used for the context tree
#'    construction in [ctx_tree()], [vlmc()] and [tune_vlmc()]. Two values are
#'    supported:
#'    - `"C++"` (default value): this corresponds to the C++ implementation,
#'      which is significantly faster than the R implementation.
#'    - `"R"`: this corresponds to the original almost pure R implementation,
#'      whose context trees are nested lists that can be processed in R.
#'
#'    Both implementations give the same models. [covlmc()] and
#'    [tune_covlmc()] always use the R implementation.
#' - `mixvlmc.charset`: specifies the collection of characters used to display
#'    context trees in "ascii art" when using the `"text"` format for [draw()]
#'    and related functions. Two values are supported:
#'    - `"ascii"`: the collection uses only standard ASCII characters and
#'      should be compatible with all environments;
#'    - `"utf8"`: the collection uses UTF-8 symbols and needs a compatible
#'      display.
#'
#'    The default value is `"utf8"` if the output supports UTF-8 (according to
#'    [cli::is_utf8_output()] when the package is loaded) and `"ascii"`
#'    otherwise.
#'
"_PACKAGE"

#'
## usethis namespace: start
#' @import Rcpp
#' @importFrom Rcpp evalCpp
#' @importFrom stats simulate
#' @importFrom stats predict
#' @importFrom rlang .data
#' @useDynLib mixvlmc, .registration = TRUE
## usethis namespace: end
NULL
#>
