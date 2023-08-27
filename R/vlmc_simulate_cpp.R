#' @inherit simulate.vlmc
#' @param sample specifies which implementation of [base::sample()] to use.
#'  See the dedicated section.
#'
#' @section sampling method:
#'
#' The R backend for [vlmc()] uses [base::sample()] to generate samples for each
#' context. Internally, this function sorts the probabilities of each state in
#' decreasing probability order (among other things), which is not needed in our
#' case. The C++ backend can be used with three different implementations:
#'
#' - `sample="fast"` uses a dedicated C++ implementation adapted to the data structures
#'   used internally. In general, the simulated time series obtained with this
#'   implementation will be different from the one generated with the R backend,
#'   even using the same seed.
#' - `sample="slow"` uses another C++ implementation that mimics [base::sample()] in
#'   order to maximize the chance to provide identical simulation results regardless
#'   of the backend (when using the same random seed). This process is not perfect
#'   as we use the std::lib sort algorithm which is not guaranteed to give identical
#'   results as the ones of R internal 'revsort'.
#' - `sample="R"` uses direct calls to [base::sample()]. Results are guaranteed
#'   to be identical between the two backends, but at the price of higher running
#'   time.
#'
#' @export
simulate.vlmc_cpp <- function(object, nsim = 1, seed = NULL, init = NULL, sample = c("fast", "slow", "R"), ...) {
  if (extptr_is_null(object$root$.pointer)) {
    stop("Missing C++ representation!\nThis object was probably restored from a saved object.\n")
  }
  sample <- match.arg(sample)
  if (!is.null(seed)) {
    withr::local_seed(seed)
  }
  if (!is.null(init)) {
    assertthat::assert_that((typeof(init) == typeof(object$vals)) && (class(init) == class(object$vals)),
      msg = "init is not compatible with the model state space"
    )
    assertthat::assert_that(length(init) <= nsim, msg = "too many initial values")
    init_dts <- to_dts(init, object$vals)
    ctx <- init_dts$ix
  } else {
    ctx <- vector(mode = "integer")
  }
  isample <- 0L
  if (sample == "slow") {
    isample <- 1L
  } else if (sample == "R") {
    isample <- 2L
  }
  pre_res <- object$root$simulate(ctx, nsim, isample)
  object$vals[pre_res + 1]
}
