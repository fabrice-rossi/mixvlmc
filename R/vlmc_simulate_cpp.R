#' @inherit simulate.vlmc
#' @param sample specifies which implementation of [base::sample()] to use:
#'  `"fast"` (default), `"slow"` or `"R"`. See the dedicated section.
#'
#' @section Sampling method:
#'
#' The R backend for [vlmc()] uses [base::sample()] to generate samples for each
#' context. Internally, this function sorts the probabilities of each state in
#' decreasing probability order (among other things), which is not needed in our
#' case. The C++ backend can be used with three different implementations:
#'
#' - `sample="fast"` uses a dedicated C++ implementation adapted to the data
#'   structures used internally. In general, the simulated time series obtained
#'   with this implementation will be different from the one generated with the
#'   R backend, even using the same seed.
#' - `sample="slow"` uses another C++ implementation that mimics
#'   [base::sample()] in order to maximise the chance to provide identical
#'   simulation results regardless of the backend (when using the same random
#'   seed). This process is not perfect as we use the sort algorithm of the C++
#'   standard library, which is not guaranteed to give the same results as the
#'   internal `revsort` function of R.
#' - `sample="R"` uses direct calls to [base::sample()]. Results are guaranteed
#'   to be identical between the two backends, but at the price of a higher
#'   running time.
#'
#' @examples
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' rdts <- cut(pc$active_power,
#'   breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1)))
#' )
#' model <- vlmc(rdts, min_size = 5, backend = "C++")
#' new_rdts <- simulate(model, 500, seed = 0)
#' head(new_rdts, 10)
#' ## the same simulation as with the R backend
#' new_rdts_r <- simulate(model, 500, seed = 0, sample = "R")
#' model_r <- vlmc(rdts, min_size = 5)
#' identical(new_rdts_r, simulate(model_r, 500, seed = 0))
#' @export
simulate.vlmc_cpp <- function(object, nsim = 1, seed = NULL, init = NULL, burnin = 0L, sample = c("fast", "slow", "R"), ...) {
  rlang::check_dots_empty()
  restore_model(object)
  sample <- match.arg(sample)
  if (!is.null(seed)) {
    attr(seed, "kind") <- as.list(RNGkind())
    withr::local_seed(seed)
  } else {
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      stats::runif(1)
    }
    seed <- .Random.seed
  }
  if (burnin == "auto") {
    burnin <- 64L * context_number(object)
  } else {
    burnin <- as.integer(burnin)
    if (burnin < 0L) {
      stop("burnin must be non negative or \"auto\"")
    }
  }
  if (!is.null(init)) {
    assertthat::assert_that(length(init) <= nsim + burnin, msg = "too many initial values")
    init_dts <- convert_with_check(init, object$vals, "init")
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
  pre_res <- object$root$simulate(ctx, nsim, burnin, isample)
  pre_res <- object$vals[pre_res + 1]
  structure(pre_res, "seed" = seed, "class" = c("dts_simulated", class(pre_res)))
}
