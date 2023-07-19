#' Convert an object to a Variable Length Markov Chain (VLMC)
#'
#' This generic function converts an object into a vlmc.
#'
#' @param x an object to convert into a vlmc.
#' @param ... additional arguments for conversion functions.
#'
#' @returns a vlmc
#' @export
as_vlmc <- function(x, ...) {
  UseMethod("as_vlmc")
}

#' @export
#' @rdname as_vlmc
#' @seealso [ctx_tree()]
#' @examples
#' ## conversion from a context tree
#' dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
#' dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 3)
#' draw(dts_ctree)
#' dts_vlmc <- as_vlmc(dts_ctree)
#' class(dts_vlmc)
#' draw(dts_vlmc)
as_vlmc.ctx_tree <- function(x, ...) {
  new_ctx_tree(x$vals, x, class = "vlmc")
}

#' @export
#' @rdname as_vlmc
#' @seealso [tune_vlmc()]
#' @examples
#' ## conversion from the result of tune_vlmc
#' dts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
#' tune_result <- tune_vlmc(dts)
#' tune_result
#' dts_best_vlmc <- as_vlmc(tune_result)
#' draw(dts_best_vlmc)
as_vlmc.tune_vlmc <- function(x, ...) {
  x$best_model
}