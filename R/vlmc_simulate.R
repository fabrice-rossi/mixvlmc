#' @export
simulate.vlmc <- function(object, nsim = 1, seed = NULL, ...) {
  if (!is.null(seed)) {
    stop("Non NULL seed are not supported")
  }
  int_vals <- seq_along(object$vals)
  ctx <- c()
  pre_res <- rep(0, nsim)
  max_depth <- depth(object)
  for (i in 1:nsim) {
    subtree <- match_context(object, ctx)
    pre_res[i] <- sample(int_vals, 1, prob = subtree$tree$f_by)
    if (length(ctx) < max_depth) {
      ctx <- c(pre_res[i], ctx)
    } else {
      ctx <- c(pre_res[i], ctx[1:(max_depth - 1)])
    }
  }
  factor(pre_res, levels = int_vals, labels = object$vals)
}
