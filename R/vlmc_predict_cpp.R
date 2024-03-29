#' @rdname predict.vlmc
#' @export
#' @examples
#' ## C++ backend
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' rdts <- cut(pc$active_power,
#'   breaks = c(0, quantile(pc$active_power,
#'     probs = c(0.25, 0.5, 0.75, 1)
#'   ))
#' )
#' model <- vlmc(rdts, min_size = 5, backend = "C++")
#' predict(model, rdts[1:5])
#' predict(model, rdts[1:5], "probs")
predict.vlmc_cpp <- function(object, newdata, type = c("raw", "probs"),
                             final_pred = TRUE, ...) {
  restore_model(object)
  type <- match.arg(type)
  assertthat::assert_that(rlang::is_logical(final_pred))
  if (!missing(newdata) && !is.null(newdata)) {
    nd_dts <- convert_with_check(newdata, object$vals, "newdata")
  } else {
    stop("newdata must be provided.")
  }
  if (type == "raw") {
    object$vals[1 + object$root$predict_raw(nd_dts$ix, final_pred)]
  } else {
    pre_res <- object$root$predict_probs(nd_dts$ix, final_pred)
    colnames(pre_res) <- as.character(object$vals)
    pre_res
  }
}
