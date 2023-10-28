#' @rdname predict.vlmc
#' @export
#' @examples
#' ## C++ backend
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
#' model <- vlmc(dts, min_size = 5, backend = "C++")
#' predict(model, dts[1:5])
#' predict(model, dts[1:5], "probs")
predict.vlmc_cpp <- function(object, newdata, type = c("raw", "probs"),
                             final_pred = TRUE, ...) {
  restore_model(object)
  type <- match.arg(type)
  assertthat::assert_that(rlang::is_logical(final_pred))
  if (!missing(newdata) && !is.null(newdata)) {
    assertthat::assert_that((typeof(newdata) == typeof(object$vals)) && (class(newdata) == class(object$vals)),
      msg = "newdata is not compatible with the model state space"
    )
    dts <- to_dts(newdata, object$vals)
  } else {
    stop("newdata must be provided.")
  }
  if (type == "raw") {
    object$vals[1 + object$root$predict_raw(dts$ix, final_pred)]
  } else {
    pre_res <- object$root$predict_probs(dts$ix, final_pred)
    colnames(pre_res) <- as.character(object$vals)
    pre_res
  }
}
