validate_multi_dts <- function(xs, vals = NULL) {
  if (is.null(vals)) {
    vals <- states(xs[[1]])
  }
  ## consistency check
  lapply(xs[-1], \(x) assertthat::assert_that(is_dts(x)))
  lapply(xs, \(x) assertthat::assert_that(identical(x$vals, vals)))
  ## extraction
  list(ixs = lapply(xs, \(x) x$ix), vals = vals)
}

validate_multi_vector <- function(xs, vals = NULL) {
  if (is.null(vals)) {
    nx <- validate_dts(xs[[1]], NULL)
    vals <- nx$vals
    if (length(xs) > 1) {
      ixs <- c(list(nx$ix), lapply(xs[-1], \(x) validate_dts(x, vals = vals)$ix))
    } else {
      ixs <- list(nx$ix)
    }
    list(ixs = ixs, vals = vals)
  } else {
    list(ixs = lapply(xs, \(x) validate_dts(x, vals = vals)$ix), vals = vals)
  }
}

new_dts_list <- function(ixs, vals, ..., class = character()) {
  structure(list(ixs = ixs, vals = vals),
    class = c(class, "dts_list")
  )
}


#' Convert a list of vectors/dts to a collection of discrete time series.
#'
#' This function creates a representation of a collection of discrete time
#' series that can be further processed by model estimation functions.
#'
#' The content of the list can be a collection of [dts()] or of vectors.
#' The vectors of the list must all be of the same type and when interpreted
#' as discrete time series, they have to use a common state space. The possible
#' types are the one supported by `[dts()]` (numeric, character, factor or
#' logical type).
#'
#' If the state space of a collection of series is not specified, that is when
#' `vals` is `NULL`, it is computed base on the first vector `xs[[1]]` or
#' taken from the first `[dts()]` object.
#'
#' If `vals` is specified, the function makes sure that all time series use
#' only the allowed values.
#'
#' @param xs a list of vectors or of [dts()] objects that can be interpreted as discrete time series
#' @param vals the set of values that can be taken by the time series, a.k.a. the
#'   state space, see details (defaults to `NULL`)
#' @returns a collection of discrete time series, a list of class that inherits
#' from `dts_list`.
#' @seealso [dts()]
#' @export
#'
#' @examples
#' pre_dts <- lapply(1:20, \(x) sample(c("A", "B"), x + 20, replace = TRUE))
#' dts_collection <- dts_list(pre_dts, c("A", "B"))
#' dts_collection
#' ## sub-collection
#' dts_collection[5:10]
#' ## one time series
#' dts_collection[[15]]
dts_list <- function(xs, vals = NULL) {
  assertthat::assert_that(is.list(xs))
  assertthat::assert_that(length(xs) > 0)
  if (is_dts(xs[[1]])) {
    pre_res <- validate_multi_dts(xs, vals)
  } else {
    pre_res <- validate_multi_vector(xs, vals)
  }
  new_dts_list(pre_res$ix, pre_res$vals)
}

#' Test if the object is a list of discrete time series
#'
#' This function returns `TRUE` for discrete time series list and `FALSE`
#' for other objects.
#'
#' @param x an R object.
#' @returns `TRUE` for discrete time series list (as created by [dts_list()])
#' @export
#' @examples
#' pre_dts <- lapply(1:20, \(x) sample(c("A", "B"), x + 20, replace = TRUE))
#' dts_collection <- dts_list(pre_dts, c("A", "B"))
#' is_dts_list(dts_collection)
#' is_dts_list(pre_dts)
is_dts_list <- function(x) {
  inherits(x, "dts_list")
}

#' @export
`[.dts_list` <- function(x, i, ...) {
  new_dts_list(x$ixs[i, ...], x$vals)
}

#' @export
`[[.dts_list` <- function(x, i, ...) {
  x_sub <- x$ixs[[i, ...]]
  fx <- x$vals[x_sub + 1]
  if (!is.factor(x$vals)) {
    fx <- factor(fx, x$vals)
  }
  new_dts(x_sub, vals = x$vals, fx = fx)
}

#' @export
length.dts_list <- function(x) {
  length(x$ixs)
}

# For some reasons, plain export does not work
#' @exportS3Method lengths dts_list
lengths.dts_list <- function(x, use.names = TRUE) {
  lengths(x$ixs)
}

#' @export
print.dts_list <- function(x, ...) {
  cwidth <- options("width")$width
  cat(
    "Collection of", length(x$ixs), "discrete time series",
    "with state space"
  )
  cat(stringr::str_flatten(as.character(x$vals), collapse = ", "),
    fill = cwidth, labels = "", sep = ""
  )
  invisible(x)
}

#' @export
as.list.dts_list <- function(x, ...) {
  lapply(x$ixs, \(y) dts(x$vals[y + 1], x$vals))
}

#' @export
#' @rdname states
#' @examples
#' pre_dts <- lapply(1:10, \(x) sample(c("A", "B", "C"), x + 10, replace = TRUE))
#' dts_collection <- dts_list(pre_dts, c("A", "B", "C"))
#' states(dts_collection)
states.dts_list <- function(x) {
  x$vals
}
