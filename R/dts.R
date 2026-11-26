validate_dts <- function(x, vals) {
  if (anyNA(x)) {
    stop("discrete time series cannot contain missing values")
  }
  if (is.null(vals)) {
    ## x defines the state space: it must contain at least one value
    if (length(x) == 0) {
      stop("discrete time series cannot be empty")
    }
    if (is.character(x) || is.numeric(x)) {
      vals <- sort(unique(x))
      fx <- factor(x, vals)
    } else if (is.factor(x)) {
      fx <- x
      vals <- factor(levels(fx), levels(fx))
    } else if (is.logical(x)) {
      vals <- c(FALSE, TRUE)
      fx <- factor(x, vals)
    } else {
      stop(paste("x is not character, numeric or factor, but", class(x)))
    }
  } else {
    fx <- factor(x, levels = vals)
    assertthat::assert_that(assertthat::noNA(fx), msg = "x contains unknown states")
  }
  list(ix = as.integer(fx) - 1L, vals = vals, fx = fx)
}

new_dts <- function(ix, vals, fx, ..., class = character()) {
  structure(list(ix = ix, vals = vals, fx = fx),
    class = c(class, "dts")
  )
}

#' Convert a vector to a discrete time series
#'
#' This function creates a representation of a discrete time series that
#' can be further processed by model estimation functions.
#'
#' The discrete time series `x` can be a vector of numeric, character, factor or
#' logical type. If the state space of the series is not specified, that is when
#' `vals` is `NULL`, it is computed in a way that depends on the type of `x`:
#'
#' - for a factor, `vals` is set to the `levels()` of `x`;
#' - for a logical vector, `vals` is set to `c(FALSE, TRUE)`;
#' - for other types, `vals` is set to all the unique values taken by the time
#'   series (as returned by `sort(unique(x))`).
#'
#' If `vals` is specified, the function makes sure that `x` contains only the
#' specified values and reports an error otherwise. The state space `vals` may
#' include values that do not appear in `x`.
#'
#' @param x a discrete time series; can be numeric, character, factor or
#'   logical.
#' @param vals the set of values that can be taken by the time series, a.k.a. the
#'   state space, see details (defaults to `NULL`).
#' @returns a discrete time series (an object of a class that inherits from
#'   `dts`).
#' @export
#'
#' @examples
#' set.seed(0)
#' x_dts <- dts(sample(c("A", "B"), 20, replace = TRUE))
#' x_dts
#' ## specifying the state space
#' x_dts_3 <- dts(sample(c("A", "B"), 20, replace = TRUE),
#'   vals = c("A", "B", "C")
#' )
#' states(x_dts_3)
dts <- function(x, vals = NULL) {
  pre_res <- validate_dts(x, vals)
  new_dts(pre_res$ix, pre_res$vals, pre_res$fx)
}

as_dts <- function(x) {
  if (is_dts(x)) {
    x
  } else {
    dts(x)
  }
}

#' Test if the object is a discrete time series
#'
#' This function returns `TRUE` for discrete time series (objects of class
#' `dts`) and `FALSE` for other objects.
#'
#' @param x an R object.
#' @returns `TRUE` for discrete time series.
#' @export
#' @examples
#' set.seed(0)
#' pre_dts <- sample(c("A", "B"), 20, replace = TRUE)
#' x_dts <- dts(pre_dts)
#' is_dts(x_dts)
#' is_dts(pre_dts)
is_dts <- function(x) {
  inherits(x, "dts")
}

assertthat::on_failure(is_dts) <- function(call, env) {
  paste0(deparse(call$x), " is not a dts")
}

#' Basic operations on discrete time series
#'
#' These functions implement standard operations on discrete time series
#' represented by `dts` objects (see [dts()]), preserving their state space.
#'
#' @param x a `dts` object.
#' @param i indices specifying the elements to extract.
#' @param ... not used.
#'
#' @returns
#' - `x[i]` returns a `dts` object with the selected observations and the same
#'   state space as `x` (including the states that are absent from the
#'   selection);
#' - `x[[i]]` returns the `i`-th observation, as a value of the state space
#'   (see [states()]);
#' - `length(x)` returns the number of observations of `x`;
#' - `rev(x)` returns a `dts` object with the observations of `x` in reverse
#'   order and the same state space.
#' @seealso [dts()], [dts_data()], [states()]
#' @examples
#' x <- dts(c("A", "B", "C", "A", "A"))
#' length(x)
#' x[[2]]
#' ## the state space is preserved
#' x_short <- x[1:2]
#' x_short
#' states(x_short)
#' rev(x)
#' @name dts-operations
#' @aliases [.dts [[.dts length.dts rev.dts
NULL

#' @rdname dts-operations
#' @export
`[.dts` <- function(x, i, ...) {
  new_dts(x$ix[i, ...], x$vals, x$fx[i, ...])
}

#' @rdname dts-operations
#' @export
`[[.dts` <- function(x, i, ...) {
  x$vals[x$ix[[i, ...]] + 1]
}

#' @rdname dts-operations
#' @export
length.dts <- function(x) {
  length(x$ix)
}

#' Extract the plain discrete time series from a dts object
#'
#' This function returns a copy of the discrete time series used to build the
#' `dts` object (see [dts()]).
#'
#' @param x a `dts` object.
#'
#' @return a vector representing the time series, whose values are taken from
#'   the state space of `x` (see [states()]).
#' @export
#'
#' @examples
#' set.seed(0)
#' raw_dts <- sample(c("A", "B", "C"), 50, replace = TRUE)
#' odts <- dts(raw_dts)
#' back_to_raw <- dts_data(odts)
#' ## should be TRUE
#' identical(raw_dts, back_to_raw)
dts_data <- function(x) {
  assertthat::assert_that(is_dts(x))
  x$vals[x$ix + 1]
}

#' Print a discrete time series
#'
#' This function prints a discrete time series.
#'
#' @param x the `dts` object to print.
#' @param n the number of time steps of the time series to print (defaults to
#'   5).
#' @param ... additional arguments (not used).
#' @returns the `x` object, invisibly.
#'
#' @export
#' @examples
#' set.seed(0)
#' x_dts <- dts(sample(c("A", "B"), 20, replace = TRUE))
#' print(x_dts, n = 10)
print.dts <- function(x, n = 5, ...) {
  #  attr(x, "seed") <- NULL
  #  NextMethod()
  cwidth <- options("width")$width
  cat(
    "Discrete time series of length", length(x$ix),
    "with state space"
  )
  cat(stringr::str_flatten(as.character(x$vals), collapse = ", "),
    fill = cwidth, labels = "", sep = ""
  )
  if (length(x$ix) <= n) {
    to_disp <- as.character(x$vals[x$ix + 1])
  } else {
    to_disp <- c(as.character(x$vals[x$ix[1:n] + 1]), "...")
  }
  cat(to_disp,
    fill = cwidth, sep = " ", labels = ""
  )
  invisible(x)
}

#' @export
#' @rdname states
#' @examples
#' set.seed(0)
#' x_dts <- dts(sample(c("A", "B", "C"), 20, replace = TRUE))
#' ## should be c("A", "B", "C")
#' states(x_dts)
states.dts <- function(x) {
  x$vals
}

# if x is a dts, compare its states to vals and stop if they differ
# if x is not a dts tries to convert it using vals as the state space
# in case of success return x as a dts
convert_with_check <- function(x, vals, var_name = "x") {
  if (is_dts(x)) {
    if (!identical(x$vals, vals)) {
      ## the state spaces may differ only in their representation (e.g. integer
      ## versus double) or x may use a subset of the states: x is recoded in
      ## the state space of the model
      x <- tryCatch(dts(dts_data(x), vals), error = function(e) {
        stop(stringr::str_c(var_name, " is not compatible with the model state space"))
      })
    }
  } else {
    x <- dts(x, vals)
  }
  x
}

#' @rdname dts-operations
#' @export
rev.dts <- function(x) {
  new_dts(rev(x$ix), x$vals, rev(x$fx))
}
