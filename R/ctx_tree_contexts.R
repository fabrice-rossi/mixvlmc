frequency_context_extractor <-
  function(path, ct, vals, control, is_leaf, p_summary) {
    if ((is_leaf && !is.null(ct[["f_by"]])) ||
      (!is_leaf && nb_sub_tree(ct) < length(vals))) {
      if (is.null(control[["frequency"]])) {
        data.frame(context = I(list(path)))
      } else {
        res <- data.frame(
          context = I(list(path)),
          freq = sum(ct[["f_by"]])
        )
        if (control$frequency == "detailed") {
          freq_by_val <- as.list(ct[["f_by"]])
          names(freq_by_val) <- as.character(vals)
          res <-
            cbind(res, data.frame(freq_by_val, check.names = FALSE))
        }
        res
      }
    } else {
      NULL
    }
  }

#' @inherit contexts
#' @param frequency specifies the counts to be included in the result data.frame.
#'   The default value of `NULL` does not include anything. `"total"`
#'   gives the number of occurrences of each context in the original sequence.
#'   `"detailed"` includes in addition the break down of these occurrences
#'   into all the possible states.
#' @details The default result for `type="list"` and `frequency=NULL`
#'   is the list of all contexts.
#'
#'   Other results are obtained only with `type="data.frame"`. In this case
#'   the resulting `data.frame` has a `context` column storing the
#'   contexts. If `frequency="total"`, an additional column named
#'   `freq` gives the number of occurrences of each context in the series
#'   used to build the tree. If `frequency="detailed"`, one additional
#'   column is added per state in the context space. Each column records the
#'   number of times a given context is followed by the corresponding value in
#'   the original series.
#' @examples
#' dts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
#' dts_tree <- ctx_tree(dts, max_depth = 3, min_size = 5)
#' contexts(dts_tree)
#' contexts(dts_tree, "data.frame", frequency = "total")
#' contexts(dts_tree, "data.frame", frequency = "detailed")
#' @export
contexts.ctx_tree <- function(ct, type = c("list", "data.frame"), reverse = FALSE, frequency = NULL, ...) {
  type <- match.arg(type)
  if (missing(frequency)) {
    basic_extractor <- switch(type,
      "list" = path_list_extractor,
      "data.frame" = path_df_extractor
    )
    contexts_extractor(ct, reverse, basic_extractor, NULL)
  } else {
    assertthat::assert_that(type == "data.frame")
    assertthat::assert_that(frequency %in% c("total", "detailed"))
    control <- list(frequency = frequency)
    contexts_extractor(ct, reverse, frequency_context_extractor, control)
  }
}

rec_match_context <- function(tree, d, ctx) {
  if (length(ctx) == 0) {
    list(tree = tree, depth = d)
  } else {
    if (is.null(tree$children)) {
      list(tree = tree, depth = d)
    } else {
      cand <- tree$children[[ctx[1]]]
      if (length(cand) > 0) {
        rec_match_context(cand, d + 1, ctx[-1])
      } else {
        list(tree = tree, depth = d)
      }
    }
  }
}

match_context <- function(tree, ctx) {
  rec_match_context(tree, 0, ctx)
}
