frequency_context_extractor <-
  function(tree, path, ct, vals, control, is_leaf, p_summary) {
    if ((is_leaf && !is.null(ct[["f_by"]])) ||
      (!is_leaf && nb_sub_tree(ct) < length(vals))) {
      if (is.null(control[["frequency"]])) {
        if (isFALSE(control[["positions"]])) {
          data.frame(context = I(list(path)))
        } else {
          if (is.null(ct[["match"]])) {
            stop("Cannot report positions if they were not saved")
          } else {
            data.frame(
              context = I(list(path)),
              positions = I(list(ct[["match"]] + length(path)))
            )
          }
        }
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
        if (isTRUE(control[["positions"]])) {
          if (is.null(ct[["match"]])) {
            stop("Cannot report positions if they were not saved")
          } else {
            res[["positions"]] <- list(ct[["match"]] + length(path))
          }
        }
        if (!is_leaf && isTRUE(control$counts == "local")) {
          ## remove sub counts
          for (child in ct$children) {
            if (!is.null(child[["f_by"]])) {
              res$freq <- res$freq - sum(child[["f_by"]])
              if (control$frequency == "detailed") {
                res[, -(1:2)] <- res[, 3:(2 + length(vals)), drop = FALSE] - child[["f_by"]]
              }
            }
          }
        }
        res
      }
    } else {
      NULL
    }
  }

#' @inherit contexts
#' @param frequency specifies the counts to be included in the result
#'   data.frame. The default value of `NULL` does not include anything.
#'   `"total"` gives the number of occurrences of each context in the original
#'   sequence. `"detailed"` includes in addition the break down of these
#'   occurrences into all the possible states.
#' @param positions logical (defaults to FALSE). Specify whether the positions
#'   of each context in the time series used to build the context tree should
#'   be reported in a `positions` column of the result data frame. The
#'   availability of the positions depends on the way the context
#'   tree was built. See details for the definition of a position.
#' @details The `frequency` and `positions` parameters influence only the
#'   results when `type="data.frame"`. In this case the resulting `data.frame`
#'   has a `context` column storing the contexts. If `frequency="total"`, an
#'   additional column named `freq` gives the number of occurrences of each
#'   context in the series used to build the tree. If `frequency="detailed"`,
#'   one additional column is added per state in the context space. Each column
#'   records the number of times a given context is followed by the
#'   corresponding value in the original series.
#'
#'   When `type="list"`, similar information can be obtained from the `ctx_node`
#'   objects returned by the function as a `contexts` list.
#'
#' @section Positions: A position of a context `ctx` in the time series `x` is an
#'   index value `t` such that the context ends with `x[t]`. Thus `x[t+1]` is after
#'   the context. For instance if `x=c(0, 0, 1, 1)` and `ctx=c(0, 1)` (in standard
#'   state order), then the position of `ctx` in `x` is 3.
#'
#' @examples
#' dts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
#' dts_tree <- ctx_tree(dts, max_depth = 3, min_size = 5)
#' contexts(dts_tree)
#' contexts(dts_tree, type = "data.frame", frequency = "total")
#' contexts(dts_tree, type = "data.frame", frequency = "detailed")
#' @export
contexts.ctx_tree <- function(ct, type = c("list", "data.frame"),
                              reverse = TRUE, frequency = NULL,
                              positions = FALSE, ...) {
  type <- match.arg(type)
  if (type == "list") {
    new_context_list(contexts_extractor(ct, reverse, node_content_extractor, NULL))
  } else {
    if (!is.null(frequency)) {
      assertthat::assert_that(frequency %in% c("total", "detailed"))
      extractor <- frequency_context_extractor
    } else if (positions) {
      extractor <- frequency_context_extractor
    } else {
      extractor <- path_df_extractor
    }
    control <- list(frequency = frequency, positions = positions)
    pre_res <- contexts_extractor(ct, reverse, extractor, control)
    if (positions) {
      pre_res$positions <- I(pre_res$positions)
    }
    pre_res
  }
}

node_content_extractor <- function(tree, path, ct, vals, control, is_leaf, p_summary) {
  if (is_leaf) {
    if (is.null(ct[["f_by"]])) {
      NULL
    } else {
      list(new_ctx_node(path, tree, ct, TRUE))
    }
  } else {
    if (nb_sub_tree(ct) < length(vals)) {
      list(new_ctx_node(path, tree, ct, TRUE))
    } else {
      NULL
    }
  }
}

rec_match_context <- function(tree, d, ctx) {
  if (length(ctx) == 0L) {
    list(tree = tree, depth = d)
  } else {
    if (is.null(tree$children)) {
      list(tree = tree, depth = d)
    } else {
      cand <- tree$children[[ctx[1]]]
      if (length(cand) > 0L) {
        rec_match_context(cand, d + 1L, ctx[-1])
      } else {
        list(tree = tree, depth = d)
      }
    }
  }
}

match_context <- function(tree, ctx) {
  rec_match_context(tree, 0L, ctx)
}
