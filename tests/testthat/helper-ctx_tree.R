build_demo_tree_rec <- function(vals, depth) {
  if (depth < 0) {
    NULL
  } else if (depth == 0) {
    list(f_by = rep(2, length(vals)))
  } else {
    children <- vector(mode = "list", length(vals))
    for (v in seq_along(vals)) {
      children[[v]] <- build_demo_tree_rec(vals, depth - 1)
    }
    if (any(sapply(children, is.null))) {
      list()
    } else {
      list(children = children)
    }
  }
}

build_demo_tree <- function(vals, depth) {
  pre_res <- build_demo_tree_rec(vals, depth)
  pre_res$vals <- vals
  new_ctx_tree(vals, pre_res, compute_stats = FALSE)
}

## works only for integers (1:v)
count_f_by <- function(where, what, vals) {
  if (length(what) >= length(where)) {
    rep(0L, length(vals))
  } else {
    res <- rep(0L, length(vals))
    for (k in length(what):(length(where) - 1)) {
      found <- TRUE
      for (j in 1:length(what)) {
        if (where[k - length(what) + j] != what[j]) {
          found <- FALSE
          break
        }
      }
      if (found) {
        idx <- where[k + 1]
        res[idx] <- res[idx] + 1L
      }
    }
    res
  }
}

find_occurrences <- function(where, what) {
  res <- as.integer(NULL)
  if (length(what) > length(where)) {
    res
  }
  for (k in length(what):length(where)) {
    found <- TRUE
    for (j in 1:length(what)) {
      if (where[k - length(what) + j] != what[j]) {
        found <- FALSE
        break
      }
    }
    if (found) {
      res <- c(res, k - length(what) + 1L)
    }
  }
  res
}
