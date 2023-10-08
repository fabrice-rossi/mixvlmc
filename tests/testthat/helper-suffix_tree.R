count_occurrences <- function(where, what) {
  if (length(what) > length(where)) {
    0
  }
  res <- 0
  for (k in length(what):length(where)) {
    found <- TRUE
    for (j in 1:length(what)) {
      if (where[k - length(what) + j] != what[j]) {
        found <- FALSE
        break
      }
    }
    if (found) {
      res <- res + 1
    }
  }
  res
}

