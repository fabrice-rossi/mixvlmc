## works only for integers (1:v)
multi_count_f_by <- function(mwhere, what, vals) {
  res <- count_f_by(mwhere[[1]], what, vals)
  for (k in 2:length(mwhere)) {
    res <- res + count_f_by(mwhere[[k]], what, vals)
  }
  res
}

## returns TRUE if x ends with y
ends_with <- function(x, y) {
  if (length(y) > length(x)) {
    FALSE
  } else {
    all(y == x[(length(x) - length(y) + 1):length(x)])
  }
}
