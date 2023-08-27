#' @exportS3Method
summary.vlmc_cpp <- function(object, ...) {
  res <- NextMethod()
  class(res) <- c("summary.vlmc_cpp", class(res))
  res
}

#' @exportS3Method
print.summary.vlmc_cpp <- function(x, ...) {
  cat(paste(
    "VLMC context tree on",
    paste(x$state_space, collapse = ", ")
  ), "[C++]\n")
  print_summary_vlmc(x)
  invisible(x)
}
