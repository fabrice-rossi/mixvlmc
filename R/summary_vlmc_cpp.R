#' @exportS3Method
summary.vlmc_cpp <- function(object, ...) {
  rlang::check_dots_empty()
  ## the data frame of sequences is the fastest representation of the
  ## contexts with the C++ backend
  res <- summary_vlmc_from_lengths(
    object,
    lengths(contexts(object, sequence = TRUE)$context)
  )
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
