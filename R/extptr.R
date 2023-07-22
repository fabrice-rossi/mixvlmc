#' @useDynLib mixvlmc extptr_is_null_
extptr_is_null <- function(ptr) {
  assertthat::assert_that(typeof(ptr) == "externalptr")
  .Call("extptr_is_null_", ptr)
}
