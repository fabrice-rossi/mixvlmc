## compare the console outputs of two functions that should produce
## the same things
compare_output <- function(f, g) {
  f_tmpfile <- withr::local_tempfile(fileext = ".txt")
  withr::with_output_sink(
    f_tmpfile,
    eval(substitute(f), envir = parent.frame())
  )
  g_tmpfile <- withr::local_tempfile(fileext = ".txt")
  withr::with_output_sink(
    g_tmpfile,
    eval(substitute(g), envir = parent.frame())
  )
  compare_file_text(f_tmpfile, g_tmpfile)
}
