test_that("draw() returns a printable representation", {
  withr::local_options(mixvlmc.charset = "ascii")
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 200, replace = TRUE)
  models <- list(
    ctx_tree(x, max_depth = 3), ctx_tree(x, max_depth = 3, backend = "C++"),
    vlmc(x), vlmc(x, backend = "C++"),
    covlmc(x, data.frame(y = rnorm(length(x))), min_size = 10)
  )
  for (model in models) {
    for (format in c("text", "latex")) {
      expect_silent(repr <- draw(model, format))
      expect_s3_class(repr, "mixvlmc_draw")
      lines <- as.character(repr)
      expect_type(lines, "character")
      expect_null(attributes(lines))
      expect_identical(format(repr), lines)
      expect_identical(capture.output(print(repr)), lines)
      expect_identical(capture.output(draw(model, format)), lines)
      capture.output(printed <- withVisible(print(repr)))
      expect_false(printed$visible)
      expect_identical(printed$value, repr)
    }
  }
})

test_that("knitr includes LaTeX representations as is in LaTeX documents", {
  skip_if_not_installed("knitr")
  withr::local_seed(0)
  model <- vlmc(sample(c("A", "B", "C"), 200, replace = TRUE))
  repr <- draw(model, "latex")
  ## LaTeX output
  old_to <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  knitr::opts_knit$set(rmarkdown.pandoc.to = "latex")
  withr::defer(knitr::opts_knit$set(rmarkdown.pandoc.to = old_to))
  out <- knitr::knit_print(repr)
  expect_s3_class(out, "knit_asis")
  expect_identical(
    as.character(out),
    paste0("\n\n", paste(as.character(repr), collapse = "\n"), "\n\n")
  )
  deps <- attr(out, "knit_meta")
  expect_length(deps, 1)
  expect_s3_class(deps[[1]], "latex_dependency")
  expect_match(deps[[1]]$extra_lines, "\\usepackage{forest}", fixed = TRUE)
  ## other outputs
  knitr::opts_knit$set(rmarkdown.pandoc.to = "html")
  capture.output(out <- knitr::knit_print(repr))
  expect_false(inherits(out, "knit_asis"))
  knitr::opts_knit$set(rmarkdown.pandoc.to = "latex")
  capture.output(out <- knitr::knit_print(draw(model)))
  expect_false(inherits(out, "knit_asis"))
})

test_that("a LaTeX representation is rendered in a R Markdown LaTeX document", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())
  rmd <- withr::local_tempfile(fileext = ".Rmd")
  writeLines(c(
    "---",
    "output: latex_document",
    "---",
    "```{r}",
    "library(mixvlmc)",
    "model <- vlmc(rep(c(\"A\", \"B\", \"B\"), 20))",
    "draw(model, \"latex\")",
    "```"
  ), rmd)
  tex <- rmarkdown::render(rmd, quiet = TRUE, envir = new.env())
  withr::defer(unlink(tex))
  tex_lines <- readLines(tex)
  expect_true(any(grepl("\\usepackage{forest}", tex_lines, fixed = TRUE)))
  expect_true(any(grepl("\\begin{forest}", tex_lines, fixed = TRUE)))
  expect_false(any(grepl("## \\begin{forest}", tex_lines, fixed = TRUE)))
})

test_that("a document can load forest with its own options", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())
  skip_if(!nzchar(Sys.which("pdflatex")))
  rmd <- withr::local_tempfile(fileext = ".Rmd")
  writeLines(c(
    "---",
    "output:",
    "  pdf_document:",
    "    keep_tex: true",
    "header-includes:",
    "  - \\usepackage[linguistics]{forest}",
    "---",
    "```{r}",
    "library(mixvlmc)",
    "model <- vlmc(rep(c(\"A\", \"B\", \"B\"), 20))",
    "draw(model, \"latex\")",
    "```"
  ), rmd)
  pdf <- rmarkdown::render(rmd, quiet = TRUE, envir = new.env())
  withr::defer(unlink(c(pdf, sub("pdf$", "tex", pdf))))
  expect_true(file.exists(pdf))
})
