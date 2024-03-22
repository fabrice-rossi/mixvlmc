## tools
add_fontsize <- function(x, params) {
  if (params$fontsize == params$prob_fontsize) {
    x
  } else {
    stringr::str_glue("{{\\{params$prob_fontsize} {x}}}")
  }
}

tabular_content <- function(label, x, params) {
  nb <- length(x)
  if (nb > 1) {
    stringr::str_glue("\\multicolumn{{{nb}}{{c}}{{{label}}}\\\\\\hline",
      "{xc}, align={al}",
      xc = stringr::str_c(add_fontsize(x, params), collapse = "&"),
      al = stringr::str_c(rep("c", nb), collapse = ""),
      .sep = "\n"
    )
  } else {
    stringr::str_glue("{{{label}}}\\\\\\hline\n{xs}, align=c",
      xs = add_fontsize(x, params)
    )
  }
}

decoration2tikz <- function(decoration) {
  switch(decoration,
    "none" = NULL,
    "rectangle" = "draw",
    "circle" = "circle, draw",
    "ellipse" = "ellipse, draw"
  )
}

orientation2tikz <- function(orientation) {
  if (orientation == "horizontal") {
    "grow'=west"
  } else {
    NULL
  }
}

global_forest_option <- function(params) {
  dec <- decoration2tikz(params$decoration)
  ori <- orientation2tikz(params$orientation)
  if (is.null(dec) && is.null(ori)) {
    NULL
  } else {
    stringr::str_c("  for tree={", stringr::str_c(dec, ori, sep = ", "), "}")
  }
}

start_forest <- function(params) {
  if (params$fontsize != "normalsize") {
    cat("\\begin{", params$fontsize, "}\n", sep = "")
  }
  cat("\\begin{forest}\n")
  cat(global_forest_option(params))
}

end_forest <- function(params) {
  cat("\\end{forest}\n")
  if (params$fontsize != "normalsize") {
    cat("\\end{", params$fontsize, "}\n", sep = "")
  }
}
