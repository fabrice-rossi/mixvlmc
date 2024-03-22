## tools
add_fontsize <- function(x, control) {
  if (control$fontsize == control$prob_fontsize) {
    x
  } else {
    stringr::str_glue("{{\\{control$prob_fontsize} {x}}}")
  }
}

tabular_content <- function(label, x, control) {
  nb <- length(x)
  if (nb > 1) {
    stringr::str_glue("\\multicolumn{{{nb}}{{c}}{{{label}}}\\\\\\hline",
      "{xc}, align={al}",
      xc = stringr::str_c(add_fontsize(x, control), collapse = "&"),
      al = stringr::str_c(rep("c", nb), collapse = ""),
      .sep = "\n"
    )
  } else {
    stringr::str_glue("{{{label}}}\\\\\\hline\n{xs}, align=c",
      xs = add_fontsize(x, control)
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

global_forest_option <- function(control) {
  dec <- decoration2tikz(control$decoration)
  ori <- orientation2tikz(control$orientation)
  if (is.null(dec) && is.null(ori)) {
    NULL
  } else {
    stringr::str_c("  for tree={", stringr::str_c(dec, ori, sep = ", "), "}")
  }
}

start_forest <- function(control) {
  if (control$fontsize != "normalsize") {
    cat("\\begin{", control$fontsize, "}\n", sep = "")
  }
  cat("\\begin{forest}\n")
  cat(global_forest_option(control))
}

end_forest <- function(control) {
  cat("\\end{forest}\n")
  if (control$fontsize != "normalsize") {
    cat("\\end{", control$fontsize, "}\n", sep = "")
  }
}
