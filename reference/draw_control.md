# Control parameters for `draw()`

This function returns a list used to fine-tune the behaviour of the
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
function.

## Usage

``` r
draw_control(
  digits = 4,
  charset = NULL,
  orientation = c("vertical", "horizontal"),
  tabular = TRUE,
  tab_orientation = c("vertical", "horizontal"),
  decoration = c("none", "rectangle", "circle", "ellipse"),
  fontsize = "normalsize",
  prob_fontsize = "small"
)
```

## Arguments

- digits:

  numerical parameters, probabilities and p-values are represented using
  the [`base::signif()`](https://rdrr.io/r/base/Round.html) function,
  using the number of significant digits specified with this parameter
  (defaults to 4).

- charset:

  specifies the characters used for the "ascii art" representation when
  the format is "text", see the "Charset" section.

- orientation:

  specifies the global orientation of the tree, either "vertical"
  (default) or "horizontal" ("latex").

- tabular:

  if `TRUE` (default value), the "latex" format will use tables for each
  node, with one row for the state value and other rows for additional
  information (such as the conditional probability associated with the
  context). Notice that
  [`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md)
  always uses tables regardless of the value of `tabular`.

- tab_orientation:

  specifies the way the models are represented when used by
  [`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md)
  ("latex"). The default value is `"vertical"`: this is well adapted to
  models with long covariate dependencies (see
  [`covariate_depth()`](https://fabrice-rossi.github.io/mixvlmc/reference/covariate_depth.md)).
  The other possible value is `"horizontal"`.

- decoration:

  specifies node decoration in the "latex" format, see the "Decoration"
  section.

- fontsize:

  font size for the state names in the "latex" format (using LaTeX
  standard font sizes such as `"small"` or `"large"`, defaults to
  `"normalsize"`).

- prob_fontsize:

  font size for the context counts, probabilities or models in the
  "latex" format (using LaTeX standard font sizes, defaults to
  `"small"`).

## Value

a list of control parameters.

## Details

Parameters are generally specific to the `format` used for
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md).
If this is the case, the format is given at the end of the parameter
description. Some parameters are also specific to some methods of
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md).

## Decoration

The LaTeX format (`"latex"`) can "decorate" the nodes of the context
tree by drawing borders. We support only basic decorations, but in
theory all TikZ possibilities could be used (see the documentation of
the [forest LaTeX package](https://ctan.org/pkg/forest)). Supported
decorations:

- `"none"`: default, no decoration;

- `"rectangle"`: adds a rectangular border to all nodes;

- `"circle"`: adds a circular border to all nodes;

- `"ellipse"`: adds an ellipsoidal border to all nodes.

## Charset

The "ascii art" format (`"text"`) uses a collection of characters to
display a context tree. The default collection is specified by the
package option `"mixvlmc.charset"` and is used when `charset=NULL`
(default value). If `charset` is set to a character value, this value is
used to select the collection in the same way that `"mixvlmc.charset"`
specifies it:

- `"ascii"`: the collection uses only standard ASCII characters and
  should be compatible with all environments;

- `"utf8"`: the collection uses UTF-8 symbols and needs a compatible
  display.

Finally, `charset` can be a user supplied list of characters such as the
ones returned by
[`charset_ascii()`](https://fabrice-rossi.github.io/mixvlmc/reference/charset_ascii.md)
and
[`charset_utf8()`](https://fabrice-rossi.github.io/mixvlmc/reference/charset_utf8.md).

## See also

[`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md),
[`charset_ascii()`](https://fabrice-rossi.github.io/mixvlmc/reference/charset_ascii.md)
and
[`charset_utf8()`](https://fabrice-rossi.github.io/mixvlmc/reference/charset_utf8.md).

## Examples

``` r
draw_control(digits = 2, tabular = FALSE)
#> $digits
#> [1] 2
#> 
#> $orientation
#> [1] "vertical"
#> 
#> $tabular
#> [1] FALSE
#> 
#> $tab_orientation
#> [1] "vertical"
#> 
#> $decoration
#> [1] "none"
#> 
#> $fontsize
#> [1] "normalsize"
#> 
#> $prob_fontsize
#> [1] "small"
#> 
#> $root
#> [1] "▪"
#> 
#> $first_node
#> [1] "├"
#> 
#> $next_node
#> [1] "├"
#> 
#> $final_node
#> [1] "└"
#> 
#> $vbranch
#> [1] "│"
#> 
#> $hbranch
#> [1] "─"
#> 
#> $open_ct
#> [1] "("
#> 
#> $close_ct
#> [1] ")"
#> 
#> $level_sep
#> [1] " ~ "
#> 
#> $time_sep
#> [1] " ⁞ "
#> 
#> $intercept
#> [1] "(I)"
#> 
#> $intercept_sep
#> [1] " • "
#> 
#> $open_p_value
#> [1] "‹"
#> 
#> $close_p_value
#> [1] "›"
#> 
#> $open_model
#> [1] "["
#> 
#> $close_model
#> [1] "]"
#> 
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power,
  breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1)))
)
model <- vlmc(rdts, max_depth = 2)
draw(model, control = draw_control(digits = 2, charset = "ascii"))
#> * (0.25, 0.25, 0.25, 0.25)
#> +-- (0,0.458] (0.8, 0.19, 0.008, 0.004)
#> +-- (0.458,1.34] (0.2, 0.67, 0.11, 0.028)
#> |   '-- (0,0.458] (0.38, 0.42, 0.15, 0.062)
#> +-- (1.34,2.13] (0.004, 0.12, 0.73, 0.15)
#> |   '-- (2.13,7.54] (0, 0.2, 0.52, 0.28)
#> '-- (2.13,7.54] (0.004, 0.016, 0.16, 0.82)
#>     '-- (0.458,1.34] (0, 0.14, 0.57, 0.29)
draw(model, "latex",
  control = draw_control(
    orientation = "horizontal",
    decoration = "rectangle"
  )
)
#> \begin{forest}
#>   for tree={draw, grow'=west}
#> [\multicolumn{4}{c}{\textbf{$\epsilon$}}\\\hline
#> {\small 0.25}&{\small 0.25}&{\small 0.25}&{\small 0.25}, align=cccc
#> [\multicolumn{4}{c}{\textbf{(0,0.458]}}\\\hline
#> {\small 0.7968}&{\small 0.1912}&{\small 0.007968}&{\small 0.003984}, align=cccc]
#> [\multicolumn{4}{c}{\textbf{(0.458,1.34]}}\\\hline
#> {\small 0.1984}&{\small 0.6667}&{\small 0.1071}&{\small 0.02778}, align=cccc
#> [\multicolumn{4}{c}{\textbf{(0,0.458]}}\\\hline
#> {\small 0.375}&{\small 0.4167}&{\small 0.1458}&{\small 0.0625}, align=cccc]]
#> [\multicolumn{4}{c}{\textbf{(1.34,2.13]}}\\\hline
#> {\small 0.003968}&{\small 0.123}&{\small 0.7262}&{\small 0.1468}, align=cccc
#> [\multicolumn{4}{c}{\textbf{(2.13,7.54]}}\\\hline
#> {\small 0}&{\small 0.2}&{\small 0.525}&{\small 0.275}, align=cccc]]
#> [\multicolumn{4}{c}{\textbf{(2.13,7.54]}}\\\hline
#> {\small 0.003968}&{\small 0.01587}&{\small 0.1587}&{\small 0.8214}, align=cccc
#> [\multicolumn{4}{c}{\textbf{(0.458,1.34]}}\\\hline
#> {\small 0}&{\small 0.1429}&{\small 0.5714}&{\small 0.2857}, align=cccc]]]
#> \end{forest}
```
