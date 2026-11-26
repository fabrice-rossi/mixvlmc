# ASCII character set for context tree text representation

This function returns a list of ASCII characters used to fine-tune the
behaviour of the
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
function when it is used with `format="text"`. It can be used as is or
customised using its parameters, and is generally passed to
[`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw_control.md)
via its `charset` parameter.

## Usage

``` r
charset_ascii(
  root = "*",
  first_node = "+",
  next_node = "+",
  final_node = "'",
  vbranch = "|",
  hbranch = "--",
  open_ct = "(",
  close_ct = ")",
  level_sep = " ~ ",
  time_sep = " | ",
  intercept = "(I)",
  intercept_sep = " & ",
  open_p_value = "<",
  close_p_value = ">",
  open_model = "[",
  close_model = "]"
)
```

## Arguments

- root:

  character used for the root node.

- first_node:

  characters used for the first child of a node.

- next_node:

  characters used for intermediate children of a node.

- final_node:

  characters used for the last child of a node.

- vbranch:

  characters used to represent a branch in a vertical way.

- hbranch:

  characters used to represent a branch in a horizontal way.

- open_ct:

  characters used to start the node specific text representation of each
  node.

- close_ct:

  characters used to end the node specific text representation of each
  node.

- level_sep:

  characters used to separate levels from models in
  [`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md).

- time_sep:

  characters used to separate temporal blocks in
  [`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md).

- intercept:

  characters used to represent the intercept in
  [`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md).

- intercept_sep:

  characters used to separate the intercept from the other parameters in
  [`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md).

- open_p_value:

  characters used as opening delimiters for the p-value of a node in
  [`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md).

- close_p_value:

  characters used as closing delimiters for the p-value of a node in
  [`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md).

- open_model:

  characters used as opening delimiters for the representation of a
  model in
  [`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md).

- close_model:

  characters used as closing delimiters for the representation of a
  model in
  [`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md).

## Value

a list of character strings, one per parameter.

## See also

[`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md),
[`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw_control.md),
[`charset_utf8()`](https://fabrice-rossi.github.io/mixvlmc/reference/charset_utf8.md).

## Examples

``` r
charset_ascii(root = "x")
#> $root
#> [1] "x"
#> 
#> $first_node
#> [1] "+"
#> 
#> $next_node
#> [1] "+"
#> 
#> $final_node
#> [1] "'"
#> 
#> $vbranch
#> [1] "|"
#> 
#> $hbranch
#> [1] "--"
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
#> [1] " | "
#> 
#> $intercept
#> [1] "(I)"
#> 
#> $intercept_sep
#> [1] " & "
#> 
#> $open_p_value
#> [1] "<"
#> 
#> $close_p_value
#> [1] ">"
#> 
#> $open_model
#> [1] "["
#> 
#> $close_model
#> [1] "]"
#> 
rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 2)
draw(rdts_ctree, control = draw_control(charset = charset_ascii(root = "x")))
#> x
#> +-- 0
#> |   +-- 0
#> |   '-- 1
#> '-- 1
#>     +-- 0
#>     '-- 1
```
