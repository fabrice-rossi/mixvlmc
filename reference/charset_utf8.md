# UTF-8 character set for context tree text representation

This function returns a list of UTF-8 characters and symbols used to
fine-tune the behaviour of the
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
function when it is used with `format="text"`. It can be used as is or
customised using its parameters, and is generally passed to
[`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw_control.md)
via its `charset` parameter.

## Usage

``` r
charset_utf8(
  root = "\u25AA",
  first_node = "\u251C",
  next_node = "\u251C",
  final_node = "\u2514",
  vbranch = "\u2502",
  hbranch = "\u2500",
  open_ct = "(",
  close_ct = ")",
  level_sep = " ~ ",
  time_sep = " \u205E ",
  intercept = "(I)",
  intercept_sep = " \u2022 ",
  open_p_value = "\u2039",
  close_p_value = "\u203A",
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
[`charset_ascii()`](https://fabrice-rossi.github.io/mixvlmc/reference/charset_ascii.md).

## Examples

``` r
charset_utf8(root = "\u27E1")
#> $root
#> [1] "⟡"
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
rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 2)
draw(rdts_ctree,
  control = draw_control(charset = charset_utf8(root = "\u27E1"))
)
#> ⟡
#> ├─ 0
#> │  ├─ 0
#> │  └─ 1
#> └─ 1
#>    ├─ 0
#>    └─ 1
```
