# Text based representation of a VLMC with covariates

This function 'draws' a VLMC with covariates (a `covlmc` object) as
text.

## Usage

``` r
# S3 method for class 'covlmc'
draw(
  ct,
  format,
  control = draw_control(),
  model = c("coef", "full"),
  p_value = FALSE,
  with_state = FALSE,
  constant_as_prob = TRUE,
  ...
)
```

## Arguments

- ct:

  a fitted VLMC with covariates (a `covlmc` object).

- format:

  a character string that specifies the output format of the function.
  Possible values are `"text"` (default) and `"latex"`. See details.

- control:

  a list of low level control parameters of the representation. See
  details and
  [`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw_control.md).

- model:

  this parameter controls the display of logistic models associated with
  nodes (accepted values: `"coef"` (default), `"full"` and `NULL`). The
  interpretation of the parameter depends on the format, see below for
  details.

- p_value:

  specifies whether the p-values of the likelihood ratio tests conducted
  during the construction of the model must be included in the
  representation (defaults to `FALSE`).

- with_state:

  specifies whether to display the state associated with each dimension
  of the logistic model (defaults to `FALSE`, see details).

- constant_as_prob:

  specifies how to represent constant logistic models for
  `format="text"` (defaults to `TRUE`, see details). Disregarded when
  `format="latex"`.

- ...:

  not used, except for the deprecated `digits` and `time_sep` arguments
  (customisation options are given with
  [`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw_control.md)).

## Value

an object of class `mixvlmc_draw` (see
[`print.mixvlmc_draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/print.mixvlmc_draw.md)).

## Details

The function uses different text based formats (plain "ascii art" and
LaTeX) to represent the context tree. Fine-tuning of the representation
can be done via the
[`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw_control.md)
function.

Unlike the
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
methods adapted to context trees
([`draw.ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.ctx_tree.md))
and VLMC
([`draw.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.vlmc.md)),
the present function does not try to produce similar results for the
`"text"` format and the `"latex"` format as the `"text"` format is
intrinsically more limited in terms of model representations. This is
detailed below.

## Format

The `format` parameter specifies the format used for the textual output.
With the default value `"text"` the output is produced in "ascii art"
using the charset specified by the global option `mixvlmc.charset`.

With the `latex` value, the output is produced in LaTeX, leveraging the
[forest](https://ctan.org/pkg/forest) LaTeX package (see
<https://ctan.org/pkg/forest>). Each call to `draw.covlmc()` produces a
full `forest` LaTeX environment. This can be included as is in a LaTeX
document, provided the `forest` package is loaded in the preamble of the
document. The LaTeX output is sanitised to avoid potential problems
induced by special characters in the names of the states of the context
tree.

## `"text"` format

### Parameters

When `format="text"` the parameters are interpreted as follows:

- `model`: the default `model="coef"` represents only the *coefficients*
  of the logistic models associated with each context. `model="full"`
  includes the names of the variables in the representation. Setting
  `model=NULL` removes the model representations. Additional parameters
  can be used to tweak model representations (see below).

- `constant_as_prob`: specifies whether to represent logistic models
  that do not use covariates (a.k.a. constant models) using the
  probability distributions they induce on the state space (default
  behaviour with `constant_as_prob=TRUE`) or as normal models (when set
  to `FALSE`). This is not taken into account when `model` is not set to
  `"coef"`.

- fields of the `control` list (including the charset):

  - `intercept`: character(s) used to represent the intercept when
    `model="full"`

  - `intercept_sep`: character(s) used to separate the intercept from
    the other coefficients in model representation.

  - `time_sep`: character(s) used to split the coefficients list by
    blocks associated with time delays in the covariate inclusion into
    the logistic model. The first block contains the intercept(s), the
    second block the covariate values at time t-1, the third block at
    time t-2, etc.

  - `level_sep`: character(s) used to separate levels from model, see
    below.

  - `open_p_value` and `close_p_value`: delimiters used around the
    p-values when `p_value=TRUE`

  - `open_model` and `close_model`: delimiters around the model when
    `model` is not `NULL`

### State representation

When `model` is not `NULL`, the coefficients of the logistic models are
presented, organised in rows associated with states. One state is used
as the reference state and the logistic model aims at predicting the
ratio of probability between another state and the reference one (in log
scale). When `with_state` is `TRUE`, the display includes for each row
of coefficients the target state. This is useful when using e.g.
[`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) as unused
levels of the target variable will be automatically dropped from the
model, leading to a reduced number of rows. The reference state is
either shown on the first row if `model` is `"full"` or after the state
on each row if `model` is `"coef"`. States are separated from the model
representation by the character(s) specified in `level_sep` in the
`control` list.

## `"latex"` format

### Parameters

When `format="latex"` the parameters are interpreted as follows:

- `model`: the models are always represented completely in the LaTeX
  export unless `model` is set to `NULL`.

- `constant_as_prob`: in the LaTeX export, constant logistic models are
  always represented by the corresponding probability distribution on
  the state space, regardless of the value of `constant_as_prob`.

- fields of the `control` list:

  - `orientation`: specifies the orientation of the tree, either the
    default `"vertical"` (expanding from top to bottom) or
    `"horizontal"` (expanding from right to left);

  - `tab_orientation`: specifies the orientation of the tables used to
    represent model coefficients in the tree, either the default
    `"vertical"` (covariates are listed on one column) or `"horizontal"`
    (covariates are listed on one row);

  - `fontsize` and `prob_fontsize` handle the size of the fonts used for
    the states and for the models, see
    [`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw_control.md)
    for details;

  - `decoration` can be used to add borders around states, see
    [`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw_control.md)
    for details;

### State representation

When `model` is not `NULL`, the coefficients of the logistic models are
presented, organised in rows or in columns (depending on
`tab_orientation`) associated with states. One state is used as the
reference state and the logistic model aims at predicting the ratio of
probability between another state and the reference one (in log scale).
When `with_state` is `TRUE`, the display includes for each row/column of
coefficients the target state. The reference state is shown on the first
row/column.

## Variable representation

When the representation includes the names of the variables used by the
logistic models, they are the ones generated by the underlying logistic
model, e.g. [`stats::glm()`](https://rdrr.io/r/stats/glm.html).
Numerical variable names are used as is, while factors have levels
appended. The intercept is denoted by the `intercept` member of the
`control` list when `format="text"` (as part of the charset). It is
always represented by `(I)` when `format="latex"`.

When `format="text"`, the time delays are represented by an underscore
followed by the time delay. For instance if the model uses the numerical
covariate `y` with two delays, it will appear with two variables `y_1`
and `y_2`.

When `format="latex"`, the representation uses a temporal subscript of
the form `t-1`, `t-2`, etc. and levels are given between square
brackets. For instance, the level `B` of a factor `y` with a delay of
one is represented by `y_1B` in text and by `y[B]` with subscript `t-1`
in LaTeX, while a logical covariate `day` with a delay of one is
represented by `day_1TRUE` and by `day[TRUE]` with subscript `t-1`.

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power,
  breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1)))
)
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
m_cov <- covlmc(rdts, rdts_cov, min_size = 5)
draw(m_cov, control = draw_control(digits = 3))
#> ▪
#> ├─ (0,1.34]
#> │  ├─ (0,1.34]
#> │  │  ├─ (0,1.34] ([ -2.81 • 2.18 ⁞ -4.45 ⁞ 2.42 ])
#> │  │  └─ (1.34,7.54] ([ -19.6 • 18.4 ])
#> │  └─ (1.34,7.54] (0.865, 0.135)
#> └─ (1.34,7.54] (0.0734, 0.927)
draw(m_cov, model = NULL)
#> ▪
#> ├─ (0,1.34]
#> │  ├─ (0,1.34]
#> │  │  ├─ (0,1.34]
#> │  │  └─ (1.34,7.54]
#> │  └─ (1.34,7.54]
#> └─ (1.34,7.54]
draw(m_cov, p_value = TRUE)
#> ▪
#> ├─ (0,1.34]
#> │  ├─ (0,1.34]
#> │  │  ├─ (0,1.34] (‹0.02329› [ -2.813 • 2.178 ⁞ -4.449 ⁞ 2.418 ])
#> │  │  └─ (1.34,7.54] (‹0.01834› [ -19.57 • 18.39 ])
#> │  └─ (1.34,7.54] (‹0.6763› 0.8649, 0.1351)
#> └─ (1.34,7.54] (‹0.8175› 0.07341, 0.9266)
draw(m_cov, p_value = FALSE, control = draw_control(digits = 2))
#> ▪
#> ├─ (0,1.34]
#> │  ├─ (0,1.34]
#> │  │  ├─ (0,1.34] ([ -2.8 • 2.2 ⁞ -4.4 ⁞ 2.4 ])
#> │  │  └─ (1.34,7.54] ([ -20 • 18 ])
#> │  └─ (1.34,7.54] (0.86, 0.14)
#> └─ (1.34,7.54] (0.073, 0.93)
draw(m_cov, model = "full", control = draw_control(digits = 3))
#> ▪
#> ├─ (0,1.34]
#> │  ├─ (0,1.34]
#> │  │  ├─ (0,1.34] ([ (I)   • day_night_1TRUE ⁞ day_night_2TRUE ⁞ day_night_3TRUE
#> │  │  │              -2.81 • 2.18            ⁞ -4.45           ⁞ 2.42            ])
#> │  │  └─ (1.34,7.54] ([ (I)   • day_night_1TRUE
#> │  │                    -19.6 • 18.4            ])
#> │  └─ (1.34,7.54] ([ (I)  
#> │                    -1.86 ])
#> └─ (1.34,7.54] ([ (I) 
#>                   2.54 ])
draw(m_cov, model = "full", with_state = TRUE)
#> ▪
#> ├─ (0,1.34]
#> │  ├─ (0,1.34]
#> │  │  ├─ (0,1.34] ([ ((0,1.34])  ~ (I)    • day_night_1TRUE ⁞ day_night_2TRUE ⁞ day_night_3TRUE
#> │  │  │              (1.34,7.54] ~ -2.813 • 2.178           ⁞ -4.449          ⁞ 2.418           ])
#> │  │  └─ (1.34,7.54] ([ ((0,1.34])  ~ (I)    • day_night_1TRUE
#> │  │                    (1.34,7.54] ~ -19.57 • 18.39           ])
#> │  └─ (1.34,7.54] ([ ((0,1.34])  ~ (I)   
#> │                    (1.34,7.54] ~ -1.856 ])
#> └─ (1.34,7.54] ([ ((0,1.34])  ~ (I)  
#>                   (1.34,7.54] ~ 2.535 ])
draw(m_cov, constant_as_prob = FALSE)
#> ▪
#> ├─ (0,1.34]
#> │  ├─ (0,1.34]
#> │  │  ├─ (0,1.34] ([ -2.813 • 2.178 ⁞ -4.449 ⁞ 2.418 ])
#> │  │  └─ (1.34,7.54] ([ -19.57 • 18.39 ])
#> │  └─ (1.34,7.54] ([ -1.856 ])
#> └─ (1.34,7.54] ([ 2.535 ])
draw(m_cov, format = "latex", control = draw_control(orientation = "h"))
#> \begin{forest}
#>   for tree={grow'=west}
#> [\textbf{$\epsilon$}
#> [\textbf{(0,1.34]}
#> [\textbf{(0,1.34]}
#> [\multicolumn{2}{c}{\textbf{(0,1.34]}}\\\hline
#> {\small (I)}&{\small -2.813}\\
#> {\small $\text{day\_night[TRUE]}_{t-1}$}&{\small 2.178}\\
#> {\small $\text{day\_night[TRUE]}_{t-2}$}&{\small -4.449}\\
#> {\small $\text{day\_night[TRUE]}_{t-3}$}&{\small 2.418}\\, align=cc]
#> [\multicolumn{2}{c}{\textbf{(1.34,7.54]}}\\\hline
#> {\small (I)}&{\small -19.57}\\
#> {\small $\text{day\_night[TRUE]}_{t-1}$}&{\small 18.39}\\, align=cc]]
#> [\multicolumn{2}{c}{\textbf{(1.34,7.54]}}\\\hline
#> {\small 0.8649}&{\small 0.1351}, align=cc]]
#> [\multicolumn{2}{c}{\textbf{(1.34,7.54]}}\\\hline
#> {\small 0.07341}&{\small 0.9266}, align=cc]]
#> \end{forest}
```
