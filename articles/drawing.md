# Drawing context trees and models

``` r

library(mixvlmc)
```

This article describes how to represent context trees, variable length
Markov chains (VLMC) and VLMC with covariates as trees, using the
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
function.
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
produces either an “ascii art” representation, well suited to the R
console, or a LaTeX representation of the tree, to be included in a
LaTeX document or in an R Markdown or Quarto document that produces a
PDF.

[`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md) is
a generic function with methods for the three types of objects:
[`draw.ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.ctx_tree.md)
for context trees,
[`draw.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.vlmc.md)
for VLMC and
[`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md)
for VLMC with covariates. Context trees and VLMC are drawn in the same
way whatever their backend (the default C++ backend or the R one, see
[`ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/reference/ctx_tree.md)
and
[`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/vlmc.md)).
All the methods share the same organisation:

- the `format` argument (the second one) selects the output format,
  either `"text"` (the default) or `"latex"`;
- additional arguments specific to each method specify *what*
  information is shown in each node of the tree (frequencies,
  probabilities, models, etc.);
- the `control` argument, built with
  [`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw_control.md),
  specifies *how* this information is shown (characters used to draw the
  tree, number of significant digits, LaTeX layout, etc.).

The methods report an error when they are given an argument they do not
use, for instance an option of
[`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw_control.md)
given directly to
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md).

This organisation of the arguments was introduced in mixvlmc 0.3.0.
Calls written for previous versions, in which the second argument of
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
was the control list, still work but trigger a deprecation warning.

The models used in this article are kept very simple: see
[`vignette("context-trees")`](https://fabrice-rossi.github.io/mixvlmc/articles/context-trees.md),
[`vignette("variable-length-markov-chains")`](https://fabrice-rossi.github.io/mixvlmc/articles/variable-length-markov-chains.md)
and
[`vignette("covlmc")`](https://fabrice-rossi.github.io/mixvlmc/articles/covlmc.md)
for the construction and the interpretation of context trees and models.

## Context trees

### Basic representation

We use a small random sequence on three states.

``` r

set.seed(0)
x <- sample(c("a", "b", "c"), 100, replace = TRUE)
x_ctx <- ctx_tree(x, min_size = 10, max_depth = 2)
draw(x_ctx)
#> ▪
#> ├─ a
#> │  ├─ a
#> │  └─ b
#> ├─ b
#> │  ├─ b
#> │  └─ c
#> └─ c
#>    └─ a
```

The root of the tree (the empty context) is shown by a small square.
Each node of the first level corresponds to the most recent state of a
context (\\x\_{t-1}\\), each node of the second level to the state
before it (\\x\_{t-2}\\), etc. A context is therefore read from its node
to the root in chronological order. For instance, the leaf `b` under the
node `a` stands for the context in which `b` is followed by `a`, denoted
`b, a` by
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.md):

``` r

contexts(x_ctx)
#> Contexts:
#>  a, a
#>  b, a
#>  a
#>  b, b
#>  c, b
#>  b
#>  a, c
#>  c
```

### Frequencies

The `frequency` argument of
[`draw.ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.ctx_tree.md)
adds the frequencies of the (partial) contexts to the representation.
With `frequency = "total"`, each node shows the number of occurrences of
its context in the sequence (only the occurrences followed by at least
one state are taken into account, see
[`vignette("context-trees")`](https://fabrice-rossi.github.io/mixvlmc/articles/context-trees.md)).

``` r

draw(x_ctx, frequency = "total")
#> ▪ (100)
#> ├─ a (32)
#> │  ├─ a (15)
#> │  └─ b (10)
#> ├─ b (38)
#> │  ├─ b (19)
#> │  └─ c (12)
#> └─ c (29)
#>    └─ a (11)
```

With `frequency = "detailed"`, each node shows the number of occurrences
of each state that follows its context, in the order of the state space
given by
[`states()`](https://fabrice-rossi.github.io/mixvlmc/reference/states.md)
(here a, b, c).

``` r

draw(x_ctx, frequency = "detailed")
#> ▪ (32, 38, 30)
#> ├─ a (15, 6, 11)
#> │  ├─ a (7, 4, 4)
#> │  └─ b (4, 1, 5)
#> ├─ b (10, 19, 9)
#> │  ├─ b (3, 10, 6)
#> │  └─ c (3, 6, 3)
#> └─ c (7, 12, 10)
#>    └─ a (3, 5, 3)
```

This is the same information as the one given by
[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.md)
with the same `frequency` argument, organised as a tree.

## Variable length Markov chains

### Probabilities and frequencies

Each context of a VLMC is associated with a conditional distribution of
the next state.
[`draw.vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.vlmc.md)
shows those distributions by default. We use as an example a VLMC fitted
to a discretised version of the active power of week 5 of the household
electricity consumption data set (`powerconsumption`). The model is
selected with the BIC (see
[`vignette("variable-length-markov-chains")`](https://fabrice-rossi.github.io/mixvlmc/articles/variable-length-markov-chains.md)
and
[`vignette("model-selection")`](https://fabrice-rossi.github.io/mixvlmc/articles/model-selection.md)).

``` r

pc_week_5 <- powerconsumption[powerconsumption$week == 5, ]
elec_dts <- cut(pc_week_5$active_power,
  breaks = c(0, 0.4, 2, 8),
  labels = c("low", "typical", "high")
)
elec_vlmc <- as_vlmc(tune_vlmc(elec_dts))
draw(elec_vlmc)
#> ▪ (0.1667, 0.5496, 0.2837)
#> ├─ low (0.7665, 0.2335, 0)
#> ├─ typical (0.0704, 0.8466, 0.08303)
#> │  └─ low (0.3846, 0.5385, 0.07692)
#> └─ high (0.003497, 0.1573, 0.8392)
```

The probabilities are given in the order of the state space, here:

``` r

states(elec_vlmc)
#> [1] low     typical high   
#> Levels: low typical high
```

The root node gives the marginal distribution of the states in the
sequence, while the other nodes give the distribution of the next state
conditionally on their (partial) context.

The `prob` argument controls this display: `prob = FALSE` replaces the
probabilities by the frequencies of the states that follow each
(partial) context (as `frequency = "detailed"` in
[`draw.ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.ctx_tree.md)),
while `prob = NULL` removes all the information.

``` r

draw(elec_vlmc, prob = FALSE)
#> ▪ (168, 554, 286)
#> ├─ low (128, 39, 0)
#> ├─ typical (39, 469, 46)
#> │  └─ low (15, 21, 3)
#> └─ high (1, 45, 240)
draw(elec_vlmc, prob = NULL)
#> ▪
#> ├─ low
#> ├─ typical
#> │  └─ low
#> └─ high
```

### Number of digits

Probabilities (and other numerical values) are rounded with
[`signif()`](https://rdrr.io/r/base/Round.html) to the number of
significant digits given by the `digits` parameter of
[`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw_control.md)
(4 by default).

``` r

draw(elec_vlmc, control = draw_control(digits = 2))
#> ▪ (0.17, 0.55, 0.28)
#> ├─ low (0.77, 0.23, 0)
#> ├─ typical (0.07, 0.85, 0.083)
#> │  └─ low (0.38, 0.54, 0.077)
#> └─ high (0.0035, 0.16, 0.84)
```

## Text representations

### Character sets

The “ascii art” representation uses a collection of characters, the
*charset*, to draw the branches of the tree and to delimit the
information attached to the nodes. The package provides two charsets:

- `"utf8"`: the charset returned by
  [`charset_utf8()`](https://fabrice-rossi.github.io/mixvlmc/reference/charset_utf8.md),
  based on UTF-8 symbols (used in this article);
- `"ascii"`: the charset returned by
  [`charset_ascii()`](https://fabrice-rossi.github.io/mixvlmc/reference/charset_ascii.md),
  based on standard ASCII characters only.

The default charset is given by the `mixvlmc.charset` option. When the
package is loaded, this option is set to `"utf8"` if the output supports
UTF-8 characters (according to
[`cli::is_utf8_output()`](https://cli.r-lib.org/reference/is_utf8_output.html))
and to `"ascii"` otherwise (unless the option was already set). The
charset can be changed for a single call with the `charset` parameter of
[`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw_control.md):

``` r

draw(x_ctx, control = draw_control(charset = "ascii"))
#> *
#> +-- a
#> |   +-- a
#> |   '-- b
#> +-- b
#> |   +-- b
#> |   '-- c
#> '-- c
#>     '-- a
```

or for all subsequent calls by setting the option:

``` r

options(mixvlmc.charset = "ascii")
draw(x_ctx, frequency = "total")
#> * (100)
#> +-- a (32)
#> |   +-- a (15)
#> |   '-- b (10)
#> +-- b (38)
#> |   +-- b (19)
#> |   '-- c (12)
#> '-- c (29)
#>     '-- a (11)
options(mixvlmc.charset = "utf8")
```

### Custom charsets

[`charset_utf8()`](https://fabrice-rossi.github.io/mixvlmc/reference/charset_utf8.md)
and
[`charset_ascii()`](https://fabrice-rossi.github.io/mixvlmc/reference/charset_ascii.md)
return lists of character strings, one per element of the
representation. All their arguments can be modified to obtain a custom
charset, which is then given to
[`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw_control.md):

``` r

str(charset_ascii())
#> List of 16
#>  $ root         : chr "*"
#>  $ first_node   : chr "+"
#>  $ next_node    : chr "+"
#>  $ final_node   : chr "'"
#>  $ vbranch      : chr "|"
#>  $ hbranch      : chr "--"
#>  $ open_ct      : chr "("
#>  $ close_ct     : chr ")"
#>  $ level_sep    : chr " ~ "
#>  $ time_sep     : chr " | "
#>  $ intercept    : chr "(I)"
#>  $ intercept_sep: chr " & "
#>  $ open_p_value : chr "<"
#>  $ close_p_value: chr ">"
#>  $ open_model   : chr "["
#>  $ close_model  : chr "]"
```

The first elements are used for all the trees:

- `root`: the symbol of the root node;
- `first_node`, `next_node` and `final_node`: the symbols that introduce
  the first child of a node, its intermediate children and its last
  child;
- `vbranch` and `hbranch`: the vertical and horizontal parts of the
  branches;
- `open_ct` and `close_ct`: the delimiters of the information attached
  to a node (frequencies, probabilities, models).

For instance:

``` r

my_charset <- charset_ascii(
  root = "@",
  final_node = "+",
  hbranch = "==",
  open_ct = "<",
  close_ct = ">"
)
draw(x_ctx,
  frequency = "detailed",
  control = draw_control(charset = my_charset)
)
#> @ <32, 38, 30>
#> +== a <15, 6, 11>
#> |   +== a <7, 4, 4>
#> |   +== b <4, 1, 5>
#> +== b <10, 19, 9>
#> |   +== b <3, 10, 6>
#> |   +== c <3, 6, 3>
#> +== c <7, 12, 10>
#>     +== a <3, 5, 3>
```

The other elements (`level_sep`, `time_sep`, `intercept`,
`intercept_sep`, `open_p_value`, `close_p_value`, `open_model` and
`close_model`) are used only by
[`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md)
and are described below.

## VLMC with covariates

### A two-state model

A VLMC with covariates (see
[`vignette("covlmc")`](https://fabrice-rossi.github.io/mixvlmc/articles/covlmc.md))
associates to each context a logistic model that gives the conditional
distribution of the next state as a function of past values of the
covariates.
[`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md)
represents those models in the tree. We start with a two-state sequence
and a single logical covariate.

``` r

elec_dts_2 <- cut(pc_week_5$active_power,
  breaks = c(0, median(pc_week_5$active_power), 8),
  labels = c("low", "high")
)
elec_cov <- data.frame(day = (pc_week_5$hour >= 7 & pc_week_5$hour <= 17))
elec_covlmc <- covlmc(elec_dts_2, elec_cov, min_size = 5)
draw(elec_covlmc)
#> ▪
#> ├─ low
#> │  ├─ low
#> │  │  ├─ low ([ -2.813 • 2.178 ⁞ -4.449 ⁞ 2.418 ])
#> │  │  └─ high ([ -19.57 • 18.39 ])
#> │  └─ high (0.8649, 0.1351)
#> └─ high (0.07341, 0.9266)
```

Two types of models appear in the tree:

- *constant models* do not depend on the covariates. By default, they
  are represented by the conditional distribution of the next state that
  they induce, as for a VLMC (in the order of `states(elec_covlmc)`);
- other models are represented between brackets by their coefficients.
  With a logistic model, the coefficients describe the logarithm of the
  ratio between the probability of the second state (here `high`) and
  the probability of the first state (here `low`, the *reference*
  state). The intercept comes first, followed by the coefficients of the
  covariates at time \\t-1\\, then at time \\t-2\\, etc. The intercept
  is separated from the other coefficients by the `intercept_sep`
  element of the charset (a bullet with UTF-8 characters) and
  consecutive time steps are separated by the `time_sep` element (a
  vertical dotted line with UTF-8 characters).

For instance, the model of the context made of three consecutive `low`
states (the deepest `low` leaf) has an intercept followed by one
coefficient for each of the values of `day` at times \\t-1\\, \\t-2\\
and \\t-3\\, while the model of its sibling `high` only uses the value
of `day` at time \\t-1\\.

Setting `constant_as_prob = FALSE` represents constant models by their
coefficients, that is by their intercept only:

``` r

draw(elec_covlmc, constant_as_prob = FALSE)
#> ▪
#> ├─ low
#> │  ├─ low
#> │  │  ├─ low ([ -2.813 • 2.178 ⁞ -4.449 ⁞ 2.418 ])
#> │  │  └─ high ([ -19.57 • 18.39 ])
#> │  └─ high ([ -1.856 ])
#> └─ high ([ 2.535 ])
```

The probabilities shown in the default representation are obtained from
the intercepts with the logistic function. With `model = "full"`, the
names of the variables are shown above the coefficients (and constant
models are always represented by their coefficients):

``` r

draw(elec_covlmc, model = "full")
#> ▪
#> ├─ low
#> │  ├─ low
#> │  │  ├─ low ([ (I)    • day_1TRUE ⁞ day_2TRUE ⁞ day_3TRUE
#> │  │  │         -2.813 • 2.178     ⁞ -4.449    ⁞ 2.418     ])
#> │  │  └─ high ([ (I)    • day_1TRUE
#> │  │             -19.57 • 18.39     ])
#> │  └─ high ([ (I)   
#> │             -1.856 ])
#> └─ high ([ (I)  
#>            2.535 ])
```

The names of the variables are generated by the underlying logistic
model and include the time delay: for instance `day_1TRUE` stands for
the value `TRUE` of the covariate `day` at time \\t-1\\. The intercept
is denoted by the `intercept` element of the charset (`(I)` by default).

With `with_state = TRUE`, each row of coefficients is preceded by the
state it predicts. With `model = "coef"` (the default), the state is
followed by the reference state, while with `model = "full"` the
reference state is shown, in parentheses, on the row of the variable
names. States are separated from the coefficients by the `level_sep`
element of the charset. Constant models represented by probabilities are
not modified.

``` r

draw(elec_covlmc, with_state = TRUE)
#> ▪
#> ├─ low
#> │  ├─ low
#> │  │  ├─ low ([ high/low ~ -2.813 • 2.178 ⁞ -4.449 ⁞ 2.418 ])
#> │  │  └─ high ([ high/low ~ -19.57 • 18.39 ])
#> │  └─ high (0.8649, 0.1351)
#> └─ high (0.07341, 0.9266)
draw(elec_covlmc, model = "full", with_state = TRUE)
#> ▪
#> ├─ low
#> │  ├─ low
#> │  │  ├─ low ([ (low) ~ (I)    • day_1TRUE ⁞ day_2TRUE ⁞ day_3TRUE
#> │  │  │         high  ~ -2.813 • 2.178     ⁞ -4.449    ⁞ 2.418     ])
#> │  │  └─ high ([ (low) ~ (I)    • day_1TRUE
#> │  │             high  ~ -19.57 • 18.39     ])
#> │  └─ high ([ (low) ~ (I)   
#> │             high  ~ -1.856 ])
#> └─ high ([ (low) ~ (I)  
#>            high  ~ 2.535 ])
```

This option is particularly useful when some states never follow a
context: they are then removed from its logistic model, which has fewer
rows of coefficients than expected (see examples in
[`vignette("covlmc")`](https://fabrice-rossi.github.io/mixvlmc/articles/covlmc.md)
and in the article on [computing
engines](https://fabrice-rossi.github.io/mixvlmc/articles/covlmc-engines.md)).

### More states and covariates

With more than two states, the logistic models have one row of
coefficients for each state but the reference one. Moreover, each time
step can be associated with several coefficients: a numerical or logical
covariate has one coefficient per time step, while a factor has one
coefficient per level, except the first one. We illustrate this with a
simulated sequence on three states and a factor covariate with three
levels.

``` r

set.seed(0)
z <- sample(c("A", "B", "C"), 1000, replace = TRUE)
y <- ifelse(runif(length(z)) > 0.5,
  c(z[-1], sample(c("A", "B", "C"), 1)),
  c(z[-c(1, 2)], sample(c("A", "B", "C"), 2, replace = TRUE))
)
y <- as.factor(ifelse(runif(length(z)) > 0.2, y,
  sample(c("A", "B", "C"), length(z), replace = TRUE)
))
z_covlmc <- covlmc(z, data.frame(y = y), min_size = 1.5, alpha = 0.01)
draw(z_covlmc, model = "full", with_state = TRUE)
#> ▪
#> ├─ A
#> │  ├─ A ([ (A) ~ (I)    • y_1B   y_1C    ⁞ y_2B   y_2C 
#> │  │       B   ~ -1.393 • 1.966  -0.2701 ⁞ 1.957  1.173
#> │  │       C   ~ -1.296 • 0.7838 1.216   ⁞ -0.942 2.738 ])
#> │  ├─ B ([ (A) ~ (I)    • y_1B  y_1C   ⁞ y_2B   y_2C 
#> │  │       B   ~ -2.428 • 2.935 0.2797 ⁞ 2.627  -15  
#> │  │       C   ~ -1.588 • 1.07  1.347  ⁞ 0.8597 3.026 ])
#> │  └─ C ([ (A) ~ (I)    • y_1B   y_1C   ⁞ y_2B     y_2C   
#> │          B   ~ -1.083 • 1.779  0.5446 ⁞ 1.275    -0.5394
#> │          C   ~ -1.469 • 0.3142 2.158  ⁞ -0.02489 2.407   ])
#> ├─ B
#> │  ├─ A ([ (A) ~ (I)    • y_1B   y_1C  ⁞ y_2B  y_2C 
#> │  │       B   ~ -3.238 • 2.005  0.938 ⁞ 2.525 2.54 
#> │  │       C   ~ -3.142 • 0.5409 2.655 ⁞ 1.66  3.751 ])
#> │  ├─ B
#> │  │  ├─ C ([ (A) ~ (I)    • y_1B  y_1C   ⁞ y_2B  y_2C  ⁞ y_3B    y_3C 
#> │  │  │       B   ~ -15.19 • 1.787 0.6895 ⁞ 15.16 6.893 ⁞ 0.04448 6.859
#> │  │  │       C   ~ -40.68 • 13.8  28.11  ⁞ 8.893 17.34 ⁞ 4.27    32.35 ])
#> │  │  └─ A, B ([ (A) ~ (I)    • y_1B  y_1C    ⁞ y_2B  y_2C 
#> │  │             B   ~ -1.903 • 2.478 -0.1105 ⁞ 1.416 1.977
#> │  │             C   ~ -3.479 • 1.097 1.725   ⁞ 2.142 4.777 ])
#> │  └─ C ([ (A) ~ (I)    • y_1B  y_1C  ⁞ y_2B  y_2C 
#> │          B   ~ -4.997 • 4.323 3.73  ⁞ 2.829 4.266
#> │          C   ~ -22.39 • 4.75  5.124 ⁞ 18.4  22.28 ])
#> └─ C
#>    ├─ A ([ (A) ~ (I)    • y_1B   y_1C  ⁞ y_2B  y_2C 
#>    │       B   ~ -3.164 • 1.954  1.545 ⁞ 4.954 2.35 
#>    │       C   ~ -2.597 • 0.9274 1.851 ⁞ 1.814 2.125 ])
#>    ├─ B ([ (A) ~ (I)    • y_1B  y_1C  ⁞ y_2B     y_2C 
#>    │       B   ~ -2.166 • 1.553 1.428 ⁞ 2.332    1.055
#>    │       C   ~ -2.165 • 1.206 2.875 ⁞ -0.07224 1.419 ])
#>    └─ C ([ (A) ~ (I)    • y_1B   y_1C   ⁞ y_2B  y_2C 
#>            B   ~ -3.412 • 0.8723 0.9043 ⁞ 5.503 2.849
#>            C   ~ -4.026 • 0.8833 2.359  ⁞ 4.067 3.186 ])
```

In this representation, `y_1B` and `y_1C` stand for the levels `B` and
`C` of the covariate `y` at time \\t-1\\, while `y_2B` and `y_2C` stand
for the same levels at time \\t-2\\, etc.

The tree also shows a *merged* context: the leaf labelled `A, B` under
the node `B` of the second level represents the two contexts `A, B, B`
and `B, B, B` (in chronological order), which share a single logistic
model. The fitting algorithm of
[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
merges contexts in this way when their models are not significantly
different (see
[`vignette("covlmc")`](https://fabrice-rossi.github.io/mixvlmc/articles/covlmc.md),
and
[`vignette("dts-and-contexts")`](https://fabrice-rossi.github.io/mixvlmc/articles/dts-and-contexts.md)
for the exploration of merged contexts).

### p-values

During the construction of the model,
[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
conducts a series of likelihood ratio tests. Their p-values are included
in the representation when `p_value = TRUE`:

``` r

draw(z_covlmc, p_value = TRUE)
#> ▪
#> ├─ A
#> │  ├─ A (‹6.085e-09› [ -1.393 • 1.966  -0.2701 ⁞ 1.957  1.173
#> │  │                   -1.296 • 0.7838 1.216   ⁞ -0.942 2.738 ])
#> │  ├─ B (‹8.921e-10› [ -2.428 • 2.935 0.2797 ⁞ 2.627  -15  
#> │  │                   -1.588 • 1.07  1.347  ⁞ 0.8597 3.026 ])
#> │  └─ C (‹1.23e-05› [ -1.083 • 1.779  0.5446 ⁞ 1.275    -0.5394
#> │                     -1.469 • 0.3142 2.158  ⁞ -0.02489 2.407   ])
#> ├─ B
#> │  ├─ A (‹4.786e-05› [ -3.238 • 2.005  0.938 ⁞ 2.525 2.54 
#> │  │                   -3.142 • 0.5409 2.655 ⁞ 1.66  3.751 ])
#> │  ├─ B
#> │  │  ├─ C (‹0.00761› [ -15.19 • 1.787 0.6895 ⁞ 15.16 6.893 ⁞ 0.04448 6.859
#> │  │  │                 -40.68 • 13.8  28.11  ⁞ 8.893 17.34 ⁞ 4.27    32.35 ])
#> │  │  └─ A, B (‹0.000705› [ -1.903 • 2.478 -0.1105 ⁞ 1.416 1.977
#> │  │                        -3.479 • 1.097 1.725   ⁞ 2.142 4.777 ])
#> │  └─ C (‹3.541e-11› [ -4.997 • 4.323 3.73  ⁞ 2.829 4.266
#> │                      -22.39 • 4.75  5.124 ⁞ 18.4  22.28 ])
#> └─ C
#>    ├─ A (‹3.459e-09› [ -3.164 • 1.954  1.545 ⁞ 4.954 2.35 
#>    │                   -2.597 • 0.9274 1.851 ⁞ 1.814 2.125 ])
#>    ├─ B (‹0.0009756› [ -2.166 • 1.553 1.428 ⁞ 2.332    1.055
#>    │                   -2.165 • 1.206 2.875 ⁞ -0.07224 1.419 ])
#>    └─ C (‹3.65e-08› [ -3.412 • 0.8723 0.9043 ⁞ 5.503 2.849
#>                       -4.026 • 0.8833 2.359  ⁞ 4.067 3.186 ])
```

The p-value attached to a model, shown between the `open_p_value` and
`close_p_value` delimiters of the charset, is associated with the test
that compares the model to a model with a shorter memory of the
covariates. Internal nodes can show p-values associated with the merging
of some of their children (`merging`) or with the removal of all their
children (`collapsing`), as explained in
[`vignette("covlmc")`](https://fabrice-rossi.github.io/mixvlmc/articles/covlmc.md).
The models can be removed from the representation with `model = NULL`,
which gives a compact view of the p-values:

``` r

draw(z_covlmc, model = NULL, p_value = TRUE)
#> ▪
#> ├─ A
#> │  ├─ A (6.085e-09)
#> │  ├─ B (8.921e-10)
#> │  └─ C (1.23e-05)
#> ├─ B
#> │  ├─ A (4.786e-05)
#> │  ├─ B
#> │  │  ├─ C (0.00761)
#> │  │  └─ A, B (0.000705)
#> │  └─ C (3.541e-11)
#> └─ C
#>    ├─ A (3.459e-09)
#>    ├─ B (0.0009756)
#>    └─ C (3.65e-08)
```

### Customising the model representation

The elements of the charset dedicated to
[`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md)
can be modified as the other ones. For instance, the following charset
uses `+` between the intercept and the other coefficients, `;` between
time steps, `:` between the states and the coefficients, and braces
around the models:

``` r

cov_charset <- charset_utf8(
  intercept_sep = " + ",
  time_sep = " ; ",
  level_sep = " : ",
  open_model = "{",
  close_model = "}"
)
draw(elec_covlmc,
  model = "full", with_state = TRUE,
  control = draw_control(charset = cov_charset, digits = 3)
)
#> ▪
#> ├─ low
#> │  ├─ low
#> │  │  ├─ low ({ (low) : (I)   + day_1TRUE ; day_2TRUE ; day_3TRUE
#> │  │  │         high  : -2.81 + 2.18      ; -4.45     ; 2.42      })
#> │  │  └─ high ({ (low) : (I)   + day_1TRUE
#> │  │             high  : -19.6 + 18.4      })
#> │  └─ high ({ (low) : (I)  
#> │             high  : -1.86 })
#> └─ high ({ (low) : (I) 
#>            high  : 2.54 })
```

## The result of `draw()`

[`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
does not print anything by itself: it returns an object of class
`mixvlmc_draw` that contains the representation of the tree, one line
per element.

``` r

x_draw <- draw(x_ctx)
class(x_draw)
#> [1] "mixvlmc_draw"
```

As for other R objects, this object is printed automatically when the
result of
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md) is
not assigned to a variable at top level, which is why calling
`draw(x_ctx)` in the console (or in a chunk of an R Markdown document)
displays the tree. Inside a function or a loop, automatic printing does
not happen and the representation must be printed explicitly with
[`print()`](https://rdrr.io/r/base/print.html):

``` r

for (depth in 1:2) {
  print(draw(ctx_tree(x, min_size = 10, max_depth = depth)))
}
#> ▪
#> ├─ a
#> ├─ b
#> └─ c
#> ▪
#> ├─ a
#> │  ├─ a
#> │  └─ b
#> ├─ b
#> │  ├─ b
#> │  └─ c
#> └─ c
#>    └─ a
```

The lines of the representation are obtained as a character vector with
[`format()`](https://rdrr.io/r/base/format.html) or
[`as.character()`](https://rdrr.io/r/base/character.html):

``` r

as.character(x_draw)
#> [1] "▪"       "├─ a"    "│  ├─ a" "│  └─ b" "├─ b"    "│  ├─ b" "│  └─ c"
#> [8] "└─ c"    "   └─ a"
```

They can be written to a file with
[`writeLines()`](https://rdrr.io/r/base/writeLines.html), for instance
to include a LaTeX representation in a document (see below):

``` r

tree_file <- tempfile(fileext = ".tex")
writeLines(format(draw(x_ctx, "latex")), tree_file)
```

## LaTeX representations

### The forest package

With `format = "latex"`,
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
produces LaTeX code based on the [forest](https://ctan.org/pkg/forest)
LaTeX package. Each call produces a complete `forest` environment in
which each node is described between square brackets, the children of a
node being nested in its description. The root of the tree is
represented by \\\epsilon\\ (the empty context). In this HTML article,
the code is shown as standard R output:

``` r

draw(x_ctx, "latex")
#> \begin{forest}
#> [\textbf{$\epsilon$}
#> [\textbf{a}
#> [\textbf{a}]
#> [\textbf{b}]]
#> [\textbf{b}
#> [\textbf{b}]
#> [\textbf{c}]]
#> [\textbf{c}
#> [\textbf{a}]]]
#> \end{forest}
```

The additional arguments of the methods have the same meaning as in the
text format. Node information is represented by default in a small
table, with the label of the node on a first row and its frequencies,
probabilities or model on the following one(s):

``` r

draw(x_ctx, "latex", frequency = "detailed")
#> \begin{forest}
#> [\multicolumn{3}{c}{\textbf{$\epsilon$}}\\\hline
#> {\small 32}&{\small 38}&{\small 30}, align=ccc
#> [\multicolumn{3}{c}{\textbf{a}}\\\hline
#> {\small 15}&{\small 6}&{\small 11}, align=ccc
#> [\multicolumn{3}{c}{\textbf{a}}\\\hline
#> {\small 7}&{\small 4}&{\small 4}, align=ccc]
#> [\multicolumn{3}{c}{\textbf{b}}\\\hline
#> {\small 4}&{\small 1}&{\small 5}, align=ccc]]
#> [\multicolumn{3}{c}{\textbf{b}}\\\hline
#> {\small 10}&{\small 19}&{\small 9}, align=ccc
#> [\multicolumn{3}{c}{\textbf{b}}\\\hline
#> {\small 3}&{\small 10}&{\small 6}, align=ccc]
#> [\multicolumn{3}{c}{\textbf{c}}\\\hline
#> {\small 3}&{\small 6}&{\small 3}, align=ccc]]
#> [\multicolumn{3}{c}{\textbf{c}}\\\hline
#> {\small 7}&{\small 12}&{\small 10}, align=ccc
#> [\multicolumn{3}{c}{\textbf{a}}\\\hline
#> {\small 3}&{\small 5}&{\small 3}, align=ccc]]]
#> \end{forest}
```

The names of the states are sanitised to avoid problems induced by LaTeX
special characters:

``` r

x_special <- sample(c("$1", "a_b", "{c}"), 100, replace = TRUE)
draw(ctx_tree(x_special, max_depth = 1), "latex")
#> \begin{forest}
#> [\textbf{$\epsilon$}
#> [\textbf{\{c\}}]
#> [\textbf{\$1}]
#> [\textbf{a\_b}]]
#> \end{forest}
```

### Customising the LaTeX output

The following parameters of
[`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw_control.md)
apply to the LaTeX format:

- `tabular`: when `FALSE`, the information attached to a node is given
  after its label, between parentheses, instead of in a table (this
  parameter is ignored by
  [`draw.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.covlmc.md),
  which always uses tables);
- `orientation`: `"vertical"` (default) draws the tree from top to
  bottom, while `"horizontal"` draws it from right to left;
- `decoration`: `"none"` (default), `"rectangle"`, `"circle"` or
  `"ellipse"` adds borders of the corresponding shape around all the
  nodes;
- `fontsize` and `prob_fontsize`: the LaTeX font sizes (such as
  `"small"` or `"large"`) of the labels of the nodes and of the
  additional information (`"normalsize"` and `"small"` by default);
- `digits`: the number of significant digits, as for the text format.

``` r

draw(elec_vlmc, "latex",
  control = draw_control(
    tabular = FALSE,
    orientation = "horizontal",
    decoration = "rectangle",
    fontsize = "small",
    prob_fontsize = "footnotesize",
    digits = 2
  )
)
#> \begin{small}
#> \begin{forest}
#>   for tree={draw, grow'=west}
#> [{\textbf{$\epsilon$} {\footnotesize (0.17,0.55,0.28)}}
#> [{\textbf{low} {\footnotesize (0.77,0.23,0)}}]
#> [{\textbf{typical} {\footnotesize (0.07,0.85,0.083)}}
#> [{\textbf{low} {\footnotesize (0.38,0.54,0.077)}}]]
#> [{\textbf{high} {\footnotesize (0.0035,0.16,0.84)}}]]
#> \end{forest}
#> \end{small}
```

### VLMC with covariates

The LaTeX representation of a VLMC with covariates differs from its text
counterpart:

- models are always represented with the names of the variables, unless
  `model = NULL` (`"coef"` and `"full"` are equivalent);
- constant models are always represented by the probability distribution
  they induce (`constant_as_prob` is ignored);
- the intercept is denoted `(I)` and the variables are given with a
  temporal subscript of the form \\t-1\\, \\t-2\\, etc. They are named
  after the covariates, with the level between square brackets for a
  factor or a logical covariate, as in `y[B]` or `day[TRUE]`;
- the coefficients of the levels of a factor that are absent from the
  data of a context, which are not estimated, are represented by zeros,
  whatever the computing engine (in the text format,
  [`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) gives no
  coefficient for them, see the article on [computing
  engines](https://fabrice-rossi.github.io/mixvlmc/articles/covlmc-engines.md));
- p-values (with `p_value = TRUE`) are given in parentheses after the
  labels of the nodes.

The `tab_orientation` parameter of
[`draw_control()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw_control.md)
specifies the layout of the tables that represent the models. By default
(`"vertical"`), the variables are listed in the first column, followed
by one column of coefficients per non-reference state, which is well
adapted to models with a long memory of the covariates. With
`with_state = TRUE`, the first row gives the reference state (above the
variables) and the other states (above their coefficients):

``` r

draw(elec_covlmc, "latex", with_state = TRUE)
#> \begin{forest}
#> [\textbf{$\epsilon$}
#> [\textbf{low}
#> [\textbf{low}
#> [\multicolumn{2}{c}{\textbf{low}}\\\hline
#> {\small low}&\textbf{{\small high}}\\{\small (I)}&{\small -2.813}\\
#> {\small $\text{day[TRUE]}_{t-1}$}&{\small 2.178}\\
#> {\small $\text{day[TRUE]}_{t-2}$}&{\small -4.449}\\
#> {\small $\text{day[TRUE]}_{t-3}$}&{\small 2.418}\\, align=cc]
#> [\multicolumn{2}{c}{\textbf{high}}\\\hline
#> {\small low}&\textbf{{\small high}}\\{\small (I)}&{\small -19.57}\\
#> {\small $\text{day[TRUE]}_{t-1}$}&{\small 18.39}\\, align=cc]]
#> [\multicolumn{2}{c}{\textbf{high}}\\\hline
#> {\small 0.8649}&{\small 0.1351}, align=cc]]
#> [\multicolumn{2}{c}{\textbf{high}}\\\hline
#> {\small 0.07341}&{\small 0.9266}, align=cc]]
#> \end{forest}
```

With `tab_orientation = "horizontal"`, the variables are listed in the
first row, followed by one row of coefficients per non-reference state
(the reference state and the other states are then given in the first
column when `with_state = TRUE`):

``` r

draw(elec_covlmc, "latex",
  with_state = TRUE,
  control = draw_control(tab_orientation = "horizontal")
)
#> \begin{forest}
#> [\textbf{$\epsilon$}
#> [\textbf{low}
#> [\textbf{low}
#> [\multicolumn{5}{c}{\textbf{low}}\\\hline
#> {\small low}&{\small (I)}&{\small $\text{day[TRUE]}_{t-1}$}&{\small $\text{day[TRUE]}_{t-2}$}&{\small $\text{day[TRUE]}_{t-3}$}\\
#> \textbf{{\small high}}&{\small -2.813}&{\small 2.178}&{\small -4.449}&{\small 2.418}, align=ccccc]
#> [\multicolumn{3}{c}{\textbf{high}}\\\hline
#> {\small low}&{\small (I)}&{\small $\text{day[TRUE]}_{t-1}$}\\
#> \textbf{{\small high}}&{\small -19.57}&{\small 18.39}, align=ccc]]
#> [\multicolumn{2}{c}{\textbf{high}}\\\hline
#> {\small 0.8649}&{\small 0.1351}, align=cc]]
#> [\multicolumn{2}{c}{\textbf{high}}\\\hline
#> {\small 0.07341}&{\small 0.9266}, align=cc]]
#> \end{forest}
```

With three states or more, the tables have several columns (or rows) of
coefficients, as illustrated below with a simpler version of the
three-state model used above, whose contexts are limited to a length of
one:

``` r

z_covlmc_1 <- covlmc(z, data.frame(y = y),
  min_size = 1.5, alpha = 0.01, max_depth = 1
)
draw(z_covlmc_1, "latex", with_state = TRUE)
#> \begin{forest}
#> [\textbf{$\epsilon$}
#> [\multicolumn{3}{c}{\textbf{A}}\\\hline
#> {\small A}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -1.099}&{\small -0.8755}\\
#> {\small $\text{y[B]}_{t-1}$}&{\small 2.227}&{\small 0.9199}\\
#> {\small $\text{y[C]}_{t-1}$}&{\small 0.3102}&{\small 1.598}\\
#> , align=ccc]
#> [\multicolumn{3}{c}{\textbf{B}}\\\hline
#> {\small A}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -1.346}&{\small -1.806}\\
#> {\small $\text{y[B]}_{t-1}$}&{\small 2.432}&{\small 1.554}\\
#> {\small $\text{y[C]}_{t-1}$}&{\small 1.346}&{\small 2.775}\\
#> , align=ccc]
#> [\multicolumn{3}{c}{\textbf{C}}\\\hline
#> {\small A}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -0.8755}&{\small -1.15}\\
#> {\small $\text{y[B]}_{t-1}$}&{\small 1.569}&{\small 0.7985}\\
#> {\small $\text{y[C]}_{t-1}$}&{\small 1.227}&{\small 2.3}\\
#> , align=ccc]]
#> \end{forest}
```

### Using the LaTeX code

In a LaTeX document, the code produced by
[`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
can be included as is (for instance with `\input{}` applied to a file
obtained with [`writeLines()`](https://rdrr.io/r/base/writeLines.html),
as shown above), provided the `forest` package is loaded in the preamble
of the document:

``` latex
\usepackage{forest}
```

In an R Markdown or Quarto document that produces LaTeX (for instance a
PDF document), no specific step is needed: calling
`draw(model, "latex")` in a chunk is enough to render the tree. Indeed,
when the output of the document is LaTeX, the LaTeX representation is
included as is in the document, without the need for a
`results = "asis"` chunk option, and the `forest` package is added
automatically to the preamble. More precisely, the package adds the
following lines to the preamble:

``` latex
\usepackage{etoolbox}
\AtEndPreamble{\csname @ifpackageloaded\endcsname{forest}{}{\usepackage{forest}}}
```

The `\AtEndPreamble` command of the `etoolbox` package delays the
loading of `forest` to the end of the preamble, where it is loaded only
if the document has not loaded it already. A document can therefore load
`forest` itself, anywhere in its preamble and possibly with options
(such as `\usepackage[external]{forest}`, to cache the trees), without
conflict.

Each tree is included as a paragraph of its own: when a chunk draws
several trees, they are placed one below the other.

In other output formats, such as the HTML format of this article, the
LaTeX representation is shown as standard R output. Finally, notice that
text representations remain available in a document that produces LaTeX.
As the `mixvlmc.charset` option is set when the package is loaded, it is
set to `"ascii"` when the package is loaded while knitting such a
document
([`cli::is_utf8_output()`](https://cli.r-lib.org/reference/is_utf8_output.html)
reports that LaTeX output does not support UTF-8 characters).
