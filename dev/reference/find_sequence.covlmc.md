# Find the node of a sequence in a COVLMC context tree

This function checks whether the sequence `ctx` is represented in the
context tree of the COVLMC model `ct`. If this is the case, it returns a
description of the matching node, an object of class `ctx_node_covlmc`.
If the sequence is not represented in the tree, the function returns
`NULL`.

## Usage

``` r
# S3 method for class 'covlmc'
find_sequence(ct, ctx, reverse = FALSE, ...)
```

## Arguments

- ct:

  a context tree.

- ctx:

  a sequence to search in the context tree

- reverse:

  specifies whether the sequence `ctx` is given in the temporal order
  (`FALSE`, default value) or in the reverse temporal order (`TRUE`).
  See the dedicated section.

- ...:

  additional parameters for the find_sequence function

## Value

an object of class `ctx_node_covlmc` if the sequence `ctx` is
represented in the context tree, `NULL` when this is not the case

## Details

The function looks for sequences in general. The
[`is_context()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/is_context.md)
function can be used on the resulting object to test if the sequence is
in addition a proper context.

## State order in a sequence

sequences are given by default in the temporal order and not in the
"reverse" order used by many VLMC research papers: older values are on
the left. For instance, the context `c(1, 0)` is reported if the
sequence 0, then 1 appeared in the time series used to build the context
tree. In the present function, `reverse` refers both to the order used
for the `ctx` parameter and to the default order used by the resulting
`ctx_node` object.

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
m_cov <- covlmc(rdts, rdts_cov, min_size = 10)

## not in the tree
vals <- states(m_cov)
find_sequence(m_cov, c(vals[2], vals[2]))
#> NULL
## in the tree but not a context
node <- find_sequence(m_cov, c(vals[1]))
node
#> Sequence [T]: (0,1.34] 
#>  followed by (0,1.34] (466), (1.34,7.54] (37)
is_context(node)
#> [1] FALSE
## in the tree and a context
node <- find_sequence(m_cov, c(vals[1], vals[1]))
node
#> Context [T]: (0,1.34], (0,1.34] 
#>  followed by (0,1.34] (433), (1.34,7.54] (32)
is_context(node)
#> [1] TRUE
model(node)
#>     (Intercept) day_night_1TRUE day_night_2TRUE 
#>       -2.759353        2.784578       -2.524973 
```
