# Test if the object is a VLMC model

This function returns `TRUE` for VLMC models and `FALSE` for other
objects.

## Usage

``` r
is_vlmc(x)
```

## Arguments

- x:

  an R object.

## Value

`TRUE` for VLMC models.

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power,
  breaks = c(0, quantile(pc$active_power,
    probs = c(0.25, 0.5, 0.75, 1)
  ))
)
model <- vlmc(rdts)
# should be true
is_ctx_tree(model)
#> [1] TRUE
# should be true
is_vlmc(model)
#> [1] TRUE
# should be false
is_covlmc(model)
#> [1] FALSE
```
