# Trim a VLMC

This function returns a trimmed VLMC from which the positions of the
contexts in the time series (see the `keep_match` parameter of
[`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/vlmc.md))
have been removed.

## Usage

``` r
# S3 method for class 'vlmc'
trim(ct, ...)
```

## Arguments

- ct:

  a VLMC.

- ...:

  additional arguments (not used).

## Value

a trimmed VLMC.

## Examples

``` r
## VLMC trimming is generally useless unless match positions were kept
pc <- powerconsumption[powerconsumption$week %in% 5:6, ]
rdts <- cut(pc$active_power, breaks = 4)
model <- vlmc(rdts, keep_match = TRUE)
print(object.size(model))
#> 12728 bytes
model <- trim(model)
## memory use should be reduced
print(object.size(model))
#> 12728 bytes
nm_model <- vlmc(rdts)
print(object.size(nm_model))
#> 12728 bytes
nm_model <- trim(nm_model)
## no effect when match positions are not kept
print(object.size(nm_model))
#> 12728 bytes
```
