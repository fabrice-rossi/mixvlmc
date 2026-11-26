# Merged contexts in a COVLMC

The function returns `NULL` when the context represented by the `node`
parameter is not merged with another context (see
[`is_merged()`](https://fabrice-rossi.github.io/mixvlmc/reference/is_merged.md)).
In the other case, it returns a list of contexts with which this one is
merged.

## Usage

``` r
merged_with(node)
```

## Arguments

- node:

  A `ctx_node_covlmc` object as returned by
  [`find_sequence.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.covlmc.md)
  or
  [`contexts.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.covlmc.md)

## Value

`NULL` or a list of contexts merged with `node` represented by
`ctx_node_covlmc` objects

## Details

If the context is merged, the function returns a list with one value for
each element in the state space (see
[`states()`](https://fabrice-rossi.github.io/mixvlmc/reference/states.md)).
The value is `NULL` if the corresponding context is not merged with the
`node` context, while it is a `ctx_node_covlmc` object in the other
case. A context merged with `node` differs from the context represented
by `node` only in its oldest value (i.e. its first value in temporal
order) which is used as its name in the list. For instance, if the
context `ABC` is merged only with `CBC` (when represented in temporal
ordering), then the resulting list is of the form
`list("A" = NULL, "B" = NULL, "C" = ctx_node_covlmc(CBC))`.

## See also

[`is_merged()`](https://fabrice-rossi.github.io/mixvlmc/reference/is_merged.md)

## Examples

``` r
pc_week_15_16 <- powerconsumption[powerconsumption$week %in% c(15, 16), ]
elec <- pc_week_15_16$active_power
elec_rdts <- cut(elec, breaks = c(0, 0.4, 2, 8), labels = c("low", "typical", "high"))
elec_cov <- data.frame(day = (pc_week_15_16$hour >= 7 & pc_week_15_16$hour <= 18))
elec_tune <- tune_covlmc(elec_rdts, elec_cov, min_size = 5)
elec_model <- prune(as_covlmc(elec_tune), alpha = 3.961e-10)
ctxs <- contexts(elec_model)
for (ctx in ctxs) {
  if (is_merged(ctx)) {
    print(ctx)
    cat("\nis merged with\n\n")
    print(merged_with(ctx))
  }
}
#> Context [T]: low, typical 
#>  followed by low (44), typical (70), high (6)
#> 
#> is merged with
#> 
#> $low
#> NULL
#> 
#> $typical
#> NULL
#> 
#> $high
#> Context [T]: high, typical 
#>  followed by low (2), typical (58), high (11)
#> 
#> Context [T]: high, typical 
#>  followed by low (2), typical (58), high (11)
#> 
#> is merged with
#> 
#> $low
#> Context [T]: low, typical 
#>  followed by low (44), typical (70), high (6)
#> 
#> $typical
#> NULL
#> 
#> $high
#> NULL
#> 
```
