# Control for COVLMC fitting

This function creates a list with parameters used to fine-tune the
COVLMC fitting algorithm.

## Usage

``` r
covlmc_control(pseudo_obs = 1)
```

## Arguments

- pseudo_obs:

  number of fake observations of each state to add to the observed ones
  (a positive number, default: 1).

## Value

a list with a `pseudo_obs` component, to be used as the `control`
parameter of
[`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/covlmc.md)
or
[`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/tune_covlmc.md).

## Details

`pseudo_obs` is used to regularise the probability estimations when a
context is always followed by the same state in the observed time
series. Transition probabilities are then computed after adding
`pseudo_obs` pseudo-observations of each of the states (including the
observed one). This corresponds to a Bayesian posterior mean estimation
with a Dirichlet prior.

## Examples

``` r
rdts <- rep(c(0, 1), 100)
rdts_cov <- data.frame(y = rep(0, length(rdts)))
default_model <- covlmc(rdts, rdts_cov)
contexts(default_model, model = "coef")$coef
#> [[1]]
#> [1] 4.615121
#> 
#> [[2]]
#> [1] -4.60517
#> 
control <- covlmc_control(pseudo_obs = 10)
model <- covlmc(rdts, rdts_cov, control = control)
contexts(model, model = "coef")$coef
#> [[1]]
#> [1] 2.397895
#> 
#> [[2]]
#> [1] -2.388763
#> 
```
