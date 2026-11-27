# Logistic model of a COVLMC context

This function returns a representation of the logistic model associated
with a COVLMC context from its node in the associated context tree.

## Usage

``` r
model(node, type = c("coef", "full"))
```

## Arguments

- node:

  A `ctx_node_covlmc` object as returned by
  [`find_sequence.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.covlmc.md)
  or
  [`contexts.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/contexts.covlmc.md)

- type:

  specifies the model information to return, either the coefficients
  only (`type="coef"`, default case) or the full model object
  (`type="full"`)

## Value

if `node` is a context, the coefficients of the logistic model (as a
vector or a matrix depending on the size of the state space) or a
logistic model as an R object. If `node` is not a context, `NULL`.

## Details

Full model extraction is only possible if the COVLMC model was not fully
trimmed (see
[`trim.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/trim.covlmc.md)).
Notice that
[`find_sequence.covlmc()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.covlmc.md)
can produce nodes that are not contexts: in this case this function
returns `NULL`.

## Examples

``` r
pc <- powerconsumption[powerconsumption$week == 5, ]
rdts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.5, 1))))
rdts_cov <- data.frame(day_night = (pc$hour >= 7 & pc$hour <= 17))
m_cov <- covlmc(rdts, rdts_cov, min_size = 10)
vals <- states(m_cov)
node <- find_sequence(m_cov, c(vals[1], vals[1]))
node
#> Context [T]: (0,1.34], (0,1.34] 
#>  followed by (0,1.34] (433), (1.34,7.54] (32)
model(node)
#>     (Intercept) day_night_1TRUE day_night_2TRUE 
#>       -2.759353        2.784578       -2.524973 
model(node, type = "full")
#> 
#> Call:  stats::glm(formula = target ~ ., family = stats::binomial(), 
#>     data = mm, control = stats::glm.control(maxit = getOption("mixvlmc.maxit")), 
#>     model = FALSE, x = FALSE, y = FALSE)
#> 
#> Coefficients:
#>     (Intercept)  day_night_1TRUE  day_night_2TRUE  
#>          -2.759            2.785           -2.525  
#> 
#> Degrees of Freedom: 464 Total (i.e. Null);  462 Residual
#> Null Deviance:       233 
#> Residual Deviance: 226.4     AIC: 232.4
```
