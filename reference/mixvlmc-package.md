# mixvlmc: Variable Length Markov Chains with Covariates

Estimates Variable Length Markov Chains (VLMC) and VLMC with covariates
from discrete sequences. Supports model selection via information
criteria, likelihood computation, prediction and simulation of new
sequences from an estimated model. See Bühlmann, P. and Wyner, A. J.
(1999)
[doi:10.1214/aos/1018031204](https://doi.org/10.1214/aos/1018031204) for
VLMC and Zanin Zambom, A., Kim, S. and Lopes Garcia, N. (2022)
[doi:10.1111/jtsa.12615](https://doi.org/10.1111/jtsa.12615) for VLMC
with covariates.

## Package options

mixvlmc uses the following
[`options()`](https://rdrr.io/r/base/options.html), which are set when
the package is loaded (unless they are already set):

- `mixvlmc.maxit`: maximum number of iterations in model fitting for
  [`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
  and related functions such as
  [`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)
  (default value: 100). It is used by all the computing engines (see
  `mixvlmc.predictive`).

- `mixvlmc.predictive`: specifies the computing engine used for model
  fitting for
  [`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
  and related functions. Two values are supported:

  - `"glm"` (default value):
    [`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
    uses [`stats::glm()`](https://rdrr.io/r/stats/glm.html) with a
    binomial link
    ([`stats::binomial()`](https://rdrr.io/r/stats/family.html)) for a
    two-value state space, and
    [`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) with a
    multinomial link
    ([`VGAM::multinomial()`](https://rdrr.io/pkg/VGAM/man/multinomial.html))
    for a state space with three or more values;

  - `"multinom"`:
    [`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
    uses
    [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html) in
    all cases.

  The first option `"glm"` is recommended as both
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html) and
  [`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) are able to
  detect and deal with degeneracy in the data set. The results of the
  two options can differ, see the "Logistic models" section of
  [`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md).

- `mixvlmc.backend`: specifies the implementation used for the context
  tree construction in
  [`ctx_tree()`](https://fabrice-rossi.github.io/mixvlmc/reference/ctx_tree.md),
  [`vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/vlmc.md)
  and
  [`tune_vlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_vlmc.md).
  Two values are supported:

  - `"C++"` (default value): this corresponds to the C++ implementation,
    which is significantly faster than the R implementation.

  - `"R"`: this corresponds to the original almost pure R
    implementation, whose context trees are nested lists that can be
    processed in R.

  Both implementations give the same models.
  [`covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/covlmc.md)
  and
  [`tune_covlmc()`](https://fabrice-rossi.github.io/mixvlmc/reference/tune_covlmc.md)
  always use the R implementation.

- `mixvlmc.charset`: specifies the collection of characters used to
  display context trees in "ascii art" when using the `"text"` format
  for
  [`draw()`](https://fabrice-rossi.github.io/mixvlmc/reference/draw.md)
  and related functions. Two values are supported:

  - `"ascii"`: the collection uses only standard ASCII characters and
    should be compatible with all environments;

  - `"utf8"`: the collection uses UTF-8 symbols and needs a compatible
    display.

  The default value is `"utf8"` if the output supports UTF-8 (according
  to
  [`cli::is_utf8_output()`](https://cli.r-lib.org/reference/is_utf8_output.html)
  when the package is loaded) and `"ascii"` otherwise.

## See also

Useful links:

- <https://github.com/fabrice-rossi/mixvlmc>

- <https://fabrice-rossi.github.io/mixvlmc/>

- Report bugs at <https://github.com/fabrice-rossi/mixvlmc/issues>

## Author

**Maintainer**: Fabrice Rossi <Fabrice.Rossi@apiacoa.org>
([ORCID](https://orcid.org/0000-0003-4638-1286)) \[copyright holder\]

Other contributors:

- Hugo Le Picard <lepicardhugo@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-7023-2996)) \[contributor\]

- Guénolé Joubioux <guenole.joubioux@gmail.com> \[contributor\]
