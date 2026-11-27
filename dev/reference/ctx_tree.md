# Build a context tree for a discrete time series

This function builds a context tree for a time series.

## Usage

``` r
ctx_tree(
  x,
  min_size = 2L,
  max_depth = 100L,
  keep_position = TRUE,
  backend = getOption("mixvlmc.backend", "C++"),
  ...
)
```

## Arguments

- x:

  an object that can be interpreted as a discrete time series: a
  numeric, character, factor or logical vector, or a `dts` object (see
  [`dts()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/dts.md)).
  Its state space must contain at least two states.

- min_size:

  integer \>= 1 (default: 2). Minimum number of observations for a
  context to be included in the tree.

- max_depth:

  integer \>= 0 (default: 100). Maximum length of a context to be
  included in the tree.

- keep_position:

  logical (default: `TRUE`). Specifies whether the context tree should
  keep the positions of the contexts in the time series.

- backend:

  "R" or "C++" (default: as specified by the "mixvlmc.backend" option).
  Specifies the implementation used to represent the context tree and to
  build it. See the "Back ends" section.

- ...:

  additional parameters (not used).

## Value

a context tree (an object of a class that inherits from `ctx_tree`).

## Details

The tree represents all the sequences of symbols/states of length at
most `max_depth` that appear at least `min_size` times in the time
series and stores the frequencies of the states that follow each
context. Optionally, the positions of the contexts in the time series
can be stored in the tree.

## Back ends

Two back ends are available to compute context trees:

- the "R" back end represents the tree in pure R data structures (nested
  lists) that can be easily processed further in pure R (C++ helper
  functions are used to speed up the construction).

- the "C++" back end (the default one) represents the tree with C++
  classes. The tree is built with an optimised suffix tree algorithm
  which speeds up the construction by a factor of at least 10 in
  standard settings. As the tree is kept outside of the direct reach of
  R, context trees built with the C++ back end must be restored after a
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html)/[`readRDS()`](https://rdrr.io/r/base/readRDS.html)
  sequence. This is done automatically by recomputing the context tree
  completely.

## Examples

``` r
rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
## get all contexts of length 2
rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 2)
draw(rdts_ctree)
#> ▪
#> ├─ 0
#> │  ├─ 0
#> │  └─ 1
#> └─ 1
#>    ├─ 0
#>    └─ 1
## with the C++ back end
rdts_ctree_cpp <- ctx_tree(rdts,
  min_size = 1, max_depth = 2, backend = "C++"
)
draw(rdts_ctree_cpp)
#> ▪
#> ├─ 0
#> │  ├─ 0
#> │  └─ 1
#> └─ 1
#>    ├─ 0
#>    └─ 1
```
