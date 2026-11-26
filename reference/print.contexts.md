# Print a context list

This function prints a list of contexts i.e. a `contexts` object listing
`ctx_node` objects.

## Usage

``` r
# S3 method for class 'contexts'
print(x, reverse = NULL, ...)
```

## Arguments

- x:

  the `contexts` object to print

- reverse:

  specifies whether the contexts should be reported in temporal order
  (`FALSE`) or in reverse temporal order (`TRUE`). If the parameter is
  `NULL` (default), the contexts are displayed in the order specified by
  the call to
  [`contexts()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.md)
  used to build the context list.

- ...:

  additional arguments for the print function.

## Value

the `x` object, invisibly

## See also

[`contexts()`](https://fabrice-rossi.github.io/mixvlmc/reference/contexts.md)

## Examples

``` r
rdts <- c("A", "B", "C", "A", "A", "B", "B", "C", "C", "A")
rdts_tree <- ctx_tree(rdts, max_depth = 3)
print(contexts(rdts_tree))
#> Contexts:
#>  A
#>  A, B
#>  B
#>  B, C
#>  C
print(contexts(rdts_tree), reverse = TRUE)
#> Contexts:
#>  A
#>  B, A
#>  B
#>  C, B
#>  C
```
