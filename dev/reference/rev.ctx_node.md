# Reverse Sequence

This function reverses the order in which the sequence represented by
the `ctx_node` parameter will be reported in other functions, mainly
[`as_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/as_sequence.md).

## Usage

``` r
# S3 method for class 'ctx_node'
rev(x)
```

## Arguments

- x:

  a `ctx_node` object as returned by
  [`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/find_sequence.md)

## Value

a `ctx_node` using the opposite ordering convention to the one of the
parameter of the function

## See also

[`is_reversed()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/is_reversed.md)

## Examples

``` r
rdts <- c("A", "B", "C", "A", "A", "B", "B", "C", "C", "A")
rdts_tree <- ctx_tree(rdts, max_depth = 3)
res <- find_sequence(rdts_tree, c("A", "B"))
print(res)
#> Context [T]: A, B 
#>  followed by A (0), B (1), C (1)
r_res <- rev(res)
print(r_res)
#> Context [R]: B, A 
#>  followed by A (0), B (1), C (1)
as_sequence(r_res)
#> [1] "B" "A"
```
