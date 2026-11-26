# Find the parent of a node in a context tree

This function returns the parent node of the node represented by the
`node` parameter. The result is `NULL` if `node` is the root node of its
context tree (representing the empty sequence).

## Usage

``` r
parent(node)

# S3 method for class 'ctx_node'
parent(node)

# S3 method for class 'ctx_node_cpp'
parent(node)
```

## Arguments

- node:

  a `ctx_node` object as returned by
  [`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.md)

## Value

a `ctx_node` object if `node` does not correspond to the empty sequence
or `NULL` when this is not the case

## Details

Each node of a context tree represents a sequence. When
[`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.md)
is called with success, the returned object represents the corresponding
node in the context tree. Unless the original sequence is empty, this
node has a parent node which is returned as a `ctx_node` object by the
present function. Another interpretation is that the function returns
the `ctx_node` object associated with the sequence obtained by removing
the oldest value from the original sequence.

## Examples

``` r
rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 3)
ctx_00 <- find_sequence(rdts_ctree, c(0, 0))
## the parent sequence/node corresponds to the 0 context
parent(ctx_00)
#> Sequence [T]: 0 
#>  followed by 0 (1), 1 (3)
identical(parent(ctx_00), find_sequence(rdts_ctree, c(0)))
#> [1] FALSE
## C++ backend
rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 3, backend = "C++")
ctx_00 <- find_sequence(rdts_ctree, c(0, 0))
## the parent sequence/node corresponds to the 0 context
parent(ctx_00)
#> Sequence [T]: 0 
#>  followed by 0 (1), 1 (3)
identical(
  as_sequence(parent(ctx_00)),
  as_sequence(find_sequence(rdts_ctree, c(0)))
)
#> [1] TRUE
```
