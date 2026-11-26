# Find the children nodes of a node in a context tree

This function returns a list (possibly empty) of `ctx_node` objects.
Each object represents one of the children of the node represented by
the `node` parameter.

## Usage

``` r
# S3 method for class 'ctx_node_covlmc'
children(node)

children(node)

# S3 method for class 'ctx_node'
children(node)

# S3 method for class 'ctx_node_cpp'
children(node)
```

## Arguments

- node:

  a `ctx_node` object as returned by
  [`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.md)

## Value

a list of `ctx_node` objects, see details.

## Details

Each node of a context tree represents a sequence. When
[`find_sequence()`](https://fabrice-rossi.github.io/mixvlmc/reference/find_sequence.md)
is called with success, the returned object represents the corresponding
node in the context tree. If this node has no children, the present
function returns an empty list. When the node has at least one child,
the function returns a list with one value for each element in the state
space (see
[`states()`](https://fabrice-rossi.github.io/mixvlmc/reference/states.md)).
The value is `NULL` if the corresponding child is empty, while it is a
`ctx_node` object when the child is present. Each `ctx_node` object is
associated with the sequence obtained by adding to the past of the
sequence represented by `node` an observation of the associated state
(this corresponds to an extension to the left of the sequence in
temporal order).

## Examples

``` r
rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 3)
ctx_00 <- find_sequence(rdts_ctree, c(0, 0))
## this context can only be extended in the past by 1:
children(ctx_00)
#> $`0`
#> NULL
#> 
#> $`1`
#> Context [T]: 1, 0, 0 
#>  followed by 0 (0), 1 (1)
#> 
ctx_10 <- find_sequence(rdts_ctree, c(1, 0))
## this context can be extended by both states
children(ctx_10)
#> $`0`
#> Context [T]: 0, 1, 0 
#>  followed by 0 (0), 1 (1)
#> 
#> $`1`
#> Context [T]: 1, 1, 0 
#>  followed by 0 (1), 1 (0)
#> 
## C++ backend
rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 3, backend = "C++")
ctx_00 <- find_sequence(rdts_ctree, c(0, 0))
## this context can only be extended in the past by 1:
children(ctx_00)
#> $`0`
#> NULL
#> 
#> $`1`
#> Context [T]: 1, 0, 0 
#>  followed by 0 (0), 1 (1)
#> 
ctx_10 <- find_sequence(rdts_ctree, c(1, 0))
## this context can be extended by both states
children(ctx_10)
#> $`0`
#> Context [T]: 0, 1, 0 
#>  followed by 0 (0), 1 (1)
#> 
#> $`1`
#> Context [T]: 1, 1, 0 
#>  followed by 0 (1), 1 (0)
#> 
```
