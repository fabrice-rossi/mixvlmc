# Find the node of a sequence in a context tree

This function checks whether the sequence `ctx` is represented in the
context tree `ct`. If this is the case, it returns a description of the
matching node, an object of class `ctx_node`. If the sequence is not
represented in the tree, the function returns `NULL`.

## Usage

``` r
find_sequence(ct, ctx, reverse = FALSE, ...)

# S3 method for class 'ctx_tree'
find_sequence(ct, ctx, reverse = FALSE, ...)

# S3 method for class 'ctx_tree_cpp'
find_sequence(ct, ctx, reverse = FALSE, ...)
```

## Arguments

- ct:

  a context tree.

- ctx:

  a sequence to search in the context tree

- reverse:

  specifies whether the sequence `ctx` is given in the temporal order
  (`FALSE`, default value) or in the reverse temporal order (`TRUE`).
  See the dedicated section.

- ...:

  additional parameters for the find_sequence function

## Value

an object of class `ctx_node` if the sequence `ctx` is represented in
the context tree, `NULL` when this is not the case.

## Details

The function looks for sequences in general. The
[`is_context()`](https://fabrice-rossi.github.io/mixvlmc/dev/reference/is_context.md)
function can be used on the resulting object to test if the sequence is
in addition a proper context.

## State order in a sequence

sequences are given by default in the temporal order and not in the
"reverse" order used by many VLMC research papers: older values are on
the left. For instance, the context `c(1, 0)` is reported if the
sequence 0, then 1 appeared in the time series used to build the context
tree. In the present function, `reverse` refers both to the order used
for the `ctx` parameter and to the default order used by the resulting
`ctx_node` object.

## Examples

``` r
rdts <- c("A", "B", "C", "A", "A", "B", "B", "C", "C", "A")
rdts_tree <- ctx_tree(rdts, max_depth = 3)
find_sequence(rdts_tree, "A")
#> Context [T]: A 
#>  followed by A (1), B (2), C (0)
## returns NULL as "A" "C" does not appear in rdts
find_sequence(rdts_tree, c("A", "C"))
#> NULL
## "A", "B" in temporal order is "B", "A" in reverse order
find_sequence(rdts_tree, c("B", "A"), reverse = TRUE)
#> Context [R]: B, A 
#>  followed by A (0), B (1), C (1)
```
