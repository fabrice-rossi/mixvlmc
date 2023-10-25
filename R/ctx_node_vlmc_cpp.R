#' @export
probs.ctx_node_cpp <- function(node) {
  the_counts <- counts(node)
  as.numeric(the_counts[-1] / the_counts$total)
}
