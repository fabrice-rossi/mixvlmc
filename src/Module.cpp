#include "ContextNode.h"
#include "SuffixTree.h"
RCPP_MODULE(suffixtree) {
  using namespace Rcpp;

  class_<SuffixTree>("SuffixTree")

      .method("print_tree", &SuffixTree::print_tree,
              "Print the structure of the tree")
      .method("is_suffix", &SuffixTree::is_suffix,
              "Test if a NumericVector is a suffix")
      .method("count_occurrences", &SuffixTree::count_occurrences,
              "Count the occurrences of a NumericVector in the original one")
      .method("compute_counts", &SuffixTree::compute_counts,
              "Compute the counts possibly keeping the match positions")
      .method("counts", &SuffixTree::counts,
              "Return the counts associated to a subsequence")
      .method("positions", &SuffixTree::positions,
              "Return the positions of a subsequence")
      .method("subsequences", &SuffixTree::subsequences,
              "Return subsequences that fulfill specified conditions")
      .method("contexts", &SuffixTree::contexts,
              "Return contexts that fulfill specified conditions")
      .method("full_contexts", &SuffixTree::full_contexts,
              "Return detailed contexts that fulfill specified conditions")
      .method("prune", &SuffixTree::prune,
              "Prune the suffix tree based on the specified conditions")
      .method("prune_context", &SuffixTree::prune_context,
              "Prune the suffix tree based on the specified conditions")
      .method("clone_prune", &SuffixTree::clone_prune,
              "Prune the suffix tree based on the specified conditions and "
              "return a clone")
      .method("clone_prune_context", &SuffixTree::clone_prune_context,
              "Prune the suffix tree based on the specified conditions and "
              "return a clone")
      .method("cutoff", &SuffixTree::cutoff,
              "Return significant cut off values")
      .method("representation", &SuffixTree::representation,
              "Return a representation in R of the tree")
      .method("make_explicit", &SuffixTree::make_explicit,
              "Make all nodes explicit")
      .method("compute_reverse", &SuffixTree::compute_reverse,
              "Compute reverse links")
      .method("extend_left", &SuffixTree::extend_left,
              "Extend a subsequence to the left")
      .method("print_context", &SuffixTree::print_context, "Print a context")
      .method("depth", &SuffixTree::depth, "Return the depth of the tree")
      .method("nb_contexts", &SuffixTree::nb_contexts,
              "Return the number of contexts of the tree")
      .method("loglikelihood", &SuffixTree::loglikelihood,
              "Return the loglikelihood of a new sequence when the tree is "
              "interpreted as a VLMC")
      .method("count_full_nodes", &SuffixTree::count_full_nodes,
              "Return the number of full nodes in the suffix tree")
      .method("logLik", &SuffixTree::default_loglikelihood,
              "Return the extended loglikelihood of the tree interpreted as a "
              "VLMC")
      .method("simulate", &SuffixTree::simulate,
              "Simulate a sequence using this VLMC as the generative model")
      .property("has_positions", &SuffixTree::get_has_positions,
                "Does this VLMC store positions?")
      .method("trim", &SuffixTree::trim,
              "Reduce the memory usage of the tree by removing positions")
      .method("predict_raw", &SuffixTree::predict_raw, "Predict values")
      .method("predict_probs", &SuffixTree::predict_probs,
              "Predict probabilities");
  function("build_suffix_tree", &build_suffix_tree);
}
