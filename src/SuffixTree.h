#ifndef SUFFIXTREE_H
#define SUFFIXTREE_H
#include <Rcpp.h>
#include "EdgeNode.h"
#include "Position.h"
#include "utils.h"

//' @name SuffixTree
//' SuffixTree representation
//'
//' An instance of SuffixTree represents a compressed suffix tree.
//'
//' We build SuffixTree for IntegerVector, under the hypothesis that the integer
//' vector contains only non negative values. Negative values are used as
//' sentinels in the algorithm.
//'
//' @noRd
class SuffixTree {
 private:
  EdgeNode* root;
  Rcpp::IntegerVector x;
  int sentinel;
  int max_x;
  bool has_total_count;
  bool has_counts;
  bool has_positions;
  bool full_explicit;
  bool has_reverse;
  int max_depth;
  int nb_ctx;
  // for restoration
  int first_value;
  int min_size;
  int max_length;
  double cut_off;

  SuffixTree(EdgeNode* _root);
  SuffixTree* clone_from_root(EdgeNode* new_root,
                              int _max_depth,
                              int _nb_ctx,
                              int _first_value) const;
  void invalidate();
  int x_at(int pos) const;
  Position find_subsequence(const Rcpp::IntegerVector& y) const;
  void add_initial_match(int first);
  std::vector<SubSequence*>* raw_subsequences(
      ExtractionConditions& when,
      const ExtractionContent& what) const;

 public:
  SuffixTree();
  ~SuffixTree();
  void insert(const Rcpp::IntegerVector& x_, int nb_vals);
  void compute_counts(int first, bool keep_position);

  void print_tree() const;
  void print_context(const Rcpp::IntegerVector& y) const;

  bool is_suffix(const Rcpp::IntegerVector& y) const;
  int count_occurrences(const Rcpp::IntegerVector& y);
  Rcpp::IntegerVector counts(const Rcpp::IntegerVector& y) const;
  Rcpp::IntegerVector positions(const Rcpp::IntegerVector& y) const;
  Rcpp::List subsequences(int min_counts, int max_length) const;
  Rcpp::List contexts(int min_counts, int max_length) const;
  Rcpp::List full_contexts(int min_counts,
                           int max_length,
                           bool with_positions,
                           bool with_cutoff,
                           bool with_local_counts) const;
  void prune(int min_counts, int max_length);
  void prune_context(int min_counts, int max_length, double K);
  SuffixTree* clone_prune(int min_counts, int max_length) const;
  SuffixTree* clone_prune_context(int min_counts,
                                  int max_length,
                                  double K) const;
  int depth() const;
  int nb_contexts() const;
  Rcpp::NumericVector cutoff() const;
  Rcpp::List representation();
  Rcpp::List restoration_info();
  void make_explicit();
  void compute_reverse();
  Rcpp::IntegerVector extend_left(const Rcpp::IntegerVector& y, int v) const;
  double loglikelihood(const Rcpp::IntegerVector& y,
                       int ignore,
                       bool extended,
                       bool verbose) const;
  int count_full_nodes() const;
  double default_loglikelihood() const;
  Rcpp::IntegerVector simulate(Rcpp::IntegerVector start,
                               int n,
                               int burnin,
                               int method) const;
  bool get_has_positions() const;
  SuffixTree* trim() const;
  Rcpp::IntegerVector predict_raw(const Rcpp::IntegerVector& y,
                                  bool final_pred) const;
  Rcpp::NumericMatrix predict_probs(const Rcpp::IntegerVector& y,
                                    bool final_pred) const;
  // ctx_node_cpp support
  Rcpp::XPtr<EdgeNode> raw_find_sequence(const Rcpp::IntegerVector& y) const;
  Rcpp::List raw_contexts() const;
  bool node_is_context(const Rcpp::XPtr<EdgeNode>& node) const;
  Rcpp::IntegerVector node_counts(const Rcpp::XPtr<EdgeNode>& node) const;
  Rcpp::IntegerVector node_local_counts(const Rcpp::XPtr<EdgeNode>& node) const;
  Rcpp::IntegerVector node_positions(const Rcpp::XPtr<EdgeNode>& node) const;
  Rcpp::XPtr<EdgeNode> node_parent(const Rcpp::XPtr<EdgeNode>& node,
                                   int length) const;
  Rcpp::List node_children(const Rcpp::XPtr<EdgeNode>& node, int length) const;
};

SuffixTree* build_suffix_tree(const Rcpp::IntegerVector& x, int nb_vals);
#endif
