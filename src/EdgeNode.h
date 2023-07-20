#ifndef EDGENODE_H
#define EDGENODE_H
#include <Rcpp.h>
#include "SubSequence.h"

//' @name EdgeNode
//' Suffix tree edge and node representation
//'
//' An instance of EdgeNode represents both the edge linking a node of the
//' suffix tree to another node and the target node itself.
//'
//' @noRd
class EdgeNode {
 public:
  // the parent node
  EdgeNode* parent;
  // start position in the NumericVector (included)
  int start;
  // end position in the NumericVector (excluded)
  int end;
  // nodes linked from this node, with indexed by the first value of the
  // corresponding edge
  std::unordered_map<int, EdgeNode*> children;
  // suffix link: if this node represents w and there is a in the alphabet
  // such that w=av and v is represented by the node P, that suffix points to P
  EdgeNode* suffix;

  // number of occurrences of the sequence represented by the path to this node
  // and the node itself. Computed post hoc.
  int total_count;

  // detailed counts of elements before the occurrences of the
  // sequence represented by this node.
  std::unordered_map<int, int>* counts;

  // Depth of the node, i.e. length of the sequence it represents
  // Computed post hoc.
  int depth;

  EdgeNode(EdgeNode* _parent, int s, int e);

  ~EdgeNode();

  void setSuffix(EdgeNode* suffix_) { suffix = suffix_; }

  // compute the current edge length: most edges are open ended and thus
  // end is only defined by the length of the string processed so far. To
  // avoid repeated updates to end, we set it to the length of the NumericVector
  // to process. So it brings information only when it is smaller than current.
  int edge_length(int current) const {
    return std::min(end, current + 1) - start;
  }

  // compute the final edge length
  int edge_length() const { return end - start; }

  // compute the current edge label
  std::string edge_label(const Rcpp::IntegerVector& x, int current) const;

  void print_tree(std::string pre,
                  const Rcpp::IntegerVector& x,
                  int cend) const;

  // computes the number of occurrences of the string represented by a node.
  // This is to be called only on a full suffix tree where
  // each leaf corresponds to a suffix ended by a sentinel.
  void compute_total_count();

  // compute the counts of the values
  // that are before each instance of subsequence represented
  // by this node using an additional first term for the
  // longest suffix. Compute depth and total counts as a by product.
  void compute_counts(int first, const Rcpp::IntegerVector& x, int cdepth);

  // insert in subs (recursively) all subsequences of length at most
  // max_length that appear at least min_counts time in the original
  // sequence (x). It only_ctx is true, the subsequences must be contexts
  // in addition. Returns true if it is a subsequence.
  bool subsequences(int min_counts,
                    int max_length,
                    bool only_ctx,
                    const Rcpp::IntegerVector& x,
                    int nb_vals,
                    std::vector<int>& pre,
                    std::vector<SubSequence*>& subs) const;
};

#endif
