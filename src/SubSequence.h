#ifndef SUBSEQUENCE_H
#define SUBSEQUENCE_H

#include <Rcpp.h>

//' @name SubSequence
//' Context Representation
//'
//' An instance of SubSequence represents a subsequence with its associated
//' occurrences counts (before the subsequence in the original sequence)
//'
//' @noRd

class SubSequence {
 private:
  std::unordered_map<int, int>* p_counts;
  std::vector<int> ctx;

 public:
  // the constructor makes an independent copy of the contents of _counts
  SubSequence(std::vector<int> _ctx, std::unordered_map<int, int>* _counts);

  int count(int val) const;

  Rcpp::IntegerVector counts(int max) const;

  Rcpp::IntegerVector sequence() const;

  ~SubSequence();
};

#endif
