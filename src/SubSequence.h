#ifndef SUBSEQUENCE_H
#define SUBSEQUENCE_H

#include <Rcpp.h>

//' @name SubSequence
//' Context Representation
//'
//' An instance of SubSequence represents a subsequence with its associated
//' occurrences counts (before the subsequence in the original sequence) and
//' its positions if those where computed.
//'
//' @noRd

class SubSequence {
 private:
  std::unordered_map<int, int>* p_counts;
  std::vector<int> ctx;
  std::vector<int>* p_positions;

 public:
   // the constructor makes an independent copy of the contents of _counts
   // and sets _positions to null
   SubSequence(std::vector<int> _ctx,
               std::unordered_map<int, int>* _counts);

  // the constructor makes an independent copy of the contents of _counts
  // and of _positions (if not null)
  SubSequence(std::vector<int> _ctx,
              std::unordered_map<int, int>* _counts,
              std::vector<int>* _positions);

  int count(int val) const;

  Rcpp::IntegerVector counts(int max) const;

  Rcpp::IntegerVector sequence() const;

  Rcpp::IntegerVector positions() const;

  ~SubSequence();
};

#endif
