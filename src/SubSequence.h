#ifndef SUBSEQUENCE_H
#define SUBSEQUENCE_H

#include <Rcpp.h>
#include "ExtractionContent.h"

//' @name SubSequence
//' Context Representation
//'
//' An instance of SubSequence represents a subsequence with its associated
//' occurrences counts (before the subsequence in the original sequence),
//' its positions if those were computed and anything else based on the
//' specification provided in the ExtractionContent object
//'
//' @noRd

class EdgeNode;

class SubSequence {
 private:
  std::unordered_map<int, int>* p_counts;
  std::unordered_map<int, int>* l_counts;
  std::vector<int> ctx;
  std::vector<int>* p_positions;
  double _cut_off;

 public:
  // direct construction from the corresponding EdgeNode
  // the constructor makes an independent copy of the contents of node->counts
  // and of node->positions (if not null and asked to do it)
  SubSequence(std::vector<int> _ctx,
              const EdgeNode* node,
              const ExtractionContent& ext);

  int count(int val) const;

  Rcpp::IntegerVector counts(int max) const;

  void insert_counts(std::vector<Rcpp::IntegerVector> table, int row) const;

  Rcpp::IntegerVector local_counts(int max) const;

  void insert_local_counts(std::vector<Rcpp::IntegerVector>, int row) const;

  Rcpp::IntegerVector sequence() const;

  Rcpp::IntegerVector positions() const;

  double cut_off() const;

  ~SubSequence();
};

#endif
