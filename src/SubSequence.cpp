#include <Rcpp.h>
#include "SubSequence.h"
#include "utils.h"
using namespace Rcpp;

SubSequence::SubSequence(std::vector<int> _ctx,
                         std::unordered_map<int, int>* _counts)
    : ctx(_ctx) {
  p_counts = new std::unordered_map<int, int>{*_counts};
}

int SubSequence::count(int val) const {
  if(auto count = p_counts->find(val); count != p_counts->end()) {
    return count->second;
  } else {
    return 0;
  }
}

IntegerVector SubSequence::counts(int max) const {
  return map_to_counts(p_counts, max);
}

IntegerVector SubSequence::sequence() const {
  return IntegerVector(ctx.begin(), ctx.end());
}

SubSequence::~SubSequence() {
  delete p_counts;
}
