#include <Rcpp.h>
#include "SubSequence.h"
#include "utils.h"
using namespace Rcpp;

SubSequence::SubSequence(std::vector<int> _ctx,
                         std::unordered_map<int, int>* _counts) :
  SubSequence(_ctx, _counts, nullptr) {}

SubSequence::SubSequence(std::vector<int> _ctx,
                         std::unordered_map<int, int>* _counts,
                         std::vector<int>* _positions)
    : ctx(_ctx) {
  p_counts = new std::unordered_map<int, int>{*_counts};
  if(_positions == nullptr) {
    p_positions = nullptr;
  } else {
    p_positions = new std::vector<int>{*_positions};
  }
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

IntegerVector SubSequence::positions() const {
  if(p_positions == nullptr) {
    return IntegerVector();
  } else {
    return IntegerVector(p_positions->begin(), p_positions->end());
  }
}

SubSequence::~SubSequence() {
  delete p_counts;
}
