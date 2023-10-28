#include <Rcpp.h>
#include "EdgeNode.h"
#include "SubSequence.h"
#include "utils.h"
using namespace Rcpp;

void insert_any_counts(std::vector<IntegerVector> table,
                       int row,
                       std::unordered_map<int, int>* counts) {
  int total = 0;
  for(auto count : (*counts)) {
    (table[count.first + 1])[row] = count.second;
    total += count.second;
  }
  (table[0])[row] = total;
}

SubSequence::SubSequence(std::vector<int> _ctx,
                         const EdgeNode* node,
                         const ExtractionContent& ext)
    : ctx(_ctx), _cut_off(0) {
  p_counts = new std::unordered_map<int, int>{*(node->counts)};
  if(ext.with_local_counts) {
    l_counts = new std::unordered_map<int, int>{*(node->counts)};
    // remove child's counts
    for(auto child : node->children) {
      if(child.first >= 0) {
        for(auto count : *(child.second->counts)) {
          (*l_counts)[count.first] -= count.second;
        }
      }
    }
  } else {
    l_counts = nullptr;
  }
  if(ext.with_position) {
    p_positions = new std::vector<int>{*(node->positions)};
  } else {
    p_positions = nullptr;
  }
  if(ext.with_cutoff && node->parent != nullptr) {
    _cut_off = kl_criterion(node->counts, node->total_count,
                            node->parent->counts, node->parent->total_count);
  }
}

int SubSequence::count(int val) const {
  auto count = p_counts->find(val);
  if(count != p_counts->end()) {
    return count->second;
  } else {
    return 0;
  }
}

IntegerVector SubSequence::counts(int max) const {
  return map_to_counts(p_counts, max);
}

void SubSequence::insert_counts(std::vector<Rcpp::IntegerVector> table,
                                int row) const {
  insert_any_counts(table, row, p_counts);
}

IntegerVector SubSequence::local_counts(int max) const {
  return map_to_counts(l_counts, max);
}

void SubSequence::insert_local_counts(std::vector<Rcpp::IntegerVector> table,
                                      int row) const {
  insert_any_counts(table, row, l_counts);
}

IntegerVector SubSequence::sequence() const {
  return IntegerVector(ctx.begin(), ctx.end());
}

IntegerVector SubSequence::positions() const {
  if(p_positions == nullptr) {
    return IntegerVector();
  } else {
    std::sort(p_positions->rbegin(), p_positions->rend());
    return IntegerVector(p_positions->begin(), p_positions->end());
  }
}

double SubSequence::cut_off() const {
  return _cut_off;
}

SubSequence::~SubSequence() {
  delete p_counts;
  if(p_positions != nullptr) {
    delete p_positions;
  }
}
