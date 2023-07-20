#include <algorithm>
#include "EdgeNode.h"
#include "utils.h"

using namespace Rcpp;

EdgeNode::EdgeNode(EdgeNode* _parent, int s, int e)
    : parent(_parent),
      start(s),
      end(e),
      suffix(nullptr),
      total_count(0),
      counts(nullptr),
      depth(0) {}

EdgeNode::~EdgeNode() {
  for(auto child : children) {
    delete child.second;
  }
  if(counts != nullptr) {
    delete counts;
  }
}

std::string EdgeNode::edge_label(const IntegerVector& x, int current) const {
  std::string res = "";
  int size = std::min(end, current + 1);
  for(int i = start; i < size; i++) {
    if(i < x.size()) {
      res += std::to_string(x[i]);
    } else {
      res += "$";
    }
  }
  return res;
}

void EdgeNode::print_tree(std::string pre,
                          const IntegerVector& x,
                          int cend) const {
  Rcout << pre << this;
  Rcout << "\n";
  if(suffix != nullptr) {
    Rcout << pre << "sf " << suffix << "\n";
  }
  if(counts != nullptr) {
    Rcout << pre << counts_to_string(counts) << "\n";
  }
  for(auto child : children) {
    Rcout << pre << " [" << child.first << "] -> "
          << child.second->edge_label(x, cend);
    Rcout << " (" << (child.second->start) << " - " << (child.second->end)
          << ")\n";
    child.second->print_tree(pre + "  ", x, cend);
  }
}

void EdgeNode::compute_total_count() {
  if(children.size() == 0) {
    total_count = 1;
  } else {
    total_count = 0;
    for(auto child : children) {
      child.second->compute_total_count();
      total_count += child.second->total_count;
    }
  }
}

void EdgeNode::compute_counts(int first,
                              const Rcpp::IntegerVector& x,
                              int cdepth) {
  depth = cdepth + edge_length();
  counts = new std::unordered_map<int, int>{};
  if(children.size() == 0) {
    // this is a leaf, therefore a suffix which has a single
    // preceding character at position nx-depth
    int pos = x.size() - depth;
    int val;
    if(pos >= 0) {
      val = x[pos];
    } else {
      val = first;
    }
    (*counts)[val] = 1;
    total_count = 1;
  } else {
    total_count = 0;
    for(auto child : children) {
      child.second->compute_counts(first, x, depth);
      total_count += child.second->total_count;
      // update counts
      for(auto count : *(child.second->counts)) {
        if(auto current = counts->find(count.first); current != counts->end()) {
          current->second += count.second;
        } else {
          (*counts)[count.first] = count.second;
        }
      }
    }
  }
}

void EdgeNode::subsequences(int min_counts,
                            int max_length,
                            const Rcpp::IntegerVector& x,
                            std::vector<int>& pre,
                            std::vector<SubSequence*>& subs) const {
  if(total_count >= min_counts) {
    size_t before = pre.size();
    if(start >= 0) {
      int the_end = std::min((int)x.size(), end);
      for(int i = start; i < the_end; i++) {
        pre.push_back(x[i]);
        if(pre.size() <= (size_t)max_length) {
          subs.push_back(new SubSequence(pre, counts));
        } else {
          break;
        }
      }
    }
    if(pre.size() < (size_t)max_length) {
      for(auto child : children) {
        child.second->subsequences(min_counts, max_length, x, pre, subs);
      }
    }
    pre.resize(before);
  }
  // if total_count is too small, this is also the case for the children
  // so we stop recursion here
}
