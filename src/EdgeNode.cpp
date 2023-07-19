#include "EdgeNode.h"
using namespace Rcpp;

EdgeNode::EdgeNode(EdgeNode* _parent, int s, int e)
    : parent(_parent),
      start(s),
      end(e),
      suffix(nullptr),
      total_count(0)
 {}

EdgeNode::~EdgeNode() {
  for(auto child : children) {
    delete child.second;
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
  for(auto child : children) {
    Rcout << pre << " [" << child.first << "] -> "
          << child.second->edge_label(x, cend);
    Rcout << " (" << (child.second->start) << " - " << (child.second->end)
          << ") ";
    Rcout << child.second << "\n";
    child.second->print_tree(pre + " ", x, cend);
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

