#include <algorithm>
#include "EdgeNode.h"
#include "utils.h"

using namespace Rcpp;

std::string to_string(const std::vector<int>& pre) {
  std::string res = "";
  for(int k : pre) {
    res += std::to_string(k);
  }
  return res;
}

EdgeNode::EdgeNode(EdgeNode* _parent, int s, int e)
    : parent(_parent),
      start(s),
      end(e),
      suffix(nullptr),
      total_count(0),
      counts(nullptr),
      positions(nullptr),
      depth(0) {}

EdgeNode::~EdgeNode() {
  for(auto child : children) {
    delete child.second;
  }
  if(counts != nullptr) {
    delete counts;
  }
  if(positions != nullptr) {
    delete positions;
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
  Rcout << pre << this << " ~ " << depth;
  Rcout << "\n";
  if(suffix != nullptr) {
    Rcout << pre << "sf " << suffix << "\n";
  }
  if(counts != nullptr) {
    Rcout << pre << counts_to_string(counts) << "\n";
  }
  if(positions != nullptr) {
    Rcout << pre << "{";
    int np = (int)positions->size() - 1;
    for(int i = 0; i < np; i++) {
      Rcout << (*positions)[i] << ", ";
    }
    Rcout << (*positions)[np] << "}\n";
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
                              bool keep_position,
                              int cdepth,
                              int& mdepth) {
  depth = cdepth + edge_length();
  if(depth > mdepth) {
    mdepth = depth;
  }
  counts = new std::unordered_map<int, int>{};
  if(keep_position) {
    positions = new std::vector<int>{};
  }
  if(children.size() == 0) {
    // this is a leaf, therefore a suffix which has a single
    // preceding character at position nx-depth
    int pos = x.size() - depth;
    if(keep_position) {
      positions->push_back(pos + 1);
    }
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
      child.second->compute_counts(first, x, keep_position, depth, mdepth);
      total_count += child.second->total_count;
      if(keep_position) {
        positions->insert(positions->end(), child.second->positions->begin(),
                          child.second->positions->end());
      }
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

bool EdgeNode::subsequences(int min_counts,
                            int max_length,
                            bool only_ctx,
                            const Rcpp::IntegerVector& x,
                            int nb_vals,
                            std::vector<int>& pre,
                            std::vector<SubSequence*>& subs) const {
  bool is_sub = false;
  if(total_count >= min_counts) {
    // let us first handle the root
    if(start < 0) {
      int nb_sub = 0;
      for(auto child : children) {
        if(child.first >= 0 &&
           child.second->subsequences(min_counts, max_length, only_ctx, x,
                                      nb_vals, pre, subs)) {
          nb_sub++;
        }
      }
      if(only_ctx && nb_sub < nb_vals) {
        // we consider the empty context
        subs.push_back(new SubSequence(pre, counts));
        return true;
      } else {
        return false;
      }
    } else {
      // non root
      size_t before = pre.size();
      // if the edge length is larger than one, then we are sure to keep the
      // subsequence as a context (single child)
      if(edge_length() > 1) {
        is_sub = true;
      }
      // first progress on the edge if needed (we do not include the sentinel)
      int the_end = std::min((int)x.size(), end) - 1;
      bool ml_reached = false;
      for(int i = start; i < the_end; i++) {
        pre.push_back(x[i]);
        if(pre.size() <= (size_t)max_length) {
          // always contexts
          subs.push_back(new SubSequence(pre, counts));
        } else {
          ml_reached = true;
          break;
        }
      }
      if(!ml_reached) {
        // add the final element of the edge
        pre.push_back(x[the_end]);
        // conditional inclusion only, see below
        if(pre.size() < (size_t)max_length) {
          // we may have longer subsequences
          // we will keep this subsequence
          is_sub = true;
          int nb_sub = 0;
          for(auto child : children) {
            // do not recurse in sentinel nodes
            if(child.first >= 0 &&
               child.second->subsequences(min_counts, max_length, only_ctx, x,
                                          nb_vals, pre, subs)) {
              nb_sub++;
            }
          }
          // the current subsequence is a context if all subsequences should be
          // added or if some of the potential longer subsequences have not
          // been included
          if((!only_ctx) || nb_sub < nb_vals) {
            // this is context/subsequence
            subs.push_back(new SubSequence(pre, counts));
          }
        } else if(pre.size() == (size_t)max_length) {
          // no child tested
          subs.push_back(new SubSequence(pre, counts));
          is_sub = true;
        }
      }
      pre.resize(before);
    }
  }
  // if total_count is too small, this is also the case for the children
  // so we stop recursion here
  return is_sub;
}

bool EdgeNode::prune(int min_counts,
                     int max_length,
                     int nb_vals,
                     int nx,
                     int& mdepth,
                     int& nb_ctx) {
  if(total_count >= min_counts) {
    if(depth > max_length) {
      // depth based pruning
      // we have to insert explicit nodes if a part of the edge is kept
      if(depth - edge_length() + 1 > max_length) {
        // nothing to keep
        return true;
      } else {
        // part of the edge should be kept
        // we do that by reducing the end counter
        int allowance = max_length - depth + edge_length();
        depth = depth - edge_length() + allowance;
        if(depth > mdepth) {
          mdepth = depth;
        }
        end = start + allowance;
        for(auto child : children) {
          delete child.second;
        }
        children.clear();
        nb_ctx += allowance;
        return false;
      }
    } else {
      if(edge_length() > 1) {
        // count intermediate contexts
        if(end > nx) {
          nb_ctx += edge_length() - 2;
        } else {
          nb_ctx += edge_length() - 1;
        }
      }
      // recursive processing
      if(depth > mdepth) {
        mdepth = depth;
      }
      int nb_sub = 0;
      for(auto child = children.begin(); child != children.end();) {
        if(child->first < 0) {
          // this a sentinel node, we can remove it safely
          delete child->second;
          child = children.erase(child);
        } else {
          bool result = child->second->prune(min_counts, max_length, nb_vals,
                                             nx, mdepth, nb_ctx);
          if(result) {
            delete child->second;
            child = children.erase(child);
          } else {
            ++child;
            nb_sub++;
          }
        }
      }
      if(nb_sub < nb_vals) {
        nb_ctx++;
      }
      return false;
    }
  } else {
    // count based pruning
    return true;
  }
}

int EdgeNode::flatten(const Rcpp::IntegerVector& x,
                      int nb_vals,
                      std::vector<Rcpp::IntegerVector>& tree_structure,
                      std::vector<Rcpp::IntegerVector>& tree_counts) const {
  IntegerVector f_by = map_to_counts(counts, nb_vals - 1);
  int pos = tree_structure.size();
  int the_end = end;
  if(the_end > x.size()) {
    // sentinel, no children
    the_end = x.size();
  }
  int sub_pos = pos;
  for(int i = start; i < the_end - 1; i++) {
    IntegerVector t_children(nb_vals, R_NaInt);
    tree_counts.push_back(f_by);
    t_children[x[i + 1]] = sub_pos + 2;    // +1 for R
    tree_structure.push_back(t_children);  // in position sub_pos
    sub_pos++;
  }
  if(children.size() > 0) {
    IntegerVector t_children(nb_vals, R_NaInt);
    tree_structure.push_back(t_children);
    tree_counts.push_back(f_by);
    int child_pos = tree_structure.size() - 1;
    for(auto child : children) {
      if(child.first >= 0) {
        t_children[child.first] =
            child.second->flatten(x, nb_vals, tree_structure, tree_counts);
      }
    }
    tree_structure[child_pos] = t_children;
  } else {
    IntegerVector empty{};
    tree_structure.push_back(empty);
    tree_counts.push_back(f_by);
  }
  return pos + 1;  // +1 for R
}
