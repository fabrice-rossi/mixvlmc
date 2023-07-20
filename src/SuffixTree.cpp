#include <Rcpp.h>
#include <vector>
#include "EdgeNode.h"
#include "Position.h"
using namespace Rcpp;

//' @name SuffixTree
//' SuffixTree representation
//'
//' An instance of SuffixTree represents a compressed suffix tree.
//'
//' We build SuffixTree for IntegerVector, under the hypothesis that the integer
//' vector contains only non negative values. Negative values are used as
//' sentinels in the algorithm.
//'
//' @noRd
class SuffixTree {
 private:
  EdgeNode* root;
  IntegerVector x;
  int sentinel;
  int number_of_nodes;
  int max_x;
  bool has_total_count;

 public:
  SuffixTree()
      : sentinel(-1), number_of_nodes(0), max_x(-1), has_total_count(false) {
    root = new EdgeNode(nullptr, -1, -1);
  }

  ~SuffixTree() { delete root; }

  int x_at(int pos) const {
    if(pos >= x.size()) {
      return sentinel;
    } else {
      return x[pos];
    }
  }

  void insert(const IntegerVector& x_) {
    x = x_;
    number_of_nodes = 0;
    max_x = -1;
    has_total_count = false;
    // we use a "virtual" sentinel to avoid modifying x
    int nx = x.size() + 1;
    // representation of the active position in the suffix tree
    // the parent node
    EdgeNode* active = root;
    // the active edge/node, as viewed from the active node
    int a_edge = -1;  // not a symbol
    // the position on the edge itself
    int a_length = 0;
    // the position on x
    int a_pos = 0;
    // how many suffix we need to insert in the tree (explicitly)
    int new_suffix = 0;
    // we consider x value by value
    for(int i = 0; i < nx; i++) {
      int current_val = x_at(i);
      if(i < nx - 1 && current_val < 0) {
        // this should never happen, raise an exception
        // first clean up
        delete root;
        root = new EdgeNode(nullptr, -1, -1);
        stop("x cannot contain negative values");
      }
      if(current_val > max_x) {
        max_x = current_val;
      }
      // let us add a new suffix (ending with x[i])
      new_suffix++;
      // the last node inserted or confirmed
      EdgeNode* last = nullptr;
      while(new_suffix >= 1) {
        // we do not have an active edge so we set it tentatively to the current
        // value
        if(a_length == 0) {
          a_edge = current_val;
          a_pos = i;
        }
        // is there an ongoing edge?
        if(auto child = active->children.find(a_edge);
           child != active->children.end()) {
          // there is one
          EdgeNode* current = child->second;
          // make sure we are still inside the edge!
          if(current->edge_length(i) <= a_length) {
            // a_length is too large, we need to move the next node
            int el = current->edge_length(i);
            active = current;
            a_pos += el;
            // tentative edge, which may not exist
            a_edge = x_at(a_pos);
            a_length -= el;
            // back to the while loop (we do one step only)
          } else {
            // check the symbol
            if(x[current->start + a_length] == current_val) {
              // found, we just have to progress on the edge
              a_length++;
              if(active != root && last != nullptr) {
                // keep a suffix link
                last->setSuffix(active);
                last = active;
              }
              // we do not need to process the remaining suffixes, this will be
              // done during latter iteration
              break;
            } else {
              // the current edge does not contain x[i] at the current position
              // we need to insert a two new nodes and work on remaining
              // suffixes new edge node for the first part of the edge
              EdgeNode* break_node = new EdgeNode(active, current->start,
                                                  current->start + a_length);
              // new edge node for the edge starting in x[i]
              EdgeNode* new_node = new EdgeNode(break_node, i, nx);
              number_of_nodes++;
              break_node->children[current_val] = new_node;
              int end = current->start + a_length;
              if(end < nx - 1) {
                break_node->children[x_at(end)] = current;
              } else {
                break_node->children[sentinel] = current;
              }
              current->start = current->start + a_length;
              current->parent = break_node;
              active->children[a_edge] = break_node;
              if(last != nullptr) {
                // suffix node
                last->setSuffix(break_node);
              }
              last = break_node;
              // we did insert an explicit suffix
              new_suffix--;
              if(active == root && a_length > 0) {
                a_length--;
                a_pos = i - new_suffix + 1;
                a_edge = x_at(a_pos);
              } else {
                active = active->suffix;
                if(active == nullptr) {
                  active = root;
                }
              }
            }
          }
        } else {
          // there is no edge starting with x[i] in the current node, so we add
          // it
          EdgeNode* new_node = new EdgeNode(active, i, nx);
          number_of_nodes++;
          active->children[a_edge] = new_node;
          // this is an explicit insertion
          new_suffix--;
          if(active != root && last != nullptr) {
            last->setSuffix(active);
            last = active;
          }
          last = new_node;
          if(active == root && a_length > 0) {
            a_length--;
            a_pos = i - new_suffix + 1;
            a_edge = x_at(a_pos);
          } else {
            active = active->suffix;
            if(active == nullptr) {
              active = root;
            }
          }
        }
      }
    }
  }

  void print_tree() const { root->print_tree("", x, x.size() + 1); }

  // find the position of y in x expressed as a Position object.
  // Returns a valid Position if y is found and an invalid one if not.
  Position find_subsequence(const IntegerVector& y) const {
    auto current = root;
    int y_pos = 0;
    while(y_pos < y.length()) {
      // try to progress along an edge
      if(auto child = current->children.find(y[y_pos]);
         child != current->children.end()) {
        current = child->second;
        int el = current->edge_length();
        int move = std::min(el, (int)(y.length() - y_pos));
        for(int k = 1; k < move; k++) {
          if(y[y_pos + k] != x[current->start + k]) {
            // not found!
            return Position{};
          }
        }
        int new_from = y_pos + move;
        if(new_from == y.length()) {
          // found!
          return Position{current, move - 1};
        } else {
          // let's progress
          y_pos = new_from;
        }
      } else {
        // no edge -> not found
        return Position{};
      }
    }
    return Position{};  // default case
  }

  // test whether a subsequence is a suffix or not
  bool is_suffix(const IntegerVector& y) const {
    Position where = find_subsequence(y);
    if(where.is_valid()) {
      // we need to be at the end of the edge followed by a negative node
      // or the end of the edge must be a sentinel
      int el = where.node->edge_length();
      if(where.edge == el - 1) {
        // end of the edge. Let's check for a sentinel node
        if(auto child = where.node->children.find(-1);
           child != where.node->children.end()) {
          // there is an edge with a sentinel
          return true;
        } else {
          return false;
        }
      } else if(where.edge == el - 2) {
        // is there a sentinel symbol?
        return where.node->end >= x.length();
      } else {
        // nope, too many remaining symbols
        return false;
      }
    } else {
      return false;
    }
  }

  int count_occurrences(const IntegerVector& y) {
    if(!has_total_count) {
      root->compute_total_count();
      has_total_count = true;
    }
    Position where = find_subsequence(y);
    if(where.is_valid()) {
      return where.node->total_count;
    } else {
      return 0;
    }
  }

  // compute recursively the counts of the values
  // that are before each instance of subsequence represented
  // in the suffix tree using an additional first term
  void compute_counts(int first) { root->compute_counts(first, x, 0); }
};

SuffixTree* build_suffix_tree(const IntegerVector& x) {
  SuffixTree* tree = new SuffixTree();
  tree->insert(x);
  return tree;
}

RCPP_MODULE(suffixtree) {
  using namespace Rcpp;

  class_<SuffixTree>("SuffixTree")

      .method("print_tree", &SuffixTree::print_tree,
              "Print the structure of the tree")
      .method("is_suffix", &SuffixTree::is_suffix,
              "Test if a NumericVector is a suffix")
      .method("count_occurrences", &SuffixTree::count_occurrences,
              "Count the occurrences of a NumericVector in the original one");
  function("build_suffix_tree", &build_suffix_tree);
}
