#include <Rcpp.h>
#include <vector>
#include "EdgeNode.h"
#include "Position.h"
#include "utils.h"
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
  int max_x;
  bool has_total_count;
  bool has_counts;
  bool has_positions;
  bool full_explicit;
  bool has_reverse;
  int max_depth;
  int nb_ctx;

  SuffixTree(EdgeNode* _root)
      : root(_root),
        sentinel(-1),
        max_x(-1),
        has_total_count(false),
        has_counts(false),
        has_positions(false),
        full_explicit(false),
        has_reverse(false),
        max_depth(0),
        nb_ctx(0) {}

 public:
  SuffixTree()
      : sentinel(-1),
        max_x(-1),
        has_total_count(false),
        has_counts(false),
        has_positions(false),
        full_explicit(false),
        has_reverse(false),
        max_depth(0),
        nb_ctx(0) {
    root = new EdgeNode(nullptr, -1, -1);
  }

  ~SuffixTree() { delete root; }

  SuffixTree* clone_from_root(EdgeNode* new_root,
                              int _max_depth,
                              int _nb_ctx) const {
    SuffixTree* nt = new SuffixTree(new_root);
    nt->x = x;
    nt->max_x = max_x;
    nt->has_total_count = has_total_count;
    nt->has_counts = has_counts;
    nt->has_positions = has_positions;
    nt->full_explicit = full_explicit;
    nt->has_reverse = false;
    nt->max_depth = _max_depth;
    nt->nb_ctx = _nb_ctx;
    return nt;
  }

  int x_at(int pos) const {
    if(pos >= x.size()) {
      return sentinel;
    } else {
      return x[pos];
    }
  }

  void invalidate() {
    has_total_count = false;
    has_counts = false;
    has_positions = false;
    full_explicit = false;
    has_reverse = false;
    nb_ctx = 0;
  }

  void insert(const IntegerVector& x_, int nb_vals) {
    invalidate();
    x = x_;
    max_x = -1;
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
        if(max_x >= nb_vals) {
          stop("x contains values larger than expected: value " +
               std::to_string(max_x) + ", expected maximum " +
               std::to_string(nb_vals - 1));
        }
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
    max_x = nb_vals - 1;
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

  void print_context(const IntegerVector& y) const {
    Position pos = find_subsequence(y);
    if(pos.is_valid()) {
      Rcout << pos.node << " [" << pos.edge << "]  ~ " << pos.node->depth
            << "\n";
      if(pos.node->suffix != nullptr) {
        Rcout << "sf " << pos.node->suffix << "\n";
      }
      if(pos.node->counts != nullptr) {
        Rcout << counts_to_string(pos.node->counts) << "\n";
        Rcout << pos.node->total_count << "\n";
      }
      if(pos.node->reverse != nullptr) {
        for(auto rev : *(pos.node->reverse)) {
          Rcout << rev.first << " -> " << rev.second << "\n";
        }
      }
    } else {
      Rcout << "Not found\n";
    }
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
  // compute total counts on the fly
  void compute_counts(int first, bool keep_position) {
    if(!has_counts) {
      root->compute_counts(first, x, keep_position, 0, max_depth);
      has_total_count = true;
      has_counts = true;
      has_positions = keep_position;
      if(keep_position) {
        add_initial_match(first);
      }
    }
  }

  void add_initial_match(int first) {
    if(!has_positions) {
      stop("add_initial_match cannot be called directly");
    }
    // we find the nodes that encode sequences of the form first, x[0], x[1],
    // etc. and add -1 to the matched position of those nodes
    auto current = root;
    int x_pos = -1;
    while(x_pos < x.length()) {
      // try to progress along an edge
      int val = first;
      if(x_pos >= 0) {
        val = x[x_pos];
      }
      if(auto child = current->children.find(val);
         child != current->children.end()) {
        current = child->second;
        current->positions->push_back(-1);
        int el = current->edge_length();
        int move = std::min(el, (int)(x.length() - x_pos));
        for(int k = 1; k < move; k++) {
          if(x[x_pos + k] != x[current->start + k]) {
            // we need to break the edge to avoid introducing fake matches
            EdgeNode* end_of_edge =
                new EdgeNode(current, current->start + k, current->end);
            end_of_edge->positions = new std::vector<int>(
                current->positions->begin(), current->positions->end());
            end_of_edge->positions->pop_back();
            end_of_edge->total_count = current->total_count;
            end_of_edge->counts = new std::unordered_map<int, int>(
                current->counts->begin(), current->counts->end());
            end_of_edge->children = current->children;
            current->children.clear();
            current->children[x[current->start + k]] = end_of_edge;
            current->end = current->start + k;
            break;
          }
        }
        x_pos += move;
      } else {
        break;
      }
    }
  }

  // return the counts associated to a subsequence as computed by a call
  // to compute_counts. Returns an empty IntegerVector if the subsequence
  // is not found.
  IntegerVector counts(const IntegerVector& y) const {
    if(!has_counts) {
      stop(
          "counts cannot be used if compute_counts has not been called before");
    }
    Position where = find_subsequence(y);
    if(where.is_valid()) {
      return map_to_counts(where.node->counts, max_x);
    } else {
      return IntegerVector{};
    }
  }

  // return the positions associated to a subsequence as computed by a call
  // to compute_counts with position saving.
  // Returns an empty IntegerVector if the subsequence is not found.
  IntegerVector positions(const IntegerVector& y) const {
    if(!has_positions) {
      stop("positions cannot be used if positions have not been saved");
    }
    Position where = find_subsequence(y);
    if(where.is_valid()) {
      if(where.node->positions == nullptr) {
        stop(
            "Internal error in positions: I should have positions but I do "
            "not!");
      }
      return IntegerVector(where.node->positions->begin(),
                           where.node->positions->end());
    } else {
      return IntegerVector{};
    }
  }

  std::vector<SubSequence*>* raw_subsequences(ExtractionConditions& when,
                                              const ExtractionContent& what) {
    if(!has_counts) {
      stop(
          "subsequences and contexts cannot be used if compute_counts has not "
          "been called before");
    }
    if(what.with_position && !has_positions) {
      stop("cannot report positions if they were not saved");
    }
    std::vector<SubSequence*>* ctxs = new std::vector<SubSequence*>{};
    std::vector<int> pre{};
    pre.reserve(x.size());
    if(when.max_length <= 0) {
      when.max_length = x.size();
    }
    root->subsequences(when, what, x, max_x + 1, pre, *ctxs);
    return ctxs;
  }

  // extract sub sequences based on length and counts constraints
  // to be extracted, a sub sequence must:
  // - be of length smaller or equal to max_length, unless max_length is
  // negative in this latter case, length is not taken into account
  // - have at least min_counts occurrences
  List subsequences(int min_counts, int max_length) {
    ExtractionConditions when(min_counts, max_length, false);
    ExtractionContent what(false, false, false, false);
    std::vector<SubSequence*>* ctxs = raw_subsequences(when, what);
    int nb = (int)ctxs->size();
    List the_contexts(nb);
    for(int i = 0; i < nb; i++) {
      the_contexts[i] = (*ctxs)[i]->sequence();
    }
    delete ctxs;
    return the_contexts;
  }

  // extract contexts based on length and counts constraints. A context
  // is a subsequence s that fulfils the constraints and in addition
  // such that among all the possible subsequence st (for t in the value
  // space) there is at least one that is not itself a subsequence.
  // The empty context is a potential candidate while it is never
  // returned by the subsequences method.
  List contexts(int min_counts, int max_length) {
    ExtractionConditions when(min_counts, max_length, true);
    ExtractionContent what(false, false, false, false);
    std::vector<SubSequence*>* ctxs = raw_subsequences(when, what);
    int nb = (int)ctxs->size();
    List the_contexts(nb);
    for(int i = 0; i < nb; i++) {
      the_contexts[i] = (*ctxs)[i]->sequence();
    }
    delete ctxs;
    return the_contexts;
  }

  // extract contexts as above but returned them in a more detailed
  // format (as data frame) including frequencies, positions and cutoff
  // values
  List full_contexts(int min_counts,
                     int max_length,
                     bool with_positions,
                     bool with_cutoff,
                     bool with_local_counts) {
    ExtractionConditions when(min_counts, max_length, true);
    ExtractionContent what(with_positions, with_cutoff, with_local_counts,
                           false);
    std::vector<SubSequence*>* ctxs = raw_subsequences(when, what);
    int nb = (int)ctxs->size();
    List the_contexts(nb);
    IntegerVector row_names(nb);
    std::vector<IntegerVector> counts;
    StringVector count_names(max_x + 2);
    count_names[0] = "freq";
    for(int k = 0; k <= max_x; k++) {
      count_names[k + 1] = std::to_string(k);
    }
    for(int k = 0; k <= max_x + 1; k++) {
      IntegerVector x(nb);
      counts.push_back(x);
    }
    std::vector<IntegerVector> l_counts;
    if(with_local_counts) {
      for(int k = 0; k <= max_x + 1; k++) {
        IntegerVector x(nb);
        l_counts.push_back(x);
      }
    }
    NumericVector cutoff;
    if(with_cutoff) {
      cutoff = NumericVector(nb);
    }
    for(int i = 0; i < nb; i++) {
      auto sub_seq = (*ctxs)[i];
      the_contexts[i] = sub_seq->sequence();
      sub_seq->insert_counts(counts, i);
      if(with_local_counts) {
        sub_seq->insert_local_counts(l_counts, i);
      }
      if(with_cutoff) {
        cutoff[i] = sub_seq->cut_off();
      }
      row_names[i] = i + 1;
    }
    int nb_cols = 2;
    List list_counts(max_x + 2);
    for(int k = 0; k <= max_x + 1; k++) {
      list_counts[k] = counts[k];
    }
    list_counts.attr("row.names") = row_names;
    list_counts.attr("names") = count_names;
    list_counts.attr("class") = "data.frame";
    if(with_local_counts) {
      nb_cols++;
    }
    if(with_cutoff) {
      nb_cols++;
    }
    if(with_positions) {
      nb_cols++;
    }
    List res(nb_cols);
    StringVector col_names(nb_cols);
    res[0] = the_contexts;
    res[1] = list_counts;
    col_names[0] = "context";
    col_names[1] = "counts";
    int next_col = 2;
    if(with_local_counts) {
      List list_local_counts(max_x + 2);
      for(int k = 0; k <= max_x + 1; k++) {
        list_local_counts[k] = l_counts[k];
      }
      res[next_col] = list_local_counts;
      col_names[next_col] = "local_counts";
      list_local_counts.attr("row.names") = row_names;
      list_local_counts.attr("names") = count_names;
      list_local_counts.attr("class") = "data.frame";
      next_col++;
    }
    if(with_positions) {
      List the_positions(nb);
      for(int i = 0; i < nb; i++) {
        the_positions[i] = ((int)x.size() - (*ctxs)[i]->positions());
      }
      res[next_col] = the_positions;
      col_names[next_col] = "positions";
      next_col++;
    }
    if(with_cutoff) {
      res[next_col] = cutoff;
      col_names[next_col] = "cutoff";
    }
    delete ctxs;
    res.attr("names") = col_names;
    return res;
  }

  void prune(int min_counts, int max_length) {
    if(!has_counts) {
      stop("prune cannot be used if the counts have not been computed");
    }
    if(max_length <= 0) {
      max_length = x.size();
    }
    max_depth = 0;  // we need to recompute max_depth
    nb_ctx = 0;
    root->prune(min_counts, max_length, -1, max_x + 1, x.size(), max_depth,
                nb_ctx);
  }

  void prune_context(int min_counts, int max_length, double K) {
    if(!has_counts) {
      stop("prune cannot be used if the counts have not been computed");
    }
    if(max_length <= 0) {
      max_length = x.size();
    }
    max_depth = 0;  // we need to recompute max_depth
    nb_ctx = 0;
    root->prune(min_counts, max_length, K, max_x + 1, x.size(), max_depth,
                nb_ctx);
  }

  SuffixTree* clone_prune(int min_counts, int max_length) const {
    if(!has_counts) {
      stop("prune cannot be used if the counts have not been computed");
    }
    if(max_length <= 0) {
      max_length = x.size();
    }
    int n_max_depth = 0;  // we need to recompute max_depth
    int nb_ctx = 0;
    EdgeNode* new_root = root->clone_prune(
        min_counts, max_length, -1, max_x + 1, x.size(), n_max_depth, nb_ctx);
    return clone_from_root(new_root, n_max_depth, nb_ctx);
  }

  SuffixTree* clone_prune_context(int min_counts,
                                  int max_length,
                                  double K) const {
    if(!has_counts) {
      stop("prune cannot be used if the counts have not been computed");
    }
    if(max_length <= 0) {
      max_length = x.size();
    }
    int n_max_depth = 0;  // we need to recompute max_depth
    int nb_ctx = 0;
    EdgeNode* new_root = root->clone_prune(min_counts, max_length, K, max_x + 1,
                                           x.size(), n_max_depth, nb_ctx);
    SuffixTree* result = clone_from_root(new_root, n_max_depth, nb_ctx);
    result->compute_reverse();
    return result;
  }

  int depth() const {
    if(has_counts) {
      return max_depth;
    } else {
      stop("depth is available only when counts have been calculated");
    }
  }

  int nb_contexts() const {
    if(nb_ctx > 0) {
      return nb_ctx;
    } else {
      stop("nb_contexts is only available if the tree was pruned");
    }
  }

  NumericVector cutoff() const {
    std::set<double> values;
    root->cutoff(values);
    NumericVector res(values.size());
    int i = 0;
    for(auto val : values) {
      res[i] = val;
      i++;
    }
    return res;
  }

  List representation() {
    std::vector<Rcpp::IntegerVector> tree_structure{};
    std::vector<Rcpp::IntegerVector> tree_counts{};
    root->flatten(x, max_x + 1, tree_structure, tree_counts);
    int nb = (int)tree_structure.size();
    List preres(nb);
    for(int k = 0; k < nb; k++) {
      if(tree_structure[k].size() > 0) {
        List val = List::create(Named("children") = tree_structure[k],
                                Named("f_by") = tree_counts[k]);
        preres[k] = val;
      } else {
        List val = List::create(Named("f_by") = tree_counts[k]);
        preres[k] = val;
      }
    }
    return preres;
  }

  void make_explicit() {
    root->make_explicit(x);
    full_explicit = true;
  }

  void compute_reverse() {
    if(!has_counts) {
      stop("reverse links can only be computed on suffix trees with counts");
    }
    if(!full_explicit) {
      stop("reverse links calculation is limited to fully explicit trees");
    }
    // root case
    root->reverse = new std::unordered_map<int, EdgeNode*>{};
    for(int i = 0; i <= max_x; i++) {
      if(auto child = root->children.find(i); child != root->children.end()) {
        (*(root->reverse))[i] = child->second;
      } else {
        (*(root->reverse))[i] = root;
      }
    }
    for(auto child : root->children) {
      if(child.first >= 0) {
        child.second->compute_reverse(x, root->reverse);
      }
    }
    has_reverse = true;
  }

  IntegerVector extend_left(const IntegerVector& y, int v) {
    if(!has_reverse) {
      stop("cannot extend to the left without reverse links");
    }
    if(y.size() == 0) {
      if(auto child = root->children.find(v); child != root->children.end()) {
        IntegerVector result(1);
        result[0] = v;
        return result;
      } else {
        return IntegerVector();
      }
    }
    Position pos = find_subsequence(y);
    if(pos.is_valid()) {
      std::vector<int> res{};
      EdgeNode* current = (*(pos.node->reverse))[v];
      while(current != nullptr && current->start >= 0) {
        res.push_back(x[current->start]);
        current = current->parent;
      }
      return IntegerVector(res.rbegin(), res.rend());
    } else {
      return IntegerVector();
    }
  }
  // loglikelihood calculation
  double loglikelihood(const IntegerVector& y,
                       bool extended,
                       bool verbose) const {
    if(!has_reverse) {
      stop("cannot compute likelihood without reverse links");
    }
    double result = 0;
    EdgeNode* current = root;
    int ny = y.size();
    for(int i = 0; i < ny; i++) {
      if(extended || i >= max_depth) {
        if(auto child = current->counts->find(y[i]);
           child != current->counts->end()) {
          if(child->second == 0) {
            if(verbose) {
              Rcout << i << " " << y[i]
                    << " zero occurrence (should not happen)!\n";
            }
            return -std::numeric_limits<double>::infinity();
          } else {
            if(verbose) {
              Rcout << y[i] << ": " << current << " -> " << child->second << "/"
                    << current->total_count << "\n";
            }
            result += log(((double)child->second) / current->total_count);
          }
        } else {
          if(verbose) {
            Rcout << current << " " << i << " " << y[i]
                  << " not found in counts!\n";
          }
          return -std::numeric_limits<double>::infinity();
        }
      }
      // we take the reverse link
      current = (*(current->reverse))[y[i]];
      // let's find out if we can increase the match
      int max_ctx = std::min(i + 1, max_depth);
      int pos = i - current->depth;
      while(current->depth < max_ctx) {
        if(auto child = current->children.find(y[pos]);
           child != current->children.end()) {
          current = child->second;
          pos--;
        } else {
          break;
        }
      }
    }
    return result;
  }

  int count_full_nodes() const { return root->count_full_nodes(max_x + 1); }

  double default_loglikelihood() const {
    if(!has_counts) {
      stop("log likelihood can only be computed on suffix trees with counts");
    }
    return root->loglikelihood(max_x + 1);
  }

  IntegerVector simulate(IntegerVector start, int n, int method) const {
    if(!has_reverse) {
      stop("cannot simulate without reverse links");
    }
    Nullable<Function> r_sample = R_NilValue;
    if(method > 1) {
      r_sample = Nullable(Function("sample.int"));
    }
    RNGScope scope;
    IntegerVector result(n);
    for(int k = 0; k < start.size(); k++) {
      result[k] = start[k];
    }
    int start_generating = start.size();
    EdgeNode* current = root;
    for(int i = 0; i < n; i++) {
      if(i >= start_generating) {
        switch(method) {
          case 0:
            result[i] = sample(current->counts, current->total_count);
            break;
          case 1:
            result[i] = sample2(current->counts, max_x, current->total_count);
            break;
          default:
            result[i] = Rcpp::as<IntegerVector>(Rcpp::as<Function>(r_sample)(
                            max_x + 1, (int)1, true,
                            map_to_counts(current->counts, max_x)))[0] -
                        1;
            break;
        }
      }
      // we take the reverse link
      current = (*(current->reverse))[result[i]];
      // let's find out if we can increase the match
      int max_ctx = std::min(i + 1, max_depth);
      int pos = i - current->depth;
      while(current->depth < max_ctx) {
        if(auto child = current->children.find(result[pos]);
           child != current->children.end()) {
          current = child->second;
          pos--;
        } else {
          break;
        }
      }
    }
    return result;
  }

  bool get_has_positions() const { return has_positions; }

  SuffixTree* trim() {
    EdgeNode* new_root = root->clone_trim();
    SuffixTree* result = clone_from_root(new_root, max_depth, nb_ctx);
    result->has_positions = false;
    result->compute_reverse();
    return result;
  }
};

SuffixTree* build_suffix_tree(const IntegerVector& x, int nb_vals) {
  SuffixTree* tree = new SuffixTree();
  tree->insert(x, nb_vals);
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
              "Count the occurrences of a NumericVector in the original one")
      .method("compute_counts", &SuffixTree::compute_counts,
              "Compute the counts possibly keeping the match positions")
      .method("counts", &SuffixTree::counts,
              "Return the counts associated to a subsequence")
      .method("positions", &SuffixTree::positions,
              "Return the positions of a subsequence")
      .method("subsequences", &SuffixTree::subsequences,
              "Return subsequences that fulfill specified conditions")
      .method("contexts", &SuffixTree::contexts,
              "Return contexts that fulfill specified conditions")
      .method("full_contexts", &SuffixTree::full_contexts,
              "Return detailed contexts that fulfill specified conditions")
      .method("prune", &SuffixTree::prune,
              "Prune the suffix tree based on the specified conditions")
      .method("prune_context", &SuffixTree::prune_context,
              "Prune the suffix tree based on the specified conditions")
      .method("clone_prune", &SuffixTree::clone_prune,
              "Prune the suffix tree based on the specified conditions and "
              "return a clone")
      .method("clone_prune_context", &SuffixTree::clone_prune_context,
              "Prune the suffix tree based on the specified conditions and "
              "return a clone")
      .method("cutoff", &SuffixTree::cutoff,
              "Return significant cut off values")
      .method("representation", &SuffixTree::representation,
              "Return a representation in R of the tree")
      .method("make_explicit", &SuffixTree::make_explicit,
              "Make all nodes explicit")
      .method("compute_reverse", &SuffixTree::compute_reverse,
              "Compute reverse links")
      .method("extend_left", &SuffixTree::extend_left,
              "Extend a subsequence to the left")
      .method("print_context", &SuffixTree::print_context, "Print a context")
      .method("depth", &SuffixTree::depth, "Return the depth of the tree")
      .method("nb_contexts", &SuffixTree::nb_contexts,
              "Return the number of contexts of the tree")
      .method("loglikelihood", &SuffixTree::loglikelihood,
              "Return the loglikelihood of a new sequence when the tree is "
              "interpreted as a VLMC")
      .method("count_full_nodes", &SuffixTree::count_full_nodes,
              "Return the number of full nodes in the suffix tree")
      .method("logLik", &SuffixTree::default_loglikelihood,
              "Return the extended loglikelihood of the tree interpreted as a "
              "VLMC")
      .method("simulate", &SuffixTree::simulate,
              "Simulate a sequence using this VLMC as the generative model")
      .property("has_positions", &SuffixTree::get_has_positions,
                "Does this VLMC store positions?")
      .method("trim", &SuffixTree::trim,
              "Reduce the memory usage of the tree by removing positions");
  function("build_suffix_tree", &build_suffix_tree);
}
