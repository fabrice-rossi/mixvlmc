#include <algorithm>
#include "utils.h"

// used only by the debugging functions print_tree() and print_context()
// # nocov start
std::string counts_to_string(const std::unordered_map<int, int>* counts) {
  if(counts->size() == 0) {
    return "[ ]";
  } else {
    std::string res = "[ ";
    for(auto count : (*counts)) {
      res += std::to_string(count.first);
      res += " -> ";
      res += std::to_string(count.second);
      res += ", ";
    }
    return res.substr(0, res.length() - 2) + " ]";
  }
}
// # nocov end

Rcpp::IntegerVector map_to_counts(std::unordered_map<int, int>* counts,
                                  int max) {
  Rcpp::IntegerVector result(max + 1);
  for(auto count : (*counts)) {
    result[count.first] = count.second;
  }
  return result;
}

double kl_criterion(const std::unordered_map<int, int>* c_counts,
                    int c_total,
                    const std::unordered_map<int, int>* p_counts,
                    int p_total) {
  double res = 0;
  for(auto pcount : *p_counts) {
    auto ccount = c_counts->find(pcount.first);
    if(ccount != c_counts->end()) {
      res += log(((double)ccount->second) * p_total /
                 (((double)pcount.second) * c_total)) *
             ccount->second;
    }
  }
  return res;
}

int sample(const std::unordered_map<int, int>* counts, int max, int total) {
  // the states are considered in increasing order (the iteration order of an
  // unordered_map is implementation dependent, which would make the results
  // depend on the platform)
  double bound = total * unif_rand();
  double current = 0;
  int last = 0;
  for(int k = 0; k <= max; k++) {
    auto count = counts->find(k);
    if(count != counts->end() && count->second > 0) {
      current += count->second;
      if(current >= bound) {
        return k;
      }
      last = k;
    }
  }
  // not reached as bound <= total, kept for safety
  return last;  // # nocov
}

int sample2(const std::unordered_map<int, int>* counts, int max, int total) {
  std::vector<int> vals(max + 1);
  std::iota(vals.begin(), vals.end(), 0);
  std::vector<double> probs(max + 1, 0.0);
  for(auto count : *counts) {
    if(count.second > 0) {
      probs[count.first] = ((double)count.second) / total;
    }
  }
  std::stable_sort(vals.begin(), vals.end(),
            [&probs](int i1, int i2) { return probs[i1] > probs[i2]; });
  double ru = unif_rand();
  double current = 0;
  int i = 0;
  for(; i <= max; i++) {
    current += probs[vals[i]];
    if(current >= ru) {
      break;
    }
  }
  return vals[i];
}
