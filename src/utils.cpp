#include "utils.h"

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
    if(auto ccount = c_counts->find(pcount.first); ccount != c_counts->end()) {
      res += log(((double)ccount->second) * p_total /
                 (((double)pcount.second) * c_total)) *
             ccount->second;
    }
  }
  return res;
}
