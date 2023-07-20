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
