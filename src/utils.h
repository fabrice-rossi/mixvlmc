#ifndef UTILS_H
#define UTILS_H
#include <Rcpp.h>
#include <string>
#include <unordered_map>

std::string counts_to_string(const std::unordered_map<int, int>* counts);

Rcpp::IntegerVector map_to_counts(std::unordered_map<int, int>* counts,
                                  int max);

#endif
