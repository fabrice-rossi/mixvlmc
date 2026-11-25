#ifndef UTILS_H
#define UTILS_H
#include <Rcpp.h>
#include <string>
#include <unordered_map>

std::string counts_to_string(const std::unordered_map<int, int>* counts);

Rcpp::IntegerVector map_to_counts(std::unordered_map<int, int>* counts,
                                  int max);

// computes the Kullback-Liebler divergence based criterion used by the
// context algorithm using directly the couts maps
double kl_criterion(const std::unordered_map<int, int>* c_counts,
                    int c_total,
                    const std::unordered_map<int, int>* p_counts,
                    int p_total);

int sample(const std::unordered_map<int, int>* counts, int max, int total);

int sample2(const std::unordered_map<int, int>* counts, int max, int total);

// checks for a user interruption (and R time limits) every 65536 iterations
// of a long loop
inline void check_interrupt(std::size_t k) {
  if((k & 0xFFFF) == 0) {
    Rcpp::checkUserInterrupt();
  }
}

#endif
