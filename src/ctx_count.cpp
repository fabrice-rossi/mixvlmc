#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
List forward_match_all_ctx_counts(NumericVector x, int nb_vals, int depth=0, Nullable<NumericVector> nv_from=R_NilValue) {
  int nx = x.size();
  std::vector<std::vector<int>> result;
  for(int i=0;i<nb_vals;i++) {
    result.push_back(std::vector<int>());
  }
  // filled with 0s
  NumericMatrix counts(nb_vals, nb_vals);
  if(nv_from.isNotNull()) {
    NumericVector from(nv_from);
    int nv = from.size();
    for(int i=0;i<nv; i++) {
      int pos = from[i] - 1;
      if(pos>=0) {
        result[x[pos]].push_back(pos);
        int end = from[i] + depth;
        if(end < nx) {
          counts(x[pos], x[end])++;
        }
      }
    }
  } else {
    nx = nx - 1;
    for(int i=0;i<nx;i++) {
      result[x[i]].push_back(i);
      counts(x[i], x[i+1])++;
    }
    result[x[nx]].push_back(nx);
  }
  return List::create(_["positions"]=result,
                      _["counts"]=counts);
}
