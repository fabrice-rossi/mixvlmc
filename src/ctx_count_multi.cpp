#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
List forward_match_all_ctx_counts_multi(List xs,
                                        int nb_vals,
                                        int depth,
                                        Nullable<List> nvs_from,
                                        Nullable<NumericVector> weights) {
  int xs_l = xs.length();
  List result(nb_vals);
  List all_counts(xs_l);
  for(int j = 0; j < nb_vals; j++) {
    result[j] = List(xs_l);
  }
  for(int i = 0; i < xs_l; i++) {
    IntegerVector x = as<IntegerVector>(xs[i]);
    IntegerMatrix counts(nb_vals, nb_vals);
    std::vector<std::vector<int>> pos;
    for(int j = 0; j < nb_vals; j++) {
      pos.push_back(std::vector<int>());
    }
    if(nvs_from.isNotNull()) {
      int nx = x.size();
      List nvs(nvs_from);
      if(nvs[i] != R_NilValue) {
        IntegerVector from = as<IntegerVector>(nvs[i]);
        int nv = from.size();
        for(int k = 0; k < nv; k++) {
          int posish = from[k] - 1;
          if(posish >= 0) {
            pos[x[posish]].push_back(posish);
            int end = from[k] + depth;
            if(end < nx) {
              counts(x[posish], x[end])++;
            }
          }
        }
      }
    } else {
      int nx = x.size() - 1;
      for(int j = 0; j < nx; j++) {
        pos[x[j]].push_back(j);
        counts(x[j], x[j + 1])++;
      }
      pos[x[nx]].push_back(nx);
    }
    for(int j = 0; j < nb_vals; j++) {
      List inner = as<List>(result[j]);
      inner[i] = pos[j];
    }
    all_counts[i] = counts;
  }
  if(weights.isNotNull()) {
    NumericMatrix agg_counts(nb_vals, nb_vals);
    NumericVector w(weights);
    for(int i = 0; i < xs_l; i++) {
      double the_w = w[i];
      IntegerMatrix counts = all_counts[i];
      for(int k = 0; k < nb_vals; k++) {
        for(int j = 0; j < nb_vals; j++) {
          agg_counts(j, k) += the_w * counts(j, k);
        }
      }
    }
    return List::create(_["positions"] = result, _["counts"] = all_counts,
                        _["agg_counts"] = agg_counts);
  } else {
    IntegerMatrix agg_counts(nb_vals, nb_vals);
    for(int i = 0; i < xs_l; i++) {
      IntegerMatrix counts = all_counts[i];
      for(int k = 0; k < nb_vals; k++) {
        for(int j = 0; j < nb_vals; j++) {
          agg_counts(j, k) += counts(j, k);
        }
      }
    }
    return List::create(_["positions"] = result, _["counts"] = all_counts,
                        _["agg_counts"] = agg_counts);
  }
}
