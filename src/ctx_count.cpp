#include <Rcpp.h>
using namespace Rcpp;

//' Find values in a vector
//'
//' This function counts and reports all the positions of the accepted values in a vector.
//' The vector must contain only integers from 0 to \code{nb_vals}-1. The output of the function
//' is a list with two components:
//'
//' 1) \code{positions}: a list of nb_vals vectors. Component k of the list contains
//'     the positions of all the k-1 in \code{x} (using C++ convention, positions start at 0)
//' 2) \code{counts}: a matrix of size nb_vals times nb_vals. Entry [k, l] gives the number of
//'    times k-1 is followed by l-1 in \code{x}
//'
//' The function operates in two modes.
//' 1) if nv_from is NULL (default case), the function reports the positions and the counts exactly
//'    as described above. The \code{depth} parameter is not used.
//' 2) if ny_from is not NULL, it contains reference positions for the search.
//'    The reported positions correspond to occurences of the values just before a position
//'    in nv_from. For instance if nv_from = c(2, 4) (c++ indexing from 0) and
//'    x = c(0, 1, 0, 1, 0, 1, 0, 1), the function only looks are positions 1 and 3 in x,
//'    which contain both 1. Thus the positions list will have an empty vector in component 1
//'    (no occurence of 0) and c(1, 3) for component 2.
//'    Entry [k, l] of the counts matrix still counts the occurences of l following
//'    an occurence of k, but with a shift of depth+1. In the previous example, if depth=1,
//'    we consider positions c(3, 5) and therefore counts[2,2]=2 (all other values are 0).
//'
//' @param x the vector in which contexts are searched for
//' @param nb_vals defines the interval of expected values from 0 to \code{nb_vals}-1
//' @param depth shift in the occurence counts
//' @param nv_from starting positions for the search
//'
//' @return a list described above
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
