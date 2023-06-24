#include <Rcpp.h>
using namespace Rcpp;

//' Find a double just before another one
//'
//' This function returns a vector of values that are the "just before" the ones
//' given in input: a is just before b is a is the largest double strictly
//' smaller than b. This is implement with the \code{nextafter} C++ function.
//'
//' @param x vector of values
//' @return vector of just before values
//' @noRd
//'
// [[Rcpp::export]]
NumericVector before(NumericVector x) {
  int nx = x.size();
  NumericVector res(nx);

  for(int i = 0; i < nx; i++) {
    res[i] = nextafter(x[i], -INFINITY);
  }

  return res;
}
