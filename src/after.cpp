#include <Rcpp.h>
using namespace Rcpp;

//' Find a double just after another one
//'
//' This function returns a vector of values that are the "just after" the ones
//' given in input: a is just after b is a is the smallest double strictly
//' larger than b. This is implement with the \code{nextafter} C++ function.
//'
//' @param x vector of values
//' @return vector of just after values
//' @noRd
//'
// [[Rcpp::export]]
NumericVector after(NumericVector x) {
  int nx = x.size();
  NumericVector res(nx);

  for(int i = 0; i < nx; i++) {
    res[i] = nextafter(x[i], INFINITY);
  }

  return res;
}
