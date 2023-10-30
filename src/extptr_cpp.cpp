#include <Rcpp.h>

// [[Rcpp::export]]
SEXP extptr_is_null(SEXP ptr) {
  return Rf_ScalarLogical(!R_ExternalPtrAddr(ptr));
}
