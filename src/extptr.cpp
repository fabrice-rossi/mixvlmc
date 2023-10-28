#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

extern "C" SEXP extptr_is_null_(SEXP ptr) {
  return Rf_ScalarLogical(!R_ExternalPtrAddr(ptr));
}
