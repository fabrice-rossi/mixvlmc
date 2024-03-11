#ifndef CTX_COUNT_H
#define CTX_COUNT_H
#include <Rcpp.h>

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
//' The function operates in two modes:
//' 1) if nv_from is NULL (default case), the function reports the positions and the counts exactly
//'    as described above. The \code{depth} parameter is not used.
//' 2) if ny_from is not NULL, it contains reference positions for the search.
//'    The reported positions correspond to occurrences of the values just before a position
//'    in nv_from. For instance if nv_from = c(2, 4) (c++ indexing from 0) and
//'    x = c(0, 1, 0, 1, 0, 1, 0, 1), the function only looks are positions 1 and 3 in x,
//'    which contain both 1. Thus the positions list will have an empty vector in component 1
//'    (no occurrence of 0) and c(1, 3) for component 2.
//'    Entry [k, l] of the counts matrix still counts the occurrences of l following
//'    an occurrence of k, but with a shift of depth+1. In the previous example, if depth=1,
//'    we consider positions c(3, 5) and therefore counts[2,2]=2 (all other values are 0).
//'
//' @param x the vector in which contexts are searched for.
//' @param nb_vals defines the interval of expected values from 0 to \code{nb_vals}-1.
//' @param depth shift in the occurrence counts.
//' @param nv_from starting positions for the search.
//'
//' @return a list described above.
//' @noRd
 Rcpp::List forward_match_all_ctx_counts(
    Rcpp::IntegerVector x,
    int nb_vals,
    int depth,
    Rcpp::Nullable<Rcpp::IntegerVector> nv_from);

#endif
