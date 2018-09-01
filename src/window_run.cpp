#include <Rcpp.h>
using namespace Rcpp;
#include "window_run.h"

//' List of running windows
//'
//' Creates list of windows
//' @param x Vector of any type
//' @param k integer vector which specifies window length
//' @param indexes an optional integer vector containing indexes numbers of observation.
//' @examples
//' window_run(1:10, k=3)
//' window_run(letters[1:10],k=c(1,2,2,4,5,5,5,5,5,5))
//' @export
// [[Rcpp::export]]
SEXP window_run(SEXP x, IntegerVector k = 0, IntegerVector indexes = 1) {

  int n = Rf_length(x);

  if( k(0) == 0 ){
    k(0) = n;
  } else if(k.size() != n and k.size() > 1){
    stop("length of k and length x differs. k=0 and k=length(x) only allowed");
  } else if( Rcpp::any(Rcpp::is_na(k)) ){
    stop("Function doesn't accept NA values in k vector");
  }


  if( indexes.size() > 1){
    switch (TYPEOF(x)) {
    case INTSXP: return window::window_to_list_int(as<IntegerVector>(x), k, indexes);
    case REALSXP: return window::window_to_list_int(as<NumericVector>(x), k, indexes);
    case STRSXP: return window::window_to_list_int(as<CharacterVector>(x), k, indexes);
    default: {
      warning(
        "Invalid SEXPTYPE %d (%s).\n",
        TYPEOF(x), type2name(x)
      );
      return R_NilValue;
    }
    }
  } else {
    switch (TYPEOF(x)) {
    case INTSXP: return window::window_to_list(as<IntegerVector>(x), k);
    case REALSXP: return window::window_to_list(as<NumericVector>(x), k);
    case STRSXP: return window::window_to_list(as<CharacterVector>(x), k);
    default: {
      warning(
        "Invalid SEXPTYPE %d (%s).\n",
        TYPEOF(x), type2name(x)
      );
      return R_NilValue;
    }
    }


  }

  return R_NilValue;
}
