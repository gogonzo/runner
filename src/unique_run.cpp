#include <Rcpp.h>
using namespace Rcpp;
#include "unique_run.h"

//' List of running windows
//'
//' Creates list of windows
//' @param x Vector of any type
//' @param k integer vector which specifies window length
//' @param idx an optional integer vector containing idx numbers of observation.
//' @examples
//' unique_run(1:10, k=3)
//' unique_run(letters[1:10],k=c(1,2,2,4,5,5,5,5,5,5))
//' @export
// [[Rcpp::export]]
SEXP unique_run( SEXP x, IntegerVector k=0, IntegerVector idx = 1) {

  int n = Rf_length(x);

  if( k(0) == 0 ){
    k(0) = n;
  } else if(k.size() != n and k.size() > 1){
    stop("length of k and length x differs. k=0 and k=length(x) only allowed");
  } else if( Rcpp::any(Rcpp::is_na(k)) ){
    stop("Function doesn't accept NA values in k vector");
  }

  if( idx.size() > 1){
    switch (TYPEOF(x)) {
    case INTSXP: return unique::unique_to_list_int(as<IntegerVector>(x), k, idx);
    case REALSXP: return unique::unique_to_list_int(as<NumericVector>(x), k, idx);
    case STRSXP: return unique::unique_to_list_int(as<CharacterVector>(x), k, idx);
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
    case INTSXP: return unique::unique_to_list(as<IntegerVector>(x), k);
    case REALSXP: return unique::unique_to_list(as<NumericVector>(x), k);
    case STRSXP: return unique::unique_to_list(as<CharacterVector>(x), k);
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
