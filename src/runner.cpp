#include <Rcpp.h>
using namespace Rcpp;
#include "runner.h"

//' Custom running function
//'
//' Applies custom function to running windows
//' @param x Vector of any type
//' @param f R function to be applied on `x`
//' @param k integer vector which specifies window length
//' @param idx an optional integer vector containing index of observations.
//' @examples
//' runner(1:10, f = mean, k = 3)
//' runner(1:10, k = 3, f = function(x) mean(x, na.rm = TRUE))
//' runner(letters[1:10], k = c(1,2,2,4,5,5,5,5,5,5), f = function(x) length(unique(x)))
//' @export
// [[Rcpp::export]]
SEXP runner(SEXP x, IntegerVector k = 0, IntegerVector idx = 1, Function f = R_NilValue) {

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
    case INTSXP: return apply::runner_on_date(as<IntegerVector>(x), k, idx, f);
    case REALSXP: return apply::runner_on_date(as<NumericVector>(x), k, idx, f);
    case STRSXP: return apply::runner_on_date(as<CharacterVector>(x), k, idx, f);
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
    case INTSXP: return apply::runner_simple(as<IntegerVector>(x), k, f);
    case REALSXP: return apply::runner_simple(as<NumericVector>(x), k, f);
    case STRSXP: return apply::runner_simple(as<CharacterVector>(x), k, f);
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
