#include <Rcpp.h>
using namespace Rcpp;
#include "lag_run.h"

//' Lag dependent on variable
//'
//' Vector of input lagged along integer vector
//' @param x Vector of any type
//' @param k integer vector which specifies window length
//' @param idx an optional integer vector containing index of observations.
//' @examples
//' lag_run(1:10, k=3)
//' lag_run(letters[1:10],k=2, idx=c(1,1,1,2,3,4,6,7,8,10))
//' @export
// [[Rcpp::export]]
SEXP lag_run(SEXP x, IntegerVector k = 1, IntegerVector idx = 1) {

  if( (idx.size()==1) & (k.size()==1)) {
    switch (TYPEOF(x)) {
    case INTSXP: return lag::lag_run11(as<IntegerVector>(x), k(0));
    case REALSXP: return lag::lag_run11(as<NumericVector>(x), k(0));
    case STRSXP: return lag::lag_run11(as<CharacterVector>(x), k(0));
    default: {
      warning(
        "Invalid SEXPTYPE %d (%s).\n",
        TYPEOF(x), type2name(x)
      );
      return R_NilValue;
    }
    }
  } else if( (idx.size()==1) & (k.size()>1)) {
    switch (TYPEOF(x)) {
    case INTSXP: return lag::lag_run12(as<IntegerVector>(x), k);
    case REALSXP: return lag::lag_run12(as<NumericVector>(x), k);
    case STRSXP: return lag::lag_run12(as<CharacterVector>(x), k);
    default: {
      warning(
        "Invalid SEXPTYPE %d (%s).\n",
        TYPEOF(x), type2name(x)
      );
      return R_NilValue;
    }
    }
  } else if( (idx.size() > 1) & (k.size()==1) ){
    switch (TYPEOF(x)) {
    case INTSXP: return lag::lag_run21(as<IntegerVector>(x), k(0), idx);
    case REALSXP: return lag::lag_run21(as<NumericVector>(x), k(0), idx);
    case STRSXP: return lag::lag_run21(as<CharacterVector>(x), k(0), idx);
    default: {
      warning(
        "Invalid SEXPTYPE %d (%s).\n",
        TYPEOF(x), type2name(x)
      );
      return R_NilValue;
    }
    }
  } else if( (idx.size() > 1) & (k.size()>1) ){
    switch (TYPEOF(x)) {
    case INTSXP: return lag::lag_run22(as<IntegerVector>(x), k, idx);
    case REALSXP: return lag::lag_run22(as<NumericVector>(x), k, idx);
    case STRSXP: return lag::lag_run22(as<CharacterVector>(x), k, idx);
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
