#include <Rcpp.h>
using namespace Rcpp;
#include "lag_run.h"

//' Lag dependent on variable
//'
//' Vector of input lagged along integer vector
//' @param x Vector of any type
//' @param k integer vector which specifies window length
//' @param indexes an optional integer vector containing index of observations.
//' @examples
//' lag_run(1:10, k=3)
//' lag_run(letters[1:10],k=2, indexes=c(1,1,1,2,3,4,6,7,8,10))
//' @export
// [[Rcpp::export]]
SEXP lag_run(SEXP x, IntegerVector k = 0, IntegerVector indexes = 1) {

  if( (indexes.size()==1) & (k.size()==1)) {
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
  } else if( (indexes.size()==1) & (k.size()>1)) {
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
  } else if( (indexes.size() > 1) & (k.size()==1) ){
    switch (TYPEOF(x)) {
    case INTSXP: return lag::lag_run21(as<IntegerVector>(x), k(0), indexes);
    case REALSXP: return lag::lag_run21(as<NumericVector>(x), k(0), indexes);
    case STRSXP: return lag::lag_run21(as<CharacterVector>(x), k(0), indexes);
    default: {
      warning(
        "Invalid SEXPTYPE %d (%s).\n",
        TYPEOF(x), type2name(x)
      );
      return R_NilValue;
    }
    }
  } else if( (indexes.size() > 1) & (k.size()>1) ){
    switch (TYPEOF(x)) {
    case INTSXP: return lag::lag_run22(as<IntegerVector>(x), k, indexes);
    case REALSXP: return lag::lag_run22(as<NumericVector>(x), k, indexes);
    case STRSXP: return lag::lag_run22(as<CharacterVector>(x), k, indexes);
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
