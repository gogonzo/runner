#include <Rcpp.h>
using namespace Rcpp;
#include "window_run.h"

//' List of running windows
//'
//' Creates list of windows
//' @param x Vector of any type
//' @param k integer vector which specifies window length
//' @param unit character describing unit of window length. Default is "element". Date units are optional ("day","month","year").
//' @examples
//' window_run(1:10, k=3)
//' window_run(letters[1:10],k=c(1,2,2,4,5,5,5,5,5,5))
//' @export
// [[Rcpp::export]]
SEXP window_run(SEXP x, IntegerVector k = 0, IntegerVector interval = 1) {

  if( interval.size() > 1){
    switch (TYPEOF(x)) {
    case INTSXP: return window::window_to_list_int(as<IntegerVector>(x), k, interval);
    case REALSXP: return window::window_to_list_int(as<NumericVector>(x), k, interval);
    case STRSXP: return window::window_to_list_int(as<CharacterVector>(x), k, interval);
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
