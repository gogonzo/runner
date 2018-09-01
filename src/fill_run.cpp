#include <Rcpp.h>
using namespace Rcpp;
#include "fill_run.h"

//' Fill NA with previous non-NA element
//'
//' Fill \code{NA} with last non-NA element.
//' @param x Vector of any type where \code{NA} are replaced
//' @param run_for_first If first elements are filled with \code{NA}, \code{run_for_first = TRUE } allows to fill all initial \code{NA} with nearest non-NA value. By befault \code{run_for_first = TRUE}
//' @param only_within \code{NA} are replaced only if previous and next non-NA values are the same. By befault \code{only_within = TRUE}
//' @return numeric vector of length equals length of \code{x} containing all \code{x} elements with \code{NA} replaced with previous non-NA element.
//' @examples
//' fill_run(c(NA,NA,1:10, NA, NA), run_for_first=TRUE)
//' fill_run(c(NA,NA,1:10, NA, NA), run_for_first=TRUE)
//' fill_run(c(NA,NA,1:10, NA, NA), run_for_first=FALSE)
//' fill_run(c(NA,NA,1,2,NA,NA,2,2,NA,NA,1, NA, NA), run_for_first=TRUE,only_within = TRUE)
//' @export
// [[Rcpp::export]]
SEXP fill_run(SEXP x, bool run_for_first = false, bool only_within=false) {

  switch (TYPEOF(x)) {
  case INTSXP: return fill::fill_run(  as<IntegerVector>(x), run_for_first,only_within);
  case REALSXP: return fill::fill_run( as<NumericVector>(x), run_for_first,only_within);
  case STRSXP: return fill::fill_run(  as<CharacterVector>(x), run_for_first,only_within);
  case LGLSXP: return fill::fill_run(  as<LogicalVector>(x), run_for_first,only_within);
  case CPLXSXP: return fill::fill_run( as<ComplexVector>(x), run_for_first,only_within);
  default: {
    warning(
      "Invalid SEXPTYPE %d (%s).\n",
      TYPEOF(x), type2name(x)
    );
    return 0;
  }
  }
}
