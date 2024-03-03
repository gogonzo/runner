#include <Rcpp.h>
using namespace Rcpp;
#include "fill_run.h"

//' Fill NA with previous non-NA element
//'
//' Fill `NA` with last non-NA element.
//' @inheritParams runner
//' @param run_for_first If first elements are filled with `NA`, `run_for_first = TRUE`
//' allows to fill all initial `NA` with nearest non-NA value. By default
//' `run_for_first = TRUE`
//' @param only_within `NA` are replaced only if previous and next non-NA
//' values are the same. By default `only_within = TRUE`
//' @return vector - `x` containing all `x` elements with `NA`
//' replaced with previous non-NA element.
//' @examples
//' fill_run(c(NA, NA,1:10, NA, NA), run_for_first = TRUE)
//' fill_run(c(NA, NA,1:10, NA, NA), run_for_first = TRUE)
//' fill_run(c(NA, NA,1:10, NA, NA), run_for_first = FALSE)
//' fill_run(c(NA, NA, 1, 2, NA, NA, 2, 2, NA, NA, 1, NA, NA), run_for_first = TRUE, only_within = TRUE)
//' @export
// [[Rcpp::export]]
SEXP fill_run(SEXP x, bool run_for_first = false, bool only_within = false)
{

  switch (TYPEOF(x))
  {
  case INTSXP:
    return fill::fill_run(as<IntegerVector>(x), run_for_first, only_within);
  case REALSXP:
    return fill::fill_run(as<NumericVector>(x), run_for_first, only_within);
  case STRSXP:
    return fill::fill_run(as<CharacterVector>(x), run_for_first, only_within);
  case LGLSXP:
    return fill::fill_run(as<LogicalVector>(x), run_for_first, only_within);
  case CPLXSXP:
    return fill::fill_run(as<ComplexVector>(x), run_for_first, only_within);
  default:
  {
    stop("Invalid data type - only integer, numeric, character, factor, date vectors are possible.");
  }
  }
}
