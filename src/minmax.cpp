#include <Rcpp.h>
using namespace Rcpp;

//' Running min/max
//'
//'
//' \code{min_run} calculates running min on given \code{x} numeric vector, specified \code{k} window size.
//' @inheritParams runner
//' @inheritParams sum_run
//' @return list.
//' @export
// [[Rcpp::export]]
List minmax(NumericVector x,
            IntegerVector k = IntegerVector(1),
            IntegerVector lag = IntegerVector(1),
            IntegerVector idx = IntegerVector(1),
            bool na_rm = true,
            bool na_pad = true) {
  List res;
  IntegerVector mini = 0;
  IntegerVector maxi = 0;
  NumericVector cur_min = 0;
  NumericVector cur_max = 0;



  return res;
}
