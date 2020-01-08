#include <Rcpp.h>

//' Running min/max
//'
//'
//' \code{min_run} calculates running minimum-maximum on given \code{x} numeric
//'  vector, specified \code{k} window size.
//' @inheritParams runner
//' @inheritParams sum_run
//' @param metric \code{character} what to return, minimum or maximum
//' @return list.
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector minmax_run(
    Rcpp::NumericVector const& x,
    std::string metric = "min",
    bool na_rm = true) {

  int n = x.size();

  double prev;
  double cur;
  double temp_max = x(0);
  double temp_min = x(0);
  double last_max = x(0);
  double last_min = x(0);

  Rcpp::NumericVector res(n);
  res(0) = x(0);
  Rcpp::NumericVector mins = Rcpp::NumericVector(n);
  Rcpp::NumericVector maxes = Rcpp::NumericVector(n);

  for (int i = 1; i < n; ++i) {
    if (Rcpp::NumericVector::is_na(x(i)) && !na_rm) {
      res(i) = NA_REAL;
    } else {
      prev = x(i - 1);
      cur = x(i);

      if (prev > last_max && cur < prev) {
        last_max = prev;
        last_min = temp_min;
        temp_min = cur;
      } else if (prev < last_min && cur > prev) {
        last_min = prev;
        last_max = temp_max;
        temp_max = cur;
      }

      if (cur < temp_min) temp_min = cur;
      if (cur > temp_max) temp_max = cur;

      res(i) = (metric == "min") ? last_min : last_max;
    }
  }
  return res;
}
