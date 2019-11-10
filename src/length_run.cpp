#include <Rcpp.h>
#include "checks.h"
using namespace Rcpp;

//' Length of running windows
//'
//' Number of elements in k-long window calculated on \code{idx} vector.
//' If \code{idx} is an `as.integer(date)` vector, then k=number of days in window -
//' then the result is number of observations within k days window.
//' @inheritParams runner
//' @inheritParams sum_run
//' @examples
//' length_run(k = 3, idx = c(1, 2, 2, 4, 5, 5, 5, 5, 5, 5))
//' @export
// [[Rcpp::export]]
IntegerVector length_run(IntegerVector k = IntegerVector(1),
                         IntegerVector lag = IntegerVector(1),
                         IntegerVector idx = IntegerVector(0)) {
  int n = idx.size();
  if (n == 0) {
    stop("idx should be of length > 0");
  }

  checks::check_k(k, n);
  checks::check_idx(idx, n);
  checks::check_lag(lag, n);

  IntegerVector res(n);
  if ((k.size() == 1)) {
    for (int i = 0; i < n; i++) {
      for (int j = i; j >= 0; j--) {
        if ((idx(i) - idx(j)) > (k(0) - 1)) {
          res(i) = i - j;
          break;
        } else if (j == 0){
          res(i) = NA_INTEGER;
        }
      }
    }

  // IDX VARYING WINDOW -----------
  } else if((k.size() > 1)) {
    for (int i = 0; i < n; i++) {
      for (int j = i; j >= 0; j--) {
        if ((idx(i) - idx(j)) > (k(i) - 1)) {
          res(i) = i - j;
          break;
        } else if (j == 0) {
          res(i) = NA_INTEGER;
        }
      }
    }
  }
  return res;
}
