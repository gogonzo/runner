#include <Rcpp.h>
#include <algorithm>
#include "checks.h"
using namespace Rcpp;

//' Length of running windows
//'
//' Number of elements in k-long window calculated on `idx` vector.
//' If `idx` is an `as.integer(date)` vector, then k=number of days in window -
//' then the result is number of observations within k days window.
//' @inheritParams runner
//' @inheritParams sum_run
//' @examples
//' length_run(k = 3, idx = c(1, 2, 2, 4, 5, 5, 5, 5, 5, 5))
//' @export
// [[Rcpp::export]]
IntegerVector length_run(IntegerVector k = IntegerVector(1),
                         IntegerVector lag = IntegerVector(1),
                         IntegerVector idx = IntegerVector(0))
{
  int n = idx.size();
  if (n == 0)
  {
    stop("idx should be of length > 0");
  }

  checks::check_k(k, n, "idx");
  checks::check_idx(idx, n, "idx");
  checks::check_lag(lag, n, "idx");

  IntegerVector res(n);
  if ((k.size() == 1))
  {
    int kk = k(0);
    for (int i = 0; i < n; i++)
    {
      // Find last position j where idx(i) - idx(j) > kk - 1, i.e., idx(j) < idx(i) - kk + 1
      int target = idx(i) - kk + 1;
      // lower_bound finds first position with idx >= target
      auto it = std::lower_bound(idx.begin(), idx.begin() + i + 1, target);
      int p = (int)(it - idx.begin());

      if (p == 0)
      {
        // All positions 0..i have idx >= target, no j with idx(j) < target
        res(i) = NA_INTEGER;
      }
      else
      {
        // j = p - 1 is the last position with idx(j) < target
        res(i) = i - (p - 1);
      }
    }
  }
  else if ((k.size() > 1))
  {
    for (int i = 0; i < n; i++)
    {
      int target = idx(i) - k(i) + 1;
      auto it = std::lower_bound(idx.begin(), idx.begin() + i + 1, target);
      int p = (int)(it - idx.begin());

      if (p == 0)
      {
        res(i) = NA_INTEGER;
      }
      else
      {
        res(i) = i - (p - 1);
      }
    }
  }
  return res;
}
