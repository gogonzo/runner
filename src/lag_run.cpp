#include <Rcpp.h>
using namespace Rcpp;
#include "checks.h"
#include "lag_run.h"

//' Lag dependent on variable
//'
//' Vector of input lagged along integer vector
//' @inheritParams runner
//' @inheritParams sum_run
//' @param nearest `logical` single value. Applied when `idx` is used,
//' then `nearest = FALSE` returns observation lagged exactly by the
//' specified number of "periods". When `nearest = TRUE`
//' function returns latest observation within lag window.
//' @examples
//' lag_run(1:10, lag = 3)
//' lag_run(letters[1:10], lag = -2, idx = c(1, 1, 1, 2, 3, 4, 6, 7, 8, 10))
//' lag_run(letters[1:10], lag = 2, idx = c(1, 1, 1, 2, 3, 4, 6, 7, 8, 10), nearest = TRUE)
//' @export
// [[Rcpp::export]]
SEXP lag_run(SEXP x,
             IntegerVector lag = 1,
             IntegerVector idx = IntegerVector(0),
             bool nearest = false)
{
  int n = Rf_length(x);

  checks::check_idx(idx, n, "x");
  checks::check_lag(lag, n, "x");

  if ((idx.size() == 0) && (lag.size() == 1))
  {
    switch (TYPEOF(x))
    {
    case INTSXP:
      return lag::lag_run11(as<IntegerVector>(x), lag(0));
    case REALSXP:
      return lag::lag_run11(as<NumericVector>(x), lag(0));
    case STRSXP:
      return lag::lag_run11(as<CharacterVector>(x), lag(0));
    case LGLSXP:
      return lag::lag_run11(as<LogicalVector>(x), lag(0));
    case CPLXSXP:
      return lag::lag_run11(as<ComplexVector>(x), lag(0));
    default:
    {
      stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
    }
    }
  }
  else if ((idx.size() == 0) && (lag.size() > 1))
  {
    switch (TYPEOF(x))
    {
    case INTSXP:
      return lag::lag_run12(as<IntegerVector>(x), lag);
    case REALSXP:
      return lag::lag_run12(as<NumericVector>(x), lag);
    case STRSXP:
      return lag::lag_run12(as<CharacterVector>(x), lag);
    case LGLSXP:
      return lag::lag_run12(as<LogicalVector>(x), lag);
    case CPLXSXP:
      return lag::lag_run12(as<ComplexVector>(x), lag);
    default:
    {
      stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
    }
    }
  }
  else if ((idx.size() == n) && (lag.size() == 1))
  {
    switch (TYPEOF(x))
    {
    case INTSXP:
      return lag::lag_run21(as<IntegerVector>(x), lag(0), idx, nearest);
    case REALSXP:
      return lag::lag_run21(as<NumericVector>(x), lag(0), idx, nearest);
    case STRSXP:
      return lag::lag_run21(as<CharacterVector>(x), lag(0), idx, nearest);
    case LGLSXP:
      return lag::lag_run21(as<LogicalVector>(x), lag(0), idx, nearest);
    case CPLXSXP:
      return lag::lag_run21(as<ComplexVector>(x), lag(0), idx, nearest);
    default:
    {
      stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
    }
    }
  }
  else if ((idx.size() == n) && (lag.size() > 1))
  {
    switch (TYPEOF(x))
    {
    case INTSXP:
      return lag::lag_run22(as<IntegerVector>(x), lag, idx, nearest);
    case REALSXP:
      return lag::lag_run22(as<NumericVector>(x), lag, idx, nearest);
    case STRSXP:
      return lag::lag_run22(as<CharacterVector>(x), lag, idx, nearest);
    case LGLSXP:
      return lag::lag_run22(as<LogicalVector>(x), lag, idx, nearest);
    case CPLXSXP:
      return lag::lag_run22(as<ComplexVector>(x), lag, idx, nearest);
    default:
    {
      stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
    }
    }
  }

  return R_NilValue;
}
