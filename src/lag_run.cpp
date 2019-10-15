#include <Rcpp.h>
using namespace Rcpp;
#include "lag_run.h"

//' Lag dependent on variable
//'
//' Vector of input lagged along integer vector
//' @inheritParams runner
//' @inheritParams sum_run
//' @param nearest \code{logical} single value. Applied when \code{idx} is used, then \code{nearest = FALSE} returns
//' observation lagged exactly by the specified number of "periods". When \code{nearest = TRUE}
//' function returns latest observation within lag window.
//' @examples
//' lag_run(1:10, k = 3)
//' lag_run(letters[1:10], k = 2, idx = c(1, 1, 1, 2, 3, 4, 6, 7, 8, 10))
//' @export
// [[Rcpp::export]]
SEXP lag_run(SEXP x, IntegerVector k = 1, IntegerVector idx = 1, bool nearest = false) {
  int n = Rf_length(x);

  if (k.size() == 1 && k(0) == 0) {
    k(0) = n;
  } else if (k.size() != n and k.size() > 1) {
    stop("length of k and length of x differs. length(k) should be 1 or equal to x");
  } else if (Rcpp::any(Rcpp::is_na(k))) {
    stop("Function doesn't accept NA values in k vector");
  }

  if (idx.size() != n and idx.size() > 1) {
    stop("length of idx and length of x differs. length(idx) should be 1 or equal to x");
  } else if (Rcpp::any(Rcpp::is_na(idx))) {
    stop("Function doesn't accept NA values in idx vector");
  }

  if ((idx.size() == 1) & (k.size() == 1)) {
    switch (TYPEOF(x)) {
    case INTSXP:  return lag::lag_run11(as<IntegerVector>(x),   k(0));
    case REALSXP: return lag::lag_run11(as<NumericVector>(x),   k(0));
    case STRSXP:  return lag::lag_run11(as<CharacterVector>(x), k(0));
    case LGLSXP:  return  lag::lag_run11(as<LogicalVector>(x),  k(0));
    case CPLXSXP: return lag::lag_run11(as<ComplexVector>(x),   k(0));
    default: {
      stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
    }
    }
  } else if ((idx.size() == 1) & (k.size() > 1)) {
    switch (TYPEOF(x)) {
    case INTSXP:  return lag::lag_run12(as<IntegerVector>(x),   k);
    case REALSXP: return lag::lag_run12(as<NumericVector>(x),   k);
    case STRSXP:  return lag::lag_run12(as<CharacterVector>(x), k);
    case LGLSXP:  return  lag::lag_run12(as<LogicalVector>(x),  k);
    case CPLXSXP: return lag::lag_run12(as<ComplexVector>(x),   k);
    default: {
      stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
    }
    }
  } else if ((idx.size() > 1) & (k.size() == 1)) {
    switch (TYPEOF(x)) {
    case INTSXP:  return lag::lag_run21(as<IntegerVector>(x),   k(0), idx, nearest);
    case REALSXP: return lag::lag_run21(as<NumericVector>(x),   k(0), idx, nearest);
    case STRSXP:  return lag::lag_run21(as<CharacterVector>(x), k(0), idx, nearest);
    case LGLSXP:  return  lag::lag_run21(as<LogicalVector>(x),  k(0), idx, nearest);
    case CPLXSXP: return lag::lag_run21(as<ComplexVector>(x),   k(0), idx, nearest);
    default: {
      stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
    }
    }
  } else if ((idx.size() > 1) & (k.size() > 1)) {
    switch (TYPEOF(x)) {
      case INTSXP:  return lag::lag_run22(as<IntegerVector>(x),   k, idx, nearest);
      case REALSXP: return lag::lag_run22(as<NumericVector>(x),   k, idx, nearest);
      case STRSXP:  return lag::lag_run22(as<CharacterVector>(x), k, idx, nearest);
      case LGLSXP:  return lag::lag_run22(as<LogicalVector>(x),   k, idx, nearest);
      case CPLXSXP: return lag::lag_run22(as<ComplexVector>(x),   k, idx, nearest);
      default: {
        stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
      }
    }
  }

  return R_NilValue;
}
