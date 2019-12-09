#include <Rcpp.h>
using namespace Rcpp;
#include "checks.h"
#include "window.h"
// [[Rcpp::plugins(cpp11)]]

template <int RTYPE>
List window_simple(const Vector<RTYPE>& x,
                   const IntegerVector k,
                   const IntegerVector lag,
                   bool na_pad) {
  int n = x.size();
  IntegerVector idx;
  List res(n);

  if (k.size() > 1 && lag.size() > 1) {
    for (int i = 0; i < n; i++) {
      idx = apply::get_window_idx(i, k(i), lag(i), n, na_pad);
      res(i) = (idx.size() == 0) ? Vector<RTYPE>(0) : apply::get_window(x, idx);
    }
  } else if (k.size() > 1 && lag.size() == 1) {
    for (int i = 0; i < n; i++) {
      idx = apply::get_window_idx(i, k(i), lag(0), n, na_pad);
      res(i) = (idx.size() == 0) ? Vector<RTYPE>(0) : apply::get_window(x, idx);
    }
  } else if (k(0) == 0 && lag.size() > 1) {
    for (int i = 0; i < n; i++) {
      idx = apply::get_window_idx(i, n, lag(i), n, na_pad, true);
      res(i) = (idx.size() == 0) ? Vector<RTYPE>(0) : apply::get_window(x, idx);
    }
  } else if (k(0) == 0 && lag.size() == 1) {
    for (int i = 0; i < n; i++) {
      idx = apply::get_window_idx(i, n, lag(0), n, na_pad, true);
      res(i) = (idx.size() == 0) ? Vector<RTYPE>(0) : apply::get_window(x, idx);
    }
  } else if (k.size() == 1 && lag.size() > 1) {
    for (int i = 0; i < n; i++) {
      idx = apply::get_window_idx(i, k(0), lag(i), n, na_pad);
      res(i) = (idx.size() == 0) ? Vector<RTYPE>(0) : apply::get_window(x, idx);
    }
  } else if (k.size() == 1 && lag.size() == 1) {
    for (int i = 0; i < n; i++) {
      idx = apply::get_window_idx(i, k(0), lag(0), n, na_pad);
      res(i) = (idx.size() == 0) ? Vector<RTYPE>(0) : apply::get_window(x, idx);
    }
  }
  return res;
}

template <int RTYPE>
List window_on_date(const Vector<RTYPE>& x,
                    const IntegerVector k,
                    const IntegerVector lag,
                    const IntegerVector indexes,
                    bool na_pad) {
  int n = x.size();
  IntegerVector idx;
  List res(n);

  if (k.size() > 1) {
    if (lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(i), lag(i), n, na_pad, false);
        res(i) = apply::get_window(x, idx);
      }
    } else if (lag(0) != 0){
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(i), lag(0), n, na_pad, false);
        res(i) = apply::get_window(x, idx);
      }
    } else {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(i), 0, n, na_pad, false);
        res(i) = apply::get_window(x, idx);
      }
    }
  } else if (k(0) == 0) {
    if (lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, n, lag(i), n, na_pad, true);
        res(i) = apply::get_window(x, idx);
      }
    } else if (lag(0) != 0){
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, n, lag(0), n, na_pad, true);
        res(i) = apply::get_window(x, idx);
      }
    } else {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, n, 0, n, na_pad, true);
        res(i) = apply::get_window(x, idx);
      }
    }
  } else {
    if (lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(0), lag(i), n, na_pad, false);
        res(i) = apply::get_window(x, idx);

      }
    } else if (lag(0) != 0) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(0), lag(0), n, na_pad, false);
        res(i) = apply::get_window(x, idx);
      }
    } else {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(0), 0, n, na_pad, false);
        res(i) = apply::get_window(x, idx);
      }
    }
  }
  return res;
}

//' List of running windows
//'
//' Creates list of windows
//' @inheritParams runner
//' @examples
//' window_run(1:10, k = 3, lag = -1)
//' window_run(letters[1:10], k = c(1, 2, 2, 4, 5, 5, 5, 5, 5, 5))
//' @export
// [[Rcpp::export]]
SEXP window_run(SEXP x,
                IntegerVector k = IntegerVector(1),
                IntegerVector lag = IntegerVector(1),
                IntegerVector idx = IntegerVector(0),
                bool na_pad = false) {
  int n = Rf_length(x);
  checks::check_k(k, n);
  checks::check_idx(idx, n);
  checks::check_lag(lag, n);

  if(idx.size() > 1) {
    switch (TYPEOF(x)) {
      case INTSXP:  return window_on_date(as<IntegerVector>(x),   k, lag, idx, na_pad);
      case REALSXP: return window_on_date(as<NumericVector>(x),   k, lag, idx, na_pad);
      case STRSXP:  return window_on_date(as<CharacterVector>(x), k, lag, idx, na_pad);
      case LGLSXP:  return window_on_date(as<LogicalVector>(x),   k, lag, idx, na_pad);
      case CPLXSXP: return window_on_date(as<ComplexVector>(x),   k, lag, idx, na_pad);
      default: {
        stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
      }
    }
  } else {
    switch (TYPEOF(x)) {
      case INTSXP:  return window_simple(as<IntegerVector>(x),   k, lag, na_pad);
      case REALSXP: return window_simple(as<NumericVector>(x),   k, lag, na_pad);
      case STRSXP:  return window_simple(as<CharacterVector>(x), k, lag, na_pad);
      case LGLSXP:  return window_simple(as<LogicalVector>(x),   k, lag, na_pad);
      case CPLXSXP: return window_simple(as<ComplexVector>(x),   k, lag, na_pad);
      default: {
        stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
      }
    }


  }

  return R_NilValue;
}

