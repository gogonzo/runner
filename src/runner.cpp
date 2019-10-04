#include <Rcpp.h>
using namespace Rcpp;
#include "runner.h"

template <int RTYPE>
NumericVector runner_simple(const Vector<RTYPE>& x, IntegerVector k, IntegerVector lag, Function f) {
  int n = x.size();
  IntegerVector idx;
  NumericVector res(n);

  if (k.size() > 1) {
    if (lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_window_idx(i, k(i), lag(i));
        res(i) = apply::apply_on_window(x, idx, f);
      }
    } else {
      for (int i = 0; i < n; i++) {
        idx = apply::get_window_idx(i, k(i), lag(0));
        res(i) = apply::apply_on_window(x, idx, f);
      }
    }
  } else {
    if (lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_window_idx(i, k(0), lag(i));
        res(i) = apply::apply_on_window(x, idx, f);
      }
    } else {
      for (int i = 0; i < n; i++) {
        idx = apply::get_window_idx(i, k(0), lag(0));
        res(i) = apply::apply_on_window(x, idx, f);
      }
    }
  }
  return res;
}

template <int RTYPE>
NumericVector runner_on_date(const Vector<RTYPE>& x, IntegerVector k, IntegerVector lag, IntegerVector indexes, Function f) {
  int n = x.size();
  IntegerVector idx;
  NumericVector res(n);

  if (k.size() > 1) {
    if (lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(i), lag(i));
        res(i) = (idx.size() == 0) ? NA_REAL : apply::apply_on_window(x, idx, f);
      }
    } else if (lag(0) > 0){
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(i), lag(0));
        res(i) = (idx.size() == 0) ? NA_REAL : apply::apply_on_window(x, idx, f);
      }
    } else {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx(indexes, i, k(i));
        res(i) = (idx.size() == 0) ? NA_REAL : apply::apply_on_window(x, idx, f);
      }
    }
  } else {
    if (lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(0), lag(0));
        res(i) = (idx.size() == 0) ? NA_REAL : apply::apply_on_window(x, idx, f);

      }
    } else if (lag(0) > 0) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(0), lag(0));
        res(i) = (idx.size() == 0) ? NA_REAL : apply::apply_on_window(x, idx, f);
      }
    } else {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx(indexes, i, k(0));
        res(i) = (idx.size() == 0) ? NA_REAL : apply::apply_on_window(x, idx, f);
      }
    }
  }
  return res;
}

//' Custom running function
//'
//' Applies custom function to running windows
//' @param x Vector of any type
//' @param f R function to be applied on `x`
//' @param k integer vector which specifies window length
//' @param idx an optional integer vector containing index of observations.
//' @examples
//' runner(1:10, f = mean, k = 3)
//' runner(1:10, k = 3, f = function(x) mean(x, na.rm = TRUE))
//' runner(letters[1:10], k = c(1,2,2,4,5,5,5,5,5,5), f = function(x) length(unique(x)))
//' @export
// [[Rcpp::export]]
SEXP runner(SEXP x, IntegerVector k = 0, IntegerVector lag = 0, IntegerVector idx = 1, Function f = R_NilValue) {

  int n = Rf_length(x);

  if (k(0) == 0) {
    k(0) = n;
  } else if (k.size() != n and k.size() > 1) {
    stop("length of k and length x differs. k=0 and k=length(x) only allowed");
  } else if ( Rcpp::any(Rcpp::is_na(k))) {
    stop("Function doesn't accept NA values in k vector");
  }

  if (idx.size() > 1) {
    switch (TYPEOF(x)) {
    case INTSXP:  return runner_on_date(as<IntegerVector>(x),   k, lag, idx, f);
    case REALSXP: return runner_on_date(as<NumericVector>(x),   k, lag, idx, f);
    case STRSXP:  return runner_on_date(as<CharacterVector>(x), k, lag, idx, f);
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
    case INTSXP:  return runner_simple(as<IntegerVector>(x),   k, lag, f);
    case REALSXP: return runner_simple(as<NumericVector>(x),   k, lag, f);
    case STRSXP:  return runner_simple(as<CharacterVector>(x), k, lag, f);
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

template <int RTYPE>
List window_simple(const Vector<RTYPE>& x, IntegerVector k, IntegerVector lag) {
  int n = x.size();
  IntegerVector idx;
  List res(n);

  if (k.size() > 1) {
    if (lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_window_idx(i, k(i), lag(i));
        res(i) = apply::get_window(x, idx);
      }
    } else {
      for (int i = 0; i < n; i++) {
        idx = apply::get_window_idx(i, k(i), lag(0));
        res(i) = apply::get_window(x, idx);
      }
    }
  } else {
    if (lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_window_idx(i, k(0), lag(i));
        res(i) = apply::get_window(x, idx);
      }
    } else {
      for (int i = 0; i < n; i++) {
        idx = apply::get_window_idx(i, k(0), lag(0));
        res(i) = apply::get_window(x, idx);
      }
    }
  }
  return res;
}

template <int RTYPE>
List window_on_date(const Vector<RTYPE>& x, IntegerVector k, IntegerVector lag, IntegerVector indexes) {
  int n = x.size();
  IntegerVector idx;
  List res(n);

  if (k.size() > 1) {
    if (lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(i), lag(0));
        res(i) = apply::get_window(x, idx);
      }
    } else if (lag(0) > 0) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(i), lag(0));
        res(i) = apply::get_window(x, idx);
      }
    } else {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(i), 0);
        res(i) = apply::get_window(x, idx);
      }
    }
  } else {
    if (lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(0), lag(0));
        res(i) = apply::get_window(x, idx);
      }
    } else if (lag(0) > 0) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(0), lag(0));
        res(i) = apply::get_window(x, idx);
      }
    } else {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx(indexes, i, k(0));
        res(i) = apply::get_window(x, idx);
      }
    }
  }
  return res;
}

//' List of running windows
//'
//' Creates list of windows
//' @param x Vector of any type
//' @param k integer vector which specifies window length
//' @param idx an optional integer vector containing index of observations.
//' @examples
//' window_run(1:10, k=3)
//' window_run(letters[1:10],k=c(1,2,2,4,5,5,5,5,5,5))
//' @export
// [[Rcpp::export]]
SEXP window_run(SEXP x, IntegerVector k = 0, IntegerVector lag = 0, IntegerVector idx = 1) {

  int n = Rf_length(x);

  if( k(0) == 0 ){
    k(0) = n;
  } else if(k.size() != n and k.size() > 1){
    stop("length of k and length x differs. k=0 and k=length(x) only allowed");
  } else if( Rcpp::any(Rcpp::is_na(k)) ){
    stop("Function doesn't accept NA values in k vector");
  }


  if( idx.size() > 1){
    switch (TYPEOF(x)) {
    case INTSXP: return window_on_date(as<IntegerVector>(x), k, lag, idx);
    case REALSXP: return window_on_date(as<NumericVector>(x), k, lag, idx);
    case STRSXP: return window_on_date(as<CharacterVector>(x), k, lag, idx);
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
    case INTSXP: return window_simple(as<IntegerVector>(x), k, lag);
    case REALSXP: return window_simple(as<NumericVector>(x), k, lag);
    case STRSXP: return window_simple(as<CharacterVector>(x), k, lag);
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

