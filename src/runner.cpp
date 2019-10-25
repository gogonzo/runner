#include <Rcpp.h>
using namespace Rcpp;
#include "runner.h"

template <int RTYPE>
NumericVector runner_simple(const Vector<RTYPE>& x, IntegerVector k, IntegerVector lag, Function f, bool na_pad) {
  int n = x.size();
  IntegerVector idx;
  NumericVector res(n);

  if (k.size() > 1 && lag.size() > 1) {
    for (int i = 0; i < n; i++) {
      idx = apply::get_window_idx(i, k(i), lag(i), n, na_pad);
      res(i) = (idx.size() == 0) ? NA_REAL : apply::apply_on_window(x, idx, f);
    }
  } else if (k.size() > 1 && lag.size() == 1) {
    for (int i = 0; i < n; i++) {
      idx = apply::get_window_idx(i, k(i), lag(0), n, na_pad);
      res(i) = (idx.size() == 0) ? NA_REAL : apply::apply_on_window(x, idx, f);
    }
  } else if (k(0) == 0 && lag.size() > 1) {
    for (int i = 0; i < n; i++) {
      idx = apply::get_window_idx(i, n, lag(i), n, na_pad, true);
      res(i) = (idx.size() == 0) ? NA_REAL : apply::apply_on_window(x, idx, f);
    }
  } else if (k(0) == 0 && lag.size() == 1) {
    for (int i = 0; i < n; i++) {
      idx = apply::get_window_idx(i, n, lag(0), n, na_pad, true);
      res(i) = (idx.size() == 0) ? NA_REAL : apply::apply_on_window(x, idx, f);
    }
  } else if (k.size() == 1 && k(0) == n && lag.size() > 1) {
    for (int i = 0; i < n; i++) {
      idx = apply::get_window_idx(i, n, lag(i), n, na_pad);
      res(i) = (idx.size() == 0) ? NA_REAL : apply::apply_on_window(x, idx, f);
    }
  } else if (k.size() == 1 && k(0) == n && lag.size() == 1) {
    for (int i = 0; i < n; i++) {
      idx = apply::get_window_idx(i, n, lag(0), n, na_pad);
      res(i) = (idx.size() == 0) ? NA_REAL : apply::apply_on_window(x, idx, f);
    }
  } else if (k.size() == 1 && lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_window_idx(i, k(0), lag(i), n, na_pad);
        res(i) = (idx.size() == 0) ? NA_REAL : apply::apply_on_window(x, idx, f);
      }
  } else if (k.size() == 1 && lag.size() == 1) {
    for (int i = 0; i < n; i++) {
      idx = apply::get_window_idx(i, k(0), lag(0), n, na_pad);
      res(i) = (idx.size() == 0) ? NA_REAL : apply::apply_on_window(x, idx, f);
    }
  }

  return res;
}

template <int RTYPE>
NumericVector runner_on_date(const Vector<RTYPE>& x, IntegerVector k, IntegerVector lag, IntegerVector indexes, Function f, bool na_pad) {
  int n = x.size();
  IntegerVector idx;
  NumericVector res(n);


  if (k.size() > 1) {
    if (lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(i), lag(i), n, na_pad, false);
        res(i) = (idx.size() == 0) ? NA_REAL : apply::apply_on_window(x, idx, f);
      }
    } else if (lag(0) != 0){
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(i), lag(0), n, na_pad, false);
        res(i) = (idx.size() == 0) ? NA_REAL : apply::apply_on_window(x, idx, f);
      }
    } else {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(i), 0, n, na_pad, false);
        res(i) = (idx.size() == 0) ? NA_REAL : apply::apply_on_window(x, idx, f);
      }
    }
  } else if (k(0) == 0) {
    if (lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(0), lag(i), n, na_pad, true);
        res(i) = (idx.size() == 0) ? NA_REAL : apply::apply_on_window(x, idx, f);
      }
    } else if (lag(0) != 0){
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(0), lag(0), n, na_pad, true);
        res(i) = (idx.size() == 0) ? NA_REAL : apply::apply_on_window(x, idx, f);
      }
    } else {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(0), 0, n, na_pad, true);
        res(i) = (idx.size() == 0) ? NA_REAL : apply::apply_on_window(x, idx, f);
      }
    }
  } else {
    if (lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(0), lag(i), n, na_pad, false);
        res(i) = (idx.size() == 0) ? NA_REAL : apply::apply_on_window(x, idx, f);

      }
    } else if (lag(0) != 0) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(0), lag(0), n, na_pad, false);
        res(i) = (idx.size() == 0) ? NA_REAL : apply::apply_on_window(x, idx, f);
      }
    } else {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(0), 0, n, na_pad, false);
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
//' @param k \code{integer} vector or single value denoting size of the running window. If \code{k} is a single
//' value then window size is constant for all elements, otherwise if \code{length(k) == length(x)} different
//' window size for each element.
//' @param lag \code{integer} vector or single value denoting window lag. If \code{lag} is a single
//' value then window lag is constant for all elements, otherwise if \code{length(lag) == length(x)} different
//' window size for each element.
//' @param idx \code{date or integer} an optional integer vector containing index of observation. If specified
//' then \code{k} and \code{lag} are depending on \code{idx}. Length of \code{idx} should be equal of length \code{x}
//' @param f \code{function} to be applied on \code{x}
//' @examples
//' runner(1:10, f = mean, k = 3)
//' runner(1:10, k = 3, f = function(x) mean(x, na.rm = TRUE))
//' runner(letters[1:10],
//'        k = c(1, 2, 2, 4, 5, 5, 5, 5, 5, 5),
//'        f = function(x) length(unique(x)))
//' @export
// [[Rcpp::export]]
SEXP runner(SEXP x,
            Function f,
            IntegerVector k = IntegerVector(1),
            IntegerVector lag = IntegerVector(1),
            IntegerVector idx = IntegerVector(0),
            bool na_pad = false) {

  int n = Rf_length(x);

  if (k.size() != n and k.size() > 1) {
    stop("length of k and length of x differs. length(k) should be 1 or equal to x");
  } else if (Rcpp::any(Rcpp::is_na(k))) {
    stop("Function doesn't accept NA values in k vector");
  }

  if (idx.size() != n and idx.size() > 1) {
    stop("length of idx and length of x differs. length(idx) should be 1 or equal to x");
  } else if (Rcpp::any(Rcpp::is_na(idx))) {
    stop("Function doesn't accept NA values in idx vector");
  }

  if (lag.size() != n and lag.size() > 1) {
    stop("length of lag and length of x differs. length(lag) should be 1 or equal to x");
  } else if (Rcpp::any(Rcpp::is_na(lag))) {
    stop("Function doesn't accept NA values in lag vector");
  }

  if (idx.size() > 0) {
    switch (TYPEOF(x)) {
      case INTSXP:  return runner_on_date(as<IntegerVector>(x),   k, lag, idx, f, na_pad);
      case REALSXP: return runner_on_date(as<NumericVector>(x),   k, lag, idx, f, na_pad);
      case STRSXP:  return runner_on_date(as<CharacterVector>(x), k, lag, idx, f, na_pad);
      case LGLSXP: return runner_on_date(as<LogicalVector>(x),    k, lag, idx, f, na_pad);
      case CPLXSXP: return runner_on_date(as<ComplexVector>(x),   k, lag, idx, f, na_pad);
    default: {
      stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
    }
    }
  } else {
    switch (TYPEOF(x)) {
      case INTSXP:  return runner_simple(as<IntegerVector>(x),   k, lag, f, na_pad);
      case REALSXP: return runner_simple(as<NumericVector>(x),   k, lag, f, na_pad);
      case STRSXP:  return runner_simple(as<CharacterVector>(x), k, lag, f, na_pad);
      case LGLSXP: return runner_simple(as<LogicalVector>(x),    k, lag, f, na_pad);
      case CPLXSXP: return runner_simple(as<ComplexVector>(x),   k, lag, f, na_pad);
    default: {
      stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
    }
    }
  }

  return R_NilValue;
}

template <int RTYPE>
List window_simple(const Vector<RTYPE>& x, IntegerVector k, IntegerVector lag, bool omit_incomplete) {
  int n = x.size();
  IntegerVector idx;
  List res(n);

  if (k.size() > 1 && lag.size() > 1) {
    for (int i = 0; i < n; i++) {
      idx = apply::get_window_idx(i, k(i), lag(i), n, omit_incomplete);
      res(i) = (idx.size() == 0) ? Vector<RTYPE>(0) : apply::get_window(x, idx);
    }
  } else if (k.size() > 1 && lag.size() == 1) {
    for (int i = 0; i < n; i++) {
      idx = apply::get_window_idx(i, k(i), lag(0), n, omit_incomplete);
      res(i) = (idx.size() == 0) ? Vector<RTYPE>(0) : apply::get_window(x, idx);
    }
  } else if (k(0) == 0 && lag.size() > 1) {
    for (int i = 0; i < n; i++) {
      idx = apply::get_window_idx(i, n, lag(i), n, omit_incomplete, true);
      res(i) = (idx.size() == 0) ? Vector<RTYPE>(0) : apply::get_window(x, idx);
    }
  } else if (k(0) == 0 && lag.size() == 1) {
    for (int i = 0; i < n; i++) {
      idx = apply::get_window_idx(i, n, lag(0), n, omit_incomplete, true);
      res(i) = (idx.size() == 0) ? Vector<RTYPE>(0) : apply::get_window(x, idx);
    }
  } else if (k.size() == 1 && lag.size() > 1) {
    for (int i = 0; i < n; i++) {
      idx = apply::get_window_idx(i, k(0), lag(i), n, omit_incomplete);
      res(i) = (idx.size() == 0) ? Vector<RTYPE>(0) : apply::get_window(x, idx);
    }
  } else if (k.size() == 1 && lag.size() == 1) {
    for (int i = 0; i < n; i++) {
      idx = apply::get_window_idx(i, k(0), lag(0), n, omit_incomplete);
      res(i) = (idx.size() == 0) ? Vector<RTYPE>(0) : apply::get_window(x, idx);
    }
  }
  return res;
}

template <int RTYPE>
List window_on_date(const Vector<RTYPE>& x, IntegerVector k, IntegerVector lag, IntegerVector indexes, bool omit_incomplete) {
  int n = x.size();
  IntegerVector idx;
  List res(n);

  if (k.size() > 1) {
    if (lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(i), lag(i), n, omit_incomplete, false);
        res(i) = apply::get_window(x, idx);
      }
    } else if (lag(0) != 0){
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(i), lag(0), n, omit_incomplete, false);
        res(i) = apply::get_window(x, idx);
      }
    } else {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(i), 0, n, omit_incomplete, false);
        res(i) = apply::get_window(x, idx);
      }
    }
  } else if (k(0) == 0) {
    if (lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, n, lag(i), n, omit_incomplete, true);
        res(i) = apply::get_window(x, idx);
      }
    } else if (lag(0) != 0){
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, n, lag(0), n, omit_incomplete, true);
        res(i) = apply::get_window(x, idx);
      }
    } else {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, n, 0, n, omit_incomplete, true);
        res(i) = apply::get_window(x, idx);
      }
    }
  } else {
    if (lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(0), lag(i), n, omit_incomplete, false);
        res(i) = apply::get_window(x, idx);

      }
    } else if (lag(0) != 0) {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(0), lag(0), n, omit_incomplete, false);
        res(i) = apply::get_window(x, idx);
      }
    } else {
      for (int i = 0; i < n; i++) {
        idx = apply::get_dwindow_idx_lag(indexes, i, k(0), 0, n, omit_incomplete, false);
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
//' @param lag integer vector which specifies window shift
//' @param idx an optional integer vector containing index of observations.
//' @examples
//' window_run(k = 3)
//' window_run(letters[1:10], k = c(1, 2, 2, 4, 5, 5, 5, 5, 5, 5))
//' @export
// [[Rcpp::export]]
SEXP window_run(SEXP x,
                IntegerVector k = IntegerVector(1),
                IntegerVector lag = IntegerVector(1),
                IntegerVector idx = IntegerVector(0),
                bool omit_incomplete = false) {
  int n = Rf_length(x);
  if (k.size() != n and k.size() > 1) {
    stop("length of k and length of x differs. length(k) should be 1 or equal to x");
  } else if ( Rcpp::any(Rcpp::is_na(k)) ) {
    stop("Function doesn't accept NA values in k vector");
  }

  if(idx.size() != n and idx.size() > 1) {
    stop("length of idx and length of x differs. length(idx) should be 1 or equal to x");
  } else if( Rcpp::any(Rcpp::is_na(idx)) ){
    stop("Function doesn't accept NA values in idx vector");
  }

  if(lag.size() != n and lag.size() > 1) {
    stop("length of lag and length of x differs. length(lag) should be 1 or equal to x");
  } else if( Rcpp::any(Rcpp::is_na(lag))) {
    stop("Function doesn't accept NA values in lag vector");
  }


  if(idx.size() > 1) {
    switch (TYPEOF(x)) {
      case INTSXP:  return window_on_date(as<IntegerVector>(x),   k, lag, idx, omit_incomplete);
      case REALSXP: return window_on_date(as<NumericVector>(x),   k, lag, idx, omit_incomplete);
      case STRSXP:  return window_on_date(as<CharacterVector>(x), k, lag, idx, omit_incomplete);
      case LGLSXP:  return window_on_date(as<LogicalVector>(x),   k, lag, idx, omit_incomplete);
      case CPLXSXP: return window_on_date(as<ComplexVector>(x),   k, lag, idx, omit_incomplete);
      default: {
        stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
      }
    }
  } else {
    switch (TYPEOF(x)) {
      case INTSXP:  return window_simple(as<IntegerVector>(x),   k, lag, omit_incomplete);
      case REALSXP: return window_simple(as<NumericVector>(x),   k, lag, omit_incomplete);
      case STRSXP:  return window_simple(as<CharacterVector>(x), k, lag, omit_incomplete);
      case LGLSXP:  return window_simple(as<LogicalVector>(x),   k, lag, omit_incomplete);
      case CPLXSXP: return window_simple(as<ComplexVector>(x),   k, lag, omit_incomplete);
      default: {
        stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
      }
    }


  }

  return R_NilValue;
}

