#include <Rcpp.h>
using namespace Rcpp;
#include "checks.h"
#include "utils.h"
#include "aggregations.h"
#include "runner.h"
// [[Rcpp::plugins("cpp11")]]

template <typename otype, int ITYPE>
Rcpp::Vector<Rcpp::traits::r_sexptype_traits<otype>::rtype>
run_(Rcpp::Vector<ITYPE> const& x,
     Rcpp::IntegerVector const& k,
     Rcpp::IntegerVector const& lag,
     Rcpp::IntegerVector const& indexes,
     Function f,
     bool na_pad) {

  const int OTYPE = Rcpp::traits::r_sexptype_traits<otype>::rtype;

  int n = x.size();
  IntegerVector idx;
  Rcpp::Vector<OTYPE> res(n);

  // Simple windows-------
  if (indexes.size() == 0) {

    if (k.size() > 1 && lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        idx = utils::window_ul(i, k(i), lag(i), n, na_pad);
        res(i) = (idx.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, idx, f);
      }
    } else if (k.size() > 1 && lag.size() == 1) {
      for (int i = 0; i < n; i++) {
        idx = utils::window_ul(i, k(i), lag(0), n, na_pad);
        res(i) = (idx.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, idx, f);
      }
    } else if (k(0) == 0 && lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        idx = utils::window_ul(i, n, lag(i), n, na_pad, true);
        res(i) = (idx.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, idx, f);
      }
    } else if (k(0) == 0 && lag.size() == 1) {
      for (int i = 0; i < n; i++) {
        idx = utils::window_ul(i, n, lag(0), n, na_pad, true);
        res(i) = (idx.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, idx, f);
      }
    } else if (k.size() == 1 && k(0) == n && lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        idx = utils::window_ul(i, n, lag(i), n, na_pad);
        res(i) = (idx.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, idx, f);
      }
    } else if (k.size() == 1 && k(0) == n && lag.size() == 1) {
      for (int i = 0; i < n; i++) {
        idx = utils::window_ul(i, n, lag(0), n, na_pad);
        res(i) = (idx.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, idx, f);
      }
    } else if (k.size() == 1 && lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        idx = utils::window_ul(i, k(0), lag(i), n, na_pad);
        res(i) = (idx.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, idx, f);
      }
    } else if (k.size() == 1 && lag.size() == 1) {
      for (int i = 0; i < n; i++) {
        idx = utils::window_ul(i, k(0), lag(0), n, na_pad);
        res(i) = (idx.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, idx, f);
      }
    }

  } else {
    if (k.size() > 1) {
      if (lag.size() > 1) {
        for (int i = 0; i < n; i++) {
          idx = utils::window_ul_dl(indexes, i, k(i), lag(i), n, na_pad, false);
          res(i) = (idx.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, idx, f);
        }
      } else if (lag(0) != 0){
        for (int i = 0; i < n; i++) {
          idx = utils::window_ul_dl(indexes, i, k(i), lag(0), n, na_pad, false);
          res(i) = (idx.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, idx, f);
        }
      } else {
        for (int i = 0; i < n; i++) {
          idx = utils::window_ul_dl(indexes, i, k(i), 0, n, na_pad, false);
          res(i) = (idx.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, idx, f);
        }
      }
    } else if (k(0) == 0) {
      if (lag.size() > 1) {
        for (int i = 0; i < n; i++) {
          idx = utils::window_ul_dl(indexes, i, k(0), lag(i), n, na_pad, true);
          res(i) = (idx.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, idx, f);
        }
      } else if (lag(0) != 0){
        for (int i = 0; i < n; i++) {
          idx = utils::window_ul_dl(indexes, i, k(0), lag(0), n, na_pad, true);
          res(i) = (idx.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, idx, f);
        }
      } else {
        for (int i = 0; i < n; i++) {
          idx = utils::window_ul_dl(indexes, i, k(0), 0, n, na_pad, true);
          res(i) = (idx.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, idx, f);
        }
      }
    } else {
      if (lag.size() > 1) {
        for (int i = 0; i < n; i++) {
          idx = utils::window_ul_dl(indexes, i, k(0), lag(i), n, na_pad, false);
          res(i) = (idx.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, idx, f);

        }
      } else if (lag(0) != 0) {
        for (int i = 0; i < n; i++) {
          idx = utils::window_ul_dl(indexes, i, k(0), lag(0), n, na_pad, false);
          res(i) = (idx.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, idx, f);
        }
      } else {
        for (int i = 0; i < n; i++) {
          idx = utils::window_ul_dl(indexes, i, k(0), 0, n, na_pad, false);
          res(i) = (idx.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, idx, f);
        }
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
//' @param lag \code{integer} vector or single value denoting window lag.
//' If \code{lag} is a single value then window lag is constant for all elements,
//' otherwise if \code{length(lag) == length(x)} different window size for each
//' element. Negative value shifts window forward.
//' @param idx \code{date or integer} an optional integer vector containing index of observation. If specified
//' then \code{k} and \code{lag} are depending on \code{idx}. Length of \code{idx} should be equal of length \code{x}
//' @param f \code{function} to be applied on \code{x}
//' @param na_pad \code{logical} single value (default \code{na_pad=FALSE}) - if \code{TRUE} calculation on
//' incomplete window will return \code{NA}. Incomplete window is when some parts of the window are out of range
//' @param type output type ("logical", "numeric", "integer", "character")
//' @examples
//' runner(1:10, f = mean, k = 3)
//' runner(1:10, k = 3, f = function(x) mean(x, na.rm = TRUE))
//' runner(letters[1:10],
//'        k = c(1, 2, 2, 4, 5, 5, 5, 5, 5, 5),
//'        f = function(x) length(unique(x)))
//'
//' runner(letters[1:10],
//'        k = c(1, 2, 2, 4, 5, 5, 5, 5, 5, 5),
//'        f = function(x) paste(x, collapse = "-"),
//'        type = "character")
//' @export
// [[Rcpp::export]]
SEXP runner(const SEXP x,
            const Function f,
            const IntegerVector k = IntegerVector(1),
            const IntegerVector lag = IntegerVector(1),
            const IntegerVector idx = IntegerVector(0),
            bool na_pad = false,
            std::string type = "numeric") {

  int n = Rf_length(x);

  checks::check_k(k, n);
  checks::check_idx(idx, n);
  checks::check_lag(lag, n);

  if (type == "logical") {
    switch (TYPEOF(x)) {
    case INTSXP:  return run_<bool>(as<IntegerVector>(x), k, lag, idx, f, na_pad);
    case REALSXP: return run_<bool>(as<NumericVector>(x), k, lag, idx, f, na_pad);
    case STRSXP:  return run_<bool>(as<StringVector>(x), k, lag, idx, f, na_pad);
    case LGLSXP: return run_<bool>(as<LogicalVector>(x), k, lag, idx, f, na_pad);
    case CPLXSXP: return run_<bool>(as<ComplexVector>(x), k, lag, idx, f, na_pad);
    default: {
      stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
    }
    }
  } else if (type == "integer") {
    switch (TYPEOF(x)) {
    case INTSXP:  return run_<int>(as<IntegerVector>(x), k, lag, idx, f, na_pad);
    case REALSXP: return run_<int>(as<NumericVector>(x), k, lag, idx, f, na_pad);
    case STRSXP:  return run_<int>(as<StringVector>(x), k, lag, idx, f, na_pad);
    case LGLSXP: return run_<int>(as<LogicalVector>(x), k, lag, idx, f, na_pad);
    case CPLXSXP: return run_<int>(as<ComplexVector>(x), k, lag, idx, f, na_pad);
    default: {
      stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
    }
    }
  } else if (type == "numeric") {
    switch (TYPEOF(x)) {
    case INTSXP:  return run_<double>(as<IntegerVector>(x), k, lag, idx, f, na_pad);
    case REALSXP: return run_<double>(as<NumericVector>(x), k, lag, idx, f, na_pad);
    case STRSXP:  return run_<double>(as<StringVector>(x), k, lag, idx, f, na_pad);
    case LGLSXP: return run_<double>(as<LogicalVector>(x), k, lag, idx, f, na_pad);
    case CPLXSXP: return run_<double>(as<ComplexVector>(x), k, lag, idx, f, na_pad);
    default: {
      stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
    }
    }
  } else if (type == "character") {
    switch (TYPEOF(x)) {
    case INTSXP:  return run_<Rcpp::String>(as<IntegerVector>(x), k, lag, idx, f, na_pad);
    case REALSXP: return run_<Rcpp::String>(as<NumericVector>(x), k, lag, idx, f, na_pad);
    case STRSXP:  return run_<Rcpp::String>(as<StringVector>(x), k, lag, idx, f, na_pad);
    case LGLSXP: return run_<Rcpp::String>(as<LogicalVector>(x), k, lag, idx, f, na_pad);
    case CPLXSXP: return run_<Rcpp::String>(as<ComplexVector>(x), k, lag, idx, f, na_pad);
    default: {
      stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
    }
    }
  }

  return R_NilValue;
}


template <typename T>
using RcppVec = Rcpp::Vector<Rcpp::traits::r_sexptype_traits<T>::rtype>;

template <int otype, int ITYPE, typename ftype>
Rcpp::Vector<otype>
runner_cpp(Rcpp::Vector<ITYPE> const& x,
           ftype fun,
           Rcpp::IntegerVector const& k = Rcpp::IntegerVector(1),
           Rcpp::IntegerVector const& lag = Rcpp::IntegerVector(1),
           bool const& na_rm = true,
           bool const& na_pad = false,
           Rcpp::IntegerVector const& idx = Rcpp::IntegerVector(0)) {

  int n = x.size();

  Rcpp::Vector<otype> res(n);
  IntegerVector b(2);

  // Simple windows-------
  if (idx.size() == 0) {

    if (k.size() > 1 && lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        b = utils::window_ul(i, k(i), lag(i), n, na_pad);
        if (b.size() == 0) {
          res(i) = Rcpp::Vector<otype>::get_na();
        } else {
          res(i) = fun(x, b(1), b(0), na_rm);
        }
      }
    } else if (k.size() > 1 && lag.size() == 1) {
      for (int i = 0; i < n; i++) {
        b = utils::window_ul(i, k(i), lag(0), n, na_pad);
        if (b.size() == 0) {
          res(i) = Rcpp::Vector<otype>::get_na();
        } else {
          res(i) = fun(x, b(1), b(0), na_rm);
        }
      }
    } else if (k(0) == 0 && lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        b = utils::window_ul(i, n, lag(i), n, na_pad, true);
        if (b.size() == 0) {
          res(i) = Rcpp::Vector<otype>::get_na();
        } else {
          res(i) = fun(x, b(1), b(0), na_rm);
        }
      }
    } else if (k(0) == 0 && lag.size() == 1) {
      for (int i = 0; i < n; i++) {
        b = utils::window_ul(i, n, lag(0), n, na_pad, true);
        if (b.size() == 0) {
          res(i) = Rcpp::Vector<otype>::get_na();
        } else {
          res(i) = fun(x, b(1), b(0), na_rm);
        }
      }
    } else if (k.size() == 1 && k(0) == n && lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        b = utils::window_ul(i, n, lag(i), n, na_pad);
        if (b.size() == 0) {
          res(i) = Rcpp::Vector<otype>::get_na();
        } else {
          res(i) = fun(x, b(1), b(0), na_rm);
        }
      }
    } else if (k.size() == 1 && k(0) == n && lag.size() == 1) {
      for (int i = 0; i < n; i++) {
        b = utils::window_ul(i, n, lag(0), n, na_pad);
        if (b.size() == 0) {
          res(i) = Rcpp::Vector<otype>::get_na();
        } else {
          res(i) = fun(x, b(1), b(0), na_rm);
        }
      }
    } else if (k.size() == 1 && lag.size() > 1) {
      for (int i = 0; i < n; i++) {
        b = utils::window_ul(i, k(0), lag(i), n, na_pad);
        if (b.size() == 0) {
          res(i) = Rcpp::Vector<otype>::get_na();
        } else {
          res(i) = fun(x, b(1), b(0), na_rm);
        }
      }
    } else if (k.size() == 1 && lag.size() == 1) {
      for (int i = 0; i < n; i++) {
        b = utils::window_ul(i, k(0), lag(0), n, na_pad);
        if (b.size() == 0) {
          res(i) = Rcpp::Vector<otype>::get_na();
        } else {
          res(i) = fun(x, b(1), b(0), na_rm);
        }
      }
    }

  } else {
    if (k.size() > 1) {
      if (lag.size() > 1) {
        for (int i = 0; i < n; i++) {
          b = utils::window_ul_dl(idx, i, k(i), lag(i), n, na_pad, false);
          if (b.size() == 0) {
            res(i) = Rcpp::Vector<otype>::get_na();
          } else {
            res(i) = fun(x, b(1), b(0), na_rm);
          }
        }
      } else if (lag(0) != 0){
        for (int i = 0; i < n; i++) {
          b = utils::window_ul_dl(idx, i, k(i), lag(0), n, na_pad, false);
          if (b.size() == 0) {
            res(i) = Rcpp::Vector<otype>::get_na();
          } else {
            res(i) = fun(x, b(1), b(0), na_rm);
          }
        }
      } else {
        for (int i = 0; i < n; i++) {
          b = utils::window_ul_dl(idx, i, k(i), 0, n, na_pad, false);
          if (b.size() == 0) {
            res(i) = Rcpp::Vector<otype>::get_na();
          } else {
            res(i) = fun(x, b(1), b(0), na_rm);
          }
        }
      }
    } else if (k(0) == 0) {
      if (lag.size() > 1) {
        for (int i = 0; i < n; i++) {
          b = utils::window_ul_dl(idx, i, k(0), lag(i), n, na_pad, true);
          if (b.size() == 0) {
            res(i) = Rcpp::Vector<otype>::get_na();
          } else {
            res(i) = fun(x, b(1), b(0), na_rm);
          }
        }
      } else if (lag(0) != 0){
        for (int i = 0; i < n; i++) {
          b = utils::window_ul_dl(idx, i, k(0), lag(0), n, na_pad, true);
          if (b.size() == 0) {
            res(i) = Rcpp::Vector<otype>::get_na();
          } else {
            res(i) = fun(x, b(1), b(0), na_rm);
          }
        }
      } else {
        for (int i = 0; i < n; i++) {
          b = utils::window_ul_dl(idx, i, k(0), 0, n, na_pad, true);
          if (b.size() == 0) {
            res(i) = Rcpp::Vector<otype>::get_na();
          } else {
            res(i) = fun(x, b(1), b(0), na_rm);
          }
        }
      }
    } else {
      if (lag.size() > 1) {
        for (int i = 0; i < n; i++) {
          b = utils::window_ul_dl(idx, i, k(0), lag(i), n, na_pad, false);
          if (b.size() == 0) {
            res(i) = Rcpp::Vector<otype>::get_na();
          } else {
            res(i) = fun(x, b(1), b(0), na_rm);
          }
        }
      } else if (lag(0) != 0) {
        for (int i = 0; i < n; i++) {
          b = utils::window_ul_dl(idx, i, k(0), lag(0), n, na_pad, false);
          if (b.size() == 0) {
            res(i) = Rcpp::Vector<otype>::get_na();
          } else {
            res(i) = fun(x, b(1), b(0), na_rm);
          }
        }
      } else {
        for (int i = 0; i < n; i++) {
          b = utils::window_ul_dl(idx, i, k(0), 0, n, na_pad, false);
          if (b.size() == 0) {
            res(i) = Rcpp::Vector<otype>::get_na();
          } else {
            res(i) = fun(x, b(1), b(0), na_rm);
          }
        }
      }
    }
  }

  return res;

}


//' Running sum
//'
//' Running sum in specified window of numeric vector.
//' @inheritParams runner
//' @param x \code{numeric} vector which running function is calculated on
//' @param na_rm \code{logical} single value (default \code{na_rm = TRUE}) -
//' if \code{TRUE} sum is calculating excluding \code{NA}.
//' @inheritParams runner
//' @return sum \code{code} vector of length equals length of \code{x}.
//' @examples
//' set.seed(11)
//' x1 <- rnorm(15)
//' x2 <- sample(c(rep(NA, 5),rnorm(15)), 15, replace = TRUE)
//' k <- sample(1:15, 15, replace = TRUE)
//' sum_run(x1)
//' sum_run(x2, na_rm = TRUE)
//' sum_run(x2, na_rm = FALSE)
//' sum_run(x2, na_rm = TRUE, k = 4)
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector sum_run(
    NumericVector x,
    IntegerVector k = IntegerVector(1),
    IntegerVector lag = IntegerVector(1),
    bool na_rm = true,
    bool na_pad = false,
    IntegerVector idx = IntegerVector(0)) {

  int n = x.size();
  checks::check_k(k, n);
  checks::check_idx(idx, n);
  checks::check_lag(lag, n);

  if ((k.size() == 1) &&
      (lag.size() == 1) &&
      ((k(0) == 0) && lag(0) == 0)) {
    return aggr::cumsum(x, na_rm);
  } else {
    return runner_cpp<14>(x, aggr::calc_sum, k, lag, na_rm, na_pad, idx);
  }

}

//' Running mean
//'
//' Running mean in specified window of numeric vector.
//' @inheritParams sum_run
//' @inheritParams runner
//' @return mean {numeric} vector of length equals length of \code{x}.
//' @examples
//' set.seed(11)
//' x1 <- rnorm(15)
//' x2 <- sample(c(rep(NA,5), rnorm(15)), 15, replace = TRUE)
//' k <- sample(1:15, 15, replace = TRUE)
//' mean_run(x1)
//' mean_run(x2, na_rm = TRUE)
//' mean_run(x2, na_rm = FALSE )
//' mean_run(x2, na_rm = TRUE, k=4)
//' @export
// [[Rcpp::export]]
NumericVector mean_run(
    NumericVector x,
    IntegerVector k = IntegerVector(1),
    IntegerVector lag = IntegerVector(1),
    bool na_rm = true,
    bool na_pad = false,
    IntegerVector idx = IntegerVector(0)) {


  int n = x.size();
  checks::check_k(k, n);
  checks::check_idx(idx, n);
  checks::check_lag(lag, n);

  if ((k.size() == 1) &&
      (lag.size() == 1) &&
      ((k(0) == 0) && lag(0) == 0)) {
    return aggr::cummean(x, na_rm);
  } else {
    return runner_cpp<14>(x, aggr::calc_mean, k, lag, na_rm, na_pad, idx);
  }
}


//' Running maximum
//'
//'
//' \code{min_run} calculates running max on given \code{x} numeric vector,
//' specified \code{k} window size.
//' @inheritParams runner
//' @inheritParams sum_run
//' @return max {numeric} vector of length equals length of \code{x}.
//' @examples
//' set.seed(11)
//' x1 <- sample( c(1,2,3), 15, replace=TRUE)
//' x2 <- sample( c(NA,1,2,3), 15, replace=TRUE)
//' k  <- sample( 1:4, 15, replace=TRUE)
//' max_run(x1) # simple cumulative maximum
//' max_run(x2, na_rm = TRUE) # cumulative maximum with removing NA.
//' max_run(x2, na_rm = TRUE, k=4) # maximum in 4-element window
//' max_run(x2, na_rm = FALSE, k=k) # maximum in varying k window size
//' @export
// [[Rcpp::export]]
NumericVector max_run(
    NumericVector x,
    IntegerVector k = IntegerVector(1),
    IntegerVector lag = IntegerVector(1),
    bool na_rm = true,
    bool na_pad = false,
    IntegerVector idx = IntegerVector(0)) {


  int n = x.size();
  checks::check_k(k, n);
  checks::check_idx(idx, n);
  checks::check_lag(lag, n);

  if ((k.size() == 1) &&
      (lag.size() == 1) &&
      ((k(0) == 0) && lag(0) == 0)) {
    return aggr::cummax(x, na_rm);
  } else {
    return runner_cpp<14>(x, aggr::calc_max, k, lag, na_rm, na_pad, idx);
  }
}

//' Running minimum
//'
//'
//' \code{min_run} calculates running min on given \code{x} numeric vector, specified \code{k} window size.
//' @inheritParams runner
//' @inheritParams sum_run
//' @return min {numeric} vector of length equals length of \code{x}.
//' @examples
//' set.seed(11)
//' x1 <- sample(c(1, 2, 3), 15, replace = TRUE)
//' x2 <- sample(c(NA, 1, 2, 3), 15, replace = TRUE)
//' k  <- sample(1:4, 15, replace = TRUE)
//' min_run(x1)
//' min_run(x2, na_rm = TRUE)
//' min_run(x2, na_rm = TRUE, k = 4)
//' min_run(x2, na_rm = FALSE, k = k)
//' @export
// [[Rcpp::export]]
NumericVector min_run(
    NumericVector x,
    IntegerVector k = IntegerVector(1),
    IntegerVector lag = IntegerVector(1),
    bool na_rm = true,
    bool na_pad = false,
    IntegerVector idx = IntegerVector(0)) {

  int n = x.size();
  checks::check_k(k, n);
  checks::check_idx(idx, n);
  checks::check_lag(lag, n);

  if ((k.size() == 1) &&
      (lag.size() == 1) &&
      ((k(0) == 0) && lag(0) == 0)) {
    return aggr::cummin(x, na_rm);
  } else {
    return runner_cpp<14>(x, aggr::calc_min, k, lag, na_rm, na_pad, idx);
  }
}

//' Running streak length
//'
//' Calculates running series of consecutive elements
//' @param x {any type} vector which running function is calculated on
//' @inheritParams runner
//' @inheritParams sum_run
//' @return streak [numeric] vector of length equals length of \code{x} containing
//' number of consecutive occurrences.
//' @examples
//' set.seed(11)
//' x1 <- sample(c("a","b"), 15, replace = TRUE)
//' x2 <- sample(c(NA_character_, "a", "b"), 15, replace = TRUE)
//' k <- sample(1:4, 15, replace = TRUE)
//' streak_run(x1) # simple streak run
//' streak_run(x1, k = 2) # streak run within 2-element window
//' streak_run(x2, na_pad = TRUE, k = 3) # streak run within k=3 with padding NA
//' streak_run(x1, k = k) # streak run within varying window size specified by vector k
//' @export
// [[Rcpp::export]]
IntegerVector streak_run(
    SEXP x,
    Rcpp::IntegerVector k = IntegerVector(1),
    Rcpp::IntegerVector lag = IntegerVector(1),
    bool na_rm = true,
    bool na_pad = false,
    Rcpp::IntegerVector idx = IntegerVector(0)) {

  int n = Rf_length(x);

  checks::check_k(k, n);
  checks::check_idx(idx, n);
  checks::check_lag(lag, n);

  if ((k.size() == 1) && (lag.size() == 1) && (k(0) == 0) &&  (lag(0) == 0)) {
    switch (TYPEOF(x)) {
      case INTSXP: return aggr::cumstreak(as<IntegerVector>(x), lag(0), na_rm);
      case REALSXP: return aggr::cumstreak(as<NumericVector>(x), lag(0), na_rm);
      case STRSXP: return aggr::cumstreak(as<StringVector>(x), lag(0), na_rm);
      case LGLSXP: return aggr::cumstreak(as<LogicalVector>(x), lag(0), na_rm);
      default: {
          stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
      }
    }
  } else {
    switch (TYPEOF(x)) {
    case INTSXP: return runner_cpp<13>(as<IntegerVector>(x),
                                       aggr::calc_streak_i,
                                       k, lag, na_rm, na_pad, idx);
    case REALSXP: return runner_cpp<13>(as<NumericVector>(x),
                                        aggr::calc_streak_n,
                                        k, lag, na_rm, na_pad, idx);
    case STRSXP: return runner_cpp<13>(as<StringVector>(x),
                                       aggr::calc_streak_s,
                                       k, lag, na_rm, na_pad, idx);
    case LGLSXP: return runner_cpp<13>(as<LogicalVector>(x),
                                       aggr::calc_streak_l,
                                       k, lag, na_rm, na_pad, idx);
    default: {
      stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
    }
    }
  }

  return R_NilValue;
}

//' Running which
//'
//'
//' \code{min_run} calculates running which - returns index of element where \code{x == TRUE}.
//' @inheritParams runner
//' @inheritParams sum_run
//' @param which \code{character} value "first" or "last" denoting if the first or last \code{TRUE}
//' index is returned from the window.
//' @return integer vector of indexes of the same length as \code{x}.
//' @examples
//' set.seed(11)
//' x1 <- sample(c(1, 2, 3), 15, replace = TRUE)
//' x2 <- sample(c(NA, 1, 2, 3), 15, replace = TRUE)
//' k  <- sample(1:4, 15, replace = TRUE)
//' which_run(x1)
//' which_run(x2, na_rm = TRUE)
//' which_run(x2, na_rm = TRUE, k = 4)
//' which_run(x2, na_rm = FALSE, k = k)
//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector which_run(
    LogicalVector x,
    IntegerVector k = IntegerVector(1),
    IntegerVector lag = IntegerVector(1),
    std::string which = "last",
    bool na_rm = true,
    bool na_pad = false,
    IntegerVector idx = IntegerVector(0)) {

  int n = x.size();

  checks::check_k(k, n);
  checks::check_idx(idx, n);
  checks::check_lag(lag, n);

  if (which != "last" && which != "first") {
    stop("which value should be either 'first' or 'last'");
  }

  IntegerVector b(2);
  IntegerVector res(n);

  if (which == "first") {
    if ((k.size() == 1) &&
        (lag.size() == 1) &&
        ((k(0) == 0) && lag(0) == 0)) {
      return aggr::cumwhichf(x, na_rm);
    } else {
      return runner_cpp<13>(x, aggr::calc_whichf, k, lag, na_rm, na_pad, idx);
    }

  } else {
    if ((k.size() == 1) &&
        (lag.size() == 1) &&
        ((k(0) == 0) && lag(0) == 0)) {
      return aggr::cumwhichl(x, na_rm);
    } else {
      return runner_cpp<13>(x, aggr::calc_whichl, k, lag, na_rm, na_pad, idx);
    }
  }

  return res;
}
