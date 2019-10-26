#include <Rcpp.h>
using namespace Rcpp;
#include "utils.h"
#include "aggregations.h"

//' Running sum
//'
//' Running sum in specified window of numeric vector.
//' @inheritParams runner
//' @param x \code{numeric} vector which running function is calculated on
//' @param na_rm \code{logical} single value (default \code{na_rm = TRUE}) - if \code{TRUE} sum is calulating excluding \code{NA}.
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
NumericVector sum_run(
    NumericVector x,
    IntegerVector k = IntegerVector(1),
    IntegerVector lag = IntegerVector(1),
    bool na_rm = true,
    bool na_pad = false,
    IntegerVector idx = IntegerVector(0)) {

  int n = x.size();

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

  IntegerVector b(2);
  NumericVector res(n);

  /* Simple - no indexes */
  if (idx.size() == 0) {
    /* cum sum */
    if ((k.size() == 1) && (lag.size() == 1) && (lag(0) == 0) && (k(0) == 0)) {
      res = aggr::cumsum(x, na_rm);
    // k.size() > 0
    } else if ((k.size() > 1) && (lag.size() > 1)) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(i), lag(i), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_sum(x, b(1), b(0), na_rm) : NA_REAL;
      }
    } else if ((k.size() > 1) && (lag.size() == 1)) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(i), lag(0), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_sum(x, b(1), b(0), na_rm) : NA_REAL;
      }
    // k(0) == 0
    } else if ((k(0) == 0) && (lag.size() == 1)) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(0), lag(0), n, na_pad, true);
        res(i) = (b.size() == 2) ? aggr::calc_sum(x, b(1), b(0), na_rm) : NA_REAL;
      }
    } else if ((k(0) == 0) && (lag.size() > 1)) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(0), lag(i), n, na_pad, true);
        res(i) = (b.size() == 2) ? aggr::calc_sum(x, b(1), b(0), na_rm) : NA_REAL;
      }
    // k.size() == 1
    } else if (lag.size() > 1) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(0), lag(i), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_sum(x, b(1), b(0), na_rm) : NA_REAL;
      }
    } else if (lag.size() == 1) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(0), lag(0), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_sum(x, b(1), b(0), na_rm) : NA_REAL;
      }
    }

    /* on indexes */
  } else {
    if (k.size() > 1) {
      if (lag.size() > 1) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(i), lag(i), n, na_pad);
          res(i) = (b.size() == 2) ? aggr::calc_sum(x, b(1), b(0), na_rm) : NA_REAL;
        }

      } else if ((lag.size() == 1)) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(i), lag(0), n, na_pad);
          res(i) = (b.size() == 2) ? aggr::calc_sum(x, b(1), b(0), na_rm) : NA_REAL;
        }
      }
    } else if (k(0) == 0) {
      if (lag.size() > 1) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(0), lag(i), n, na_pad, true);
          res(i) = (b.size() == 2) ? aggr::calc_sum(x, b(1), b(0), na_rm) : NA_REAL;
        }
      } else if ((lag.size() == 1)) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(0), lag(0), n, na_pad, true);
          res(i) = (b.size() == 2) ? aggr::calc_sum(x, b(1), b(0), na_rm) : NA_REAL;
        }
      }
    } else {
      if (lag.size() > 1) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(0), lag(i), n, na_pad);
          res(i) = (b.size() == 2) ? aggr::calc_sum(x, b(1), b(0), na_rm) : NA_REAL;
        }
      } else if ((lag.size() == 1)) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(0), lag(0), n, na_pad);
          res(i) = (b.size() == 2) ? aggr::calc_sum(x, b(1), b(0), na_rm) : NA_REAL;
        }
      }
    }
  }

  return res;
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

  if (k.size() != n and k.size() > 1) {
    stop("length of k and length of x differs. length(k) should be 1 or equal to x");
  } else if (Rcpp::any(Rcpp::is_na(k))) {
    stop("Function doesn't accept NA values in k vector");
  }

  if ((idx.size() != n) and (idx.size() > 1)) {
    stop("length of idx and length of x differs. length(idx) should be 1 or equal to x");
  } else if (Rcpp::any(Rcpp::is_na(idx))) {
    stop("Function doesn't accept NA values in idx vector");
  }

  if ((lag.size() != n) and (lag.size() > 1)) {
    stop("length of lag and length of x differs. length(lag) should be 1 or equal to x");
  } else if (Rcpp::any(Rcpp::is_na(lag))) {
    stop("Function doesn't accept NA values in lag vector");
  }

  IntegerVector b(2);
  NumericVector res(n);

  /* Simple - no indexes */
  if (idx.size() == 0) {
    /* cum mean */
    if ((k.size() == 1) && (lag.size() == 1) && (lag(0) == 0) && (k(0) == 0)) {
      res = aggr::cummean(x, na_rm);
      // k.size() > 0
    } else if ((k.size() > 1) && (lag.size() > 1)) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(i), lag(i), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_mean(x, b(1), b(0), na_rm) : NA_REAL;
      }
    } else if ((k.size() > 1) && (lag.size() == 1)) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(i), lag(0), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_mean(x, b(1), b(0), na_rm) : NA_REAL;
      }
      // k(0) == 0
    } else if ((k(0) == 0) && (lag.size() == 1)) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(0), lag(0), n, na_pad, true);
        res(i) = (b.size() == 2) ? aggr::calc_mean(x, b(1), b(0), na_rm) : NA_REAL;
      }
    } else if ((k(0) == 0) && (lag.size() > 1)) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(0), lag(i), n, na_pad, true);
        res(i) = (b.size() == 2) ? aggr::calc_mean(x, b(1), b(0), na_rm) : NA_REAL;
      }
      // k.size() == 1
    } else if (lag.size() > 1) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(0), lag(i), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_mean(x, b(1), b(0), na_rm) : NA_REAL;
      }
    } else if (lag.size() == 1) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(0), lag(0), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_mean(x, b(1), b(0), na_rm) : NA_REAL;
      }
    }

    /* on indexes */
  } else {
    if (k.size() > 1) {
      if (lag.size() > 1) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(i), lag(i), n, na_pad);
          res(i) = (b.size() == 2) ? aggr::calc_mean(x, b(1), b(0), na_rm) : NA_REAL;
        }

      } else if ((lag.size() == 1)) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(i), lag(0), n, na_pad);
          res(i) = (b.size() == 2) ? aggr::calc_mean(x, b(1), b(0), na_rm) : NA_REAL;
        }
      }
    } else if (k(0) == 0) {
      if (lag.size() > 1) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(0), lag(i), n, na_pad, true);
          res(i) = (b.size() == 2) ? aggr::calc_mean(x, b(1), b(0), na_rm) : NA_REAL;
        }
      } else if ((lag.size() == 1)) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(0), lag(0), n, na_pad, true);
          res(i) = (b.size() == 2) ? aggr::calc_mean(x, b(1), b(0), na_rm) : NA_REAL;
        }
      }
    } else {
      if (lag.size() > 1) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(0), lag(i), n, na_pad);
          res(i) = (b.size() == 2) ? aggr::calc_mean(x, b(1), b(0), na_rm) : NA_REAL;
        }
      } else if ((lag.size() == 1)) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(0), lag(0), n, na_pad);
          res(i) = (b.size() == 2) ? aggr::calc_mean(x, b(1), b(0), na_rm) : NA_REAL;
        }
      }
    }
  }

  return res;
}


//' Running maximum
//'
//'
//' \code{min_run} calculates running max on given \code{x} numeric vector, specified \code{k} window size.
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

  if ((k.size() != n) and (k.size() > 1)) {
    stop("length of k and length of x differs. length(k) should be 1 or equal to x");
  } else if (Rcpp::any(Rcpp::is_na(k))) {
    stop("Function doesn't accept NA values in k vector");
  }

  if ((idx.size() != n) and (idx.size() > 1)) {
    stop("length of idx and length of x differs. length(idx) should be 1 or equal to x");
  } else if (Rcpp::any(Rcpp::is_na(idx))) {
    stop("Function doesn't accept NA values in idx vector");
  }

  if ((lag.size() != n) and (lag.size() > 1)) {
    stop("length of lag and length of x differs. length(lag) should be 1 or equal to x");
  } else if (Rcpp::any(Rcpp::is_na(lag))) {
    stop("Function doesn't accept NA values in lag vector");
  }

  IntegerVector b(2);
  NumericVector res(n);

  /* Simple - no indexes */
  if (idx.size() == 0) {
    /* cum max */
    if ((k.size() == 1) && (lag.size() == 1) && (lag(0) == 0) && (k(0) == 0)) {
      res = aggr::cummax(x, na_rm);
      // k.size() > 0
    } else if ((k.size() > 1) && (lag.size() > 1)) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(i), lag(i), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_max(x, b(1), b(0), na_rm) : NA_REAL;
      }
    } else if ((k.size() > 1) && (lag.size() == 1)) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(i), lag(0), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_max(x, b(1), b(0), na_rm) : NA_REAL;
      }
      // k(0) == 0
    } else if ((k(0) == 0) && (lag.size() == 1)) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(0), lag(0), n, na_pad, true);
        res(i) = (b.size() == 2) ? aggr::calc_max(x, b(1), b(0), na_rm) : NA_REAL;
      }
    } else if ((k(0) == 0) && (lag.size() > 1)) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(0), lag(i), n, na_pad, true);
        res(i) = (b.size() == 2) ? aggr::calc_max(x, b(1), b(0), na_rm) : NA_REAL;
      }
      // k.size() == 1
    } else if (lag.size() > 1) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(0), lag(i), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_max(x, b(1), b(0), na_rm) : NA_REAL;
      }
    } else if (lag.size() == 1) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(0), lag(0), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_max(x, b(1), b(0), na_rm) : NA_REAL;
      }
    }

    /* on indexes */
  } else {
    if (k.size() > 1) {
      if (lag.size() > 1) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(i), lag(i), n, na_pad);
          res(i) = (b.size() == 2) ? aggr::calc_max(x, b(1), b(0), na_rm) : NA_REAL;
        }

      } else if ((lag.size() == 1)) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(i), lag(0), n, na_pad);
          res(i) = (b.size() == 2) ? aggr::calc_max(x, b(1), b(0), na_rm) : NA_REAL;
        }
      }
    } else if (k(0) == 0) {
      if (lag.size() > 1) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(0), lag(i), n, na_pad, true);
          res(i) = (b.size() == 2) ? aggr::calc_max(x, b(1), b(0), na_rm) : NA_REAL;
        }
      } else if ((lag.size() == 1)) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(0), lag(0), n, na_pad, true);
          res(i) = (b.size() == 2) ? aggr::calc_max(x, b(1), b(0), na_rm) : NA_REAL;
        }
      }
    } else {
      if (lag.size() > 1) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(0), lag(i), n, na_pad);
          res(i) = (b.size() == 2) ? aggr::calc_max(x, b(1), b(0), na_rm) : NA_REAL;
        }
      } else if ((lag.size() == 1)) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(0), lag(0), n, na_pad);
          res(i) = (b.size() == 2) ? aggr::calc_max(x, b(1), b(0), na_rm) : NA_REAL;
        }
      }
    }
  }

  return res;
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

  if ((k.size() != n) and (k.size() > 1)) {
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

  IntegerVector b(2);
  NumericVector res(n);

  /* Simple - no indexes */
  if (idx.size() == 0) {
    /* cum min */
    if ((k.size() == 1) && (lag.size() == 1) && (lag(0) == 0) && (k(0) == 0)) {
      res = aggr::cummin(x, na_rm);
      // k.size() > 0
    } else if ((k.size() > 1) && (lag.size() > 1)) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(i), lag(i), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_min(x, b(1), b(0), na_rm) : NA_REAL;
      }
    } else if ((k.size() > 1) && (lag.size() == 1)) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(i), lag(0), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_min(x, b(1), b(0), na_rm) : NA_REAL;
      }
      // k(0) == 0
    } else if ((k(0) == 0) && (lag.size() == 1)) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(0), lag(0), n, na_pad, true);
        res(i) = (b.size() == 2) ? aggr::calc_min(x, b(1), b(0), na_rm) : NA_REAL;
      }
    } else if ((k(0) == 0) && (lag.size() > 1)) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(0), lag(i), n, na_pad, true);
        res(i) = (b.size() == 2) ? aggr::calc_min(x, b(1), b(0), na_rm) : NA_REAL;
      }
      // k.size() == 1
    } else if (lag.size() > 1) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(0), lag(i), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_min(x, b(1), b(0), na_rm) : NA_REAL;
      }
    } else if (lag.size() == 1) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(0), lag(0), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_min(x, b(1), b(0), na_rm) : NA_REAL;
      }
    }
    /* on indexes */
  } else {
    if (k.size() > 1) {
      if (lag.size() > 1) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(i), lag(i), n, na_pad);
          res(i) = (b.size() == 2) ? aggr::calc_min(x, b(1), b(0), na_rm) : NA_REAL;
        }

      } else if ((lag.size() == 1)) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(i), lag(0), n, na_pad);
          res(i) = (b.size() == 2) ? aggr::calc_min(x, b(1), b(0), na_rm) : NA_REAL;
        }
      }
    } else if (k(0) == 0) {
      if (lag.size() > 1) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(0), lag(i), n, na_pad, true);
          res(i) = (b.size() == 2) ? aggr::calc_min(x, b(1), b(0), na_rm) : NA_REAL;
        }
      } else if ((lag.size() == 1)) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(0), lag(0), n, na_pad, true);
          res(i) = (b.size() == 2) ? aggr::calc_min(x, b(1), b(0), na_rm) : NA_REAL;
        }
      }
    } else {
      if (lag.size() > 1) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(0), lag(i), n, na_pad);
          res(i) = (b.size() == 2) ? aggr::calc_min(x, b(1), b(0), na_rm) : NA_REAL;
        }
      } else if ((lag.size() == 1)) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(0), lag(0), n, na_pad);
          res(i) = (b.size() == 2) ? aggr::calc_min(x, b(1), b(0), na_rm) : NA_REAL;
        }
      }
    }
  }

  return res;
}

//' Running min/max
//'
//'
//' \code{min_run} calculates running minmax on given \code{x} numeric vector, specified \code{k} window size.
//' @inheritParams runner
//' @inheritParams sum_run
//' @param metric \code{character} what to return, minimum or maximum
//' @return list.
//' @export
// [[Rcpp::export]]
NumericVector minmax_run(
    NumericVector x,
    std::string metric = "min",
    bool na_rm = true) {

  int n = x.size();

  double prev;
  double cur;
  double temp_max = x(0);
  double temp_min = x(0);
  double last_max = x(0);
  double last_min = x(0);

  NumericVector res(n);
  res(0) = x(0);
  NumericVector mins = NumericVector(n);
  NumericVector maxes = NumericVector(n);

  for (int i = 1; i < n; ++i) {
    if (NumericVector::is_na(x(i)) && !na_rm) {
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
IntegerVector which_run(
    LogicalVector x,
    IntegerVector k = IntegerVector(1),
    IntegerVector lag = IntegerVector(1),
    std::string which = "last",
    bool na_rm = true,
    bool na_pad = false,
    IntegerVector idx = IntegerVector(0)) {

  int n = x.size();

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

  if (which != "last" && which != "first") {
    stop("which value should be either 'first' or 'last'");
  }

  IntegerVector b(2);
  IntegerVector res(n);

  /* Simple - no indexes */
  if (idx.size() == 0) {
    /* cum whicht */
    if ((k.size() == 1) && (lag.size() == 1) && (lag(0) == 0) && (k(0) == 0)) {
      res = aggr::cumwhicht(x, na_rm, which);
      // k.size() > 0
    } else if ((k.size() > 1) && (lag.size() > 1)) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(i), lag(i), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_whicht(x, b(1), b(0), na_rm, which) : NA_INTEGER;
      }
    } else if ((k.size() > 1) && (lag.size() == 1)) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(i), lag(0), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_whicht(x, b(1), b(0), na_rm, which) : NA_INTEGER;
      }
      // k(0) == 0
    } else if ((k(0) == 0) && (lag.size() == 1)) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(0), lag(0), n, na_pad, true);
        res(i) = (b.size() == 2) ? aggr::calc_whicht(x, b(1), b(0), na_rm, which) : NA_INTEGER;
      }
    } else if ((k(0) == 0) && (lag.size() > 1)) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(0), lag(i), n, na_pad, true);
        res(i) = (b.size() == 2) ? aggr::calc_whicht(x, b(1), b(0), na_rm, which) : NA_INTEGER;
      }
      // k.size() == 1
    } else if (lag.size() > 1) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(0), lag(i), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_whicht(x, b(1), b(0), na_rm, which) : NA_INTEGER;
      }
    } else if (lag.size() == 1) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul(i, k(0), lag(0), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_whicht(x, b(1), b(0), na_rm, which) : NA_INTEGER;
      }
    }

    /* on indexes */
  } else {
    if (k.size() > 1) {
      if (lag.size() > 1) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(i), lag(i), n, na_pad);
          res(i) = (b.size() == 2) ? aggr::calc_whicht(x, b(1), b(0), na_rm, which) : NA_INTEGER;
        }

      } else if ((lag.size() == 1)) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(i), lag(0), n, na_pad);
          res(i) = (b.size() == 2) ? aggr::calc_whicht(x, b(1), b(0), na_rm, which) : NA_INTEGER;
        }
      }
    } else if (k(0) == 0) {
      if (lag.size() > 1) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(0), lag(i), n, na_pad, true);
          res(i) = (b.size() == 2) ? aggr::calc_whicht(x, b(1), b(0), na_rm, which) : NA_INTEGER;
        }
      } else if ((lag.size() == 1)) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(0), lag(0), n, na_pad, true);
          res(i) = (b.size() == 2) ? aggr::calc_whicht(x, b(1), b(0), na_rm, which) : NA_INTEGER;
        }
      }
    } else {
      if (lag.size() > 1) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(0), lag(i), n, na_pad);
          res(i) = (b.size() == 2) ? aggr::calc_whicht(x, b(1), b(0), na_rm, which) : NA_INTEGER;
        }
      } else if ((lag.size() == 1)) {
        for (int i = 0; i < n; ++i) {
          b = utils::window_ul_dl(idx, i, k(0), lag(0), n, na_pad);
          res(i) = (b.size() == 2) ? aggr::calc_whicht(x, b(1), b(0), na_rm, which) : NA_INTEGER;
        }
      }
    }
  }


  return res;
}

template <int RTYPE>
IntegerVector streak_run1(const Vector<RTYPE>& x, IntegerVector k, IntegerVector lag,  bool na_rm, bool na_pad) {
  int n = x.size();
  int l = 0;
  int cur_streak = 0;
  IntegerVector b(2);
  IntegerVector res(n);

  if ((k.size() == 1) && (lag.size() == 1) && (k(0) == 0) &&  (lag(0) == 0)) {
    for (int i = 0; i < n ; i++) {
      if (Vector<RTYPE>::is_na(x(i))) {
        if (!na_rm) {
          cur_streak = 0;
          if (i + lag(0) >= 0 && i + lag(0) < n) {
            res(i + lag(0)) = NA_INTEGER;
            continue;
          }
        }
      } else if (x(i) == x(l)) {
        cur_streak += 1;
      } else {
        cur_streak = 1;
        l = i;
      }
      if ((i + lag(0) >= 0) && (i + lag(0) < n)) {
        res(i + lag(0)) = cur_streak;
      }
    }

    if (lag(0) > 0) {
      std::fill(res.begin(), res.end() - n + lag(0), NA_INTEGER);
    } else if (lag(0) < 0) {
      std::fill(res.end() + lag(0), res.end(), NA_INTEGER);
    }

  } else if ((k.size() > 1) && (lag.size() > 1)) {
    for (int i = 0; i < n; ++i) {
      b = utils::window_ul(i, k(i), lag(i), n, na_pad);
      res(i) = (b.size() == 2) ? aggr::calc_actual_streak(x, b(1), b(0), na_rm) : NA_INTEGER;
    }
  } else if ((k.size() > 1) && (lag.size() == 1)) {
    for (int i = 0; i < n; ++i) {
      b = utils::window_ul(i, k(i), lag(0), n, na_pad);
      res(i) = (b.size() == 2) ? aggr::calc_actual_streak(x, b(1), b(0), na_rm) : NA_INTEGER;
    }
  } else if ((k(0) == 0) && (lag.size() > 1)) {
    for (int i = 0; i < n; ++i) {
      b = utils::window_ul(i, k(0), lag(i), n, na_pad, true);
      res(i) = (b.size() == 2) ? aggr::calc_actual_streak(x, b(1), b(0), na_rm) : NA_INTEGER;
    }
  } else if ((k(0) == 0) && (lag.size() == 1)) {
    for (int i = 0; i < n; ++i) {
      b = utils::window_ul(i, k(0), lag(0), n, na_pad, true);
      res(i) = (b.size() == 2) ? aggr::calc_actual_streak(x, b(1), b(0), na_rm) : NA_INTEGER;
    }
  } else if ((lag.size() > 1)) {
    for (int i = 0; i < n; ++i) {
      b = utils::window_ul(i, k(0), lag(i), n, na_pad);
      res(i) = (b.size() == 2) ? aggr::calc_actual_streak(x, b(1), b(0), na_rm) : NA_INTEGER;
    }
  } else if ((lag.size() == 1)) {
    for (int i = 0; i < n; ++i) {
      b = utils::window_ul(i, k(0), lag(0), n, na_pad);
      res(i) = (b.size() == 2) ? aggr::calc_actual_streak(x, b(1), b(0), na_rm) : NA_INTEGER;
    }
  }

  return res;
}

template <int RTYPE>
IntegerVector streak_run2(const Vector<RTYPE>& x, IntegerVector k, IntegerVector lag, bool na_rm, bool na_pad, IntegerVector indexes) {
  IntegerVector b;
  int n = x.size();
  IntegerVector res(n);

  // no lag
  if (k.size() > 1) {
    if (lag.size() > 1) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul_dl(indexes, i, k(i), lag(i), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_actual_streak(x, b(1), b(0), na_rm) : NA_INTEGER;
      }
    } else {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul_dl(indexes, i, k(i), lag(0), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_actual_streak(x, b(1), b(0), na_rm) : NA_INTEGER;
      }
    }
  } else if (k(0) == 0) {
    if (lag.size() > 1) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul_dl(indexes, i, k(0), lag(i), n, na_pad, true);
        res(i) = (b.size() == 2) ? aggr::calc_actual_streak(x, b(1), b(0), na_rm) : NA_INTEGER;
      }
    } else {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul_dl(indexes, i, k(0), lag(0), n, na_pad, true);
        res(i) = (b.size() == 2) ? aggr::calc_actual_streak(x, b(1), b(0), na_rm) : NA_INTEGER;
      }
    }
  } else {
    if (lag.size() > 1) {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul_dl(indexes, i, k(0), lag(i), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_actual_streak(x, b(1), b(0), na_rm) : NA_INTEGER;
      }
    } else {
      for (int i = 0; i < n; ++i) {
        b = utils::window_ul_dl(indexes, i, k(0), lag(0), n, na_pad);
        res(i) = (b.size() == 2) ? aggr::calc_actual_streak(x, b(1), b(0), na_rm) : NA_INTEGER;
      }
    }
  }


  return res;
}

//' Running streak length
//'
//' Calculates running series of consecutive elements
//' @param x {any type} vector which running function is calculated on
//' @inheritParams runner
//' @inheritParams sum_run
//' @return streak [numeric] vector of length equals length of \code{x} containing number of consecutive occurences.
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
    IntegerVector k = IntegerVector(1),
    IntegerVector lag = IntegerVector(1),
    bool na_rm = true,
    bool na_pad = false,
    IntegerVector idx = IntegerVector(0)) {

  int n = Rf_length(x);

  if ((k.size() != n) and (k.size() > 1)) {
    stop("length of k and length of x differs. length(k) should be 1 or equal to x");
  } else if (Rcpp::any(Rcpp::is_na(k))) {
    stop("Function doesn't accept NA values in k vector");
  }

  if ((idx.size() != n) and (idx.size() > 1)) {
    stop("length of idx and length of x differs. length(idx) should be 1 or equal to x");
  } else if (Rcpp::any(Rcpp::is_na(idx))) {
    stop("Function doesn't accept NA values in idx vector");
  }

  if ((lag.size() != n) and (lag.size() > 1)) {
    stop("length of lag and length of x differs. length(lag) should be 1 or equal to x");
  } else if (Rcpp::any(Rcpp::is_na(lag))) {
    stop("Function doesn't accept NA values in lag vector");
  }

  if (idx.size() == 0) {
    switch (TYPEOF(x)) {
    case INTSXP: return  streak_run1(as<IntegerVector>(x),   k, lag, na_rm, na_pad);
    case REALSXP: return streak_run1(as<NumericVector>(x),   k, lag, na_rm, na_pad);
    case STRSXP: return  streak_run1(as<CharacterVector>(x), k, lag, na_rm, na_pad);
    case LGLSXP: return  streak_run1(as<LogicalVector>(x),   k, lag, na_rm, na_pad);
    case CPLXSXP: return streak_run1(as<ComplexVector>(x),   k, lag, na_rm, na_pad);
    default: {
      stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
      }
    }
  } else {
    switch (TYPEOF(x)) {
    case INTSXP: return  streak_run2(as<IntegerVector>(x),   k, lag, na_rm, na_pad, idx);
    case REALSXP: return streak_run2(as<NumericVector>(x),   k, lag, na_rm, na_pad, idx);
    case STRSXP: return  streak_run2(as<CharacterVector>(x), k, lag, na_rm, na_pad, idx);
    case LGLSXP: return  streak_run2(as<LogicalVector>(x),   k, lag, na_rm, na_pad, idx);
    case CPLXSXP: return streak_run2(as<ComplexVector>(x),   k, lag, na_rm, na_pad, idx);
    default: {
      stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
    }
    }
  }
}

