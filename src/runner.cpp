#include <Rcpp.h>
using namespace Rcpp;
#include "vector_funs.h"
#include "window_funs.h"
#include "checks.h"
#include "runner.h"
#include "utils.h"
// [[Rcpp::plugins("cpp11")]]

template <typename otype, int ITYPE>
Rcpp::Vector<Rcpp::traits::r_sexptype_traits<otype>::rtype>
run(Rcpp::Vector<ITYPE> const& x,
    Rcpp::IntegerVector const& k,
    Rcpp::IntegerVector const& lag,
    Rcpp::IntegerVector const& idx,
    Rcpp::IntegerVector const& at,
    Function const& f,
    bool na_pad) {

  const int OTYPE = Rcpp::traits::r_sexptype_traits<otype>::rtype;
  int n = x.size();
  int nn = at.size();
  Rcpp::Vector<OTYPE> res(at.size() == 0 ? n : nn);
  IntegerVector b(2);

  if (at.size() == 0) {
    if (idx.size() == 0) {
      if (k.size() > 1 && lag.size() > 1) {

        for (int i = 0; i < n; i++) {
          b = utils::window_ul(i, k(i), lag(i), n, na_pad);
          res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
        }
      } else if (k.size() > 1 && lag.size() == 1) {

        for (int i = 0; i < n; i++) {
          b = utils::window_ul(i, k(i), lag(0), n, na_pad);
          res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
        }
      } else if (k.size() == 0 && lag.size() > 1) {

        for (int i = 0; i < n; i++) {
          b = utils::window_ul(i, n, lag(i), n, na_pad, true);
          res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
        }
      } else if (k.size() == 0 && lag.size() == 1) {

        for (int i = 0; i < n; i++) {
          b = utils::window_ul(i, n, lag(0), n, na_pad, true);
          res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
        }
      } else if (k.size() == 1 && k(0) == n && lag.size() > 1) {

        for (int i = 0; i < n; i++) {
          b = utils::window_ul(i, n, lag(i), n, na_pad);
          res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
        }
      } else if (k.size() == 1 && k(0) == n && lag.size() == 1) {

        for (int i = 0; i < n; i++) {
          b = utils::window_ul(i, n, lag(0), n, na_pad);
          res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
        }
      } else if (k.size() == 1 && lag.size() > 1) {

        for (int i = 0; i < n; i++) {
          b = utils::window_ul(i, k(0), lag(i), n, na_pad);
          res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
        }
      } else if (k.size() == 1 && lag.size() == 1) {

        for (int i = 0; i < n; i++) {
          b = utils::window_ul(i, k(0), lag(0), n, na_pad);
          res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
        }
      }

    } else {
      if (k.size() > 1) {
        if (lag.size() > 1) {

          for (int i = 0; i < n; i++) {
            b = utils::window_ul_dl(idx, i, k(i), lag(i), n, na_pad, false);
            res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
          }
        } else {

          for (int i = 0; i < n; i++) {
            b = utils::window_ul_dl(idx, i, k(i), lag(0), n, na_pad, false);
            res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
          }
        }

      } else if (k.size() == 0) {
        if (lag.size() > 1) {

          for (int i = 0; i < n; i++) {
            b = utils::window_ul_dl(idx, i, n, lag(i), n, na_pad, true);
            res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
          }
        } else {

          for (int i = 0; i < n; i++) {
            b = utils::window_ul_dl(idx, i, n, lag(0), n, na_pad, true);
            res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
          }
        }
      } else if (k.size() == 1) {
        if (lag.size() > 1) {

          for (int i = 0; i < n; i++) {
            b = utils::window_ul_dl(idx, i, k(0), lag(i), n, na_pad, false);
            res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);

          }
        } else {

          for (int i = 0; i < n; i++) {
            b = utils::window_ul_dl(idx, i, k(0), lag(0), n, na_pad, false);
            res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
          }
        }
      }
    }
  } else {
    if (idx.size() == 0) {
      if (k.size() > 1 && lag.size() > 1) {

        for (int i = 0; i < nn; i++) {
          b = utils::window_ul(at(i) - 1, k(i), lag(i), n, na_pad);
          res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
        }
      } else if (k.size() > 1 && lag.size() == 1) {

        for (int i = 0; i < nn; i++) {
          b = utils::window_ul(at(i) - 1, k(i), lag(0), n, na_pad);
          res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
        }
      } else if (k.size() == 0 && lag.size() > 1) {

        for (int i = 0; i < nn; i++) {
          b = utils::window_ul(at(i) - 1, n, lag(i), n, na_pad, true);
          res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
        }
      } else if (k.size() == 0 && lag.size() == 1) {

        for (int i = 0; i < nn; i++) {
          b = utils::window_ul(at(i) - 1, n, lag(0), n, na_pad, true);
          res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
        }
      } else if (k.size() == 1 && lag.size() > 1) {

        for (int i = 0; i < nn; i++) {
          b = utils::window_ul(at(i) - 1, k(0), lag(i), n, na_pad);
          res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
        }
      } else if (k.size() == 1 && lag.size() == 1) {

        for (int i = 0; i < nn; i++) {
          b = utils::window_ul(at(i) - 1, k(0), lag(0), n, na_pad);
          res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
        }
      }
    } else {
      if (k.size() > 1) {
        if (lag.size() > 1) {

          for (int i = 0; i < nn; i++) {
            b = utils::window_ul_at(idx, at(i), k(i), lag(i), n, na_pad, false);
            res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
          }
        } else {

          for (int i = 0; i < nn; i++) {
            b = utils::window_ul_at(idx, at(i), k(i), lag(0), n, na_pad, false);
            res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
          }
        }
      } else if (k.size() == 0) {
        if (lag.size() > 1) {

          for (int i = 0; i < nn; i++) {
            b = utils::window_ul_at(idx, at(i), n, lag(i), n, na_pad, true);
            res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
          }
        } else {

          for (int i = 0; i < nn; i++) {
            b = utils::window_ul_at(idx, at(i), n, lag(0), n, na_pad, true);
            res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
          }
        }
      } else if (k.size() == 1) {
        if (lag.size() > 1) {

          for (int i = 0; i < nn; i++) {
            b = utils::window_ul_at(idx, at(i), k(0), lag(i), n, na_pad, false);
            res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);

          }
        } else {

          for (int i = 0; i < nn; i++) {
            b = utils::window_ul_at(idx, at(i), k(0), lag(0), n, na_pad, false);
            res(i) = (b.size() == 0) ? Rcpp::Vector<OTYPE>::get_na() : apply::apply<otype>(x, b, f);
          }
        }
      }
    }
  }


  return res;
}

template <typename T>
using RcppVec = Rcpp::Vector<Rcpp::traits::r_sexptype_traits<T>::rtype>;

template <int otype, int ITYPE, typename ftype>
Rcpp::Vector<otype>
runner_vec(Rcpp::Vector<ITYPE> const& x,
           ftype fun,
           Rcpp::IntegerVector const& k,
           Rcpp::IntegerVector const& lag,
           Rcpp::IntegerVector const& idx,
           Rcpp::IntegerVector const& at,
           bool na_rm,
           bool na_pad) {

  int n = x.size();
  int nn = at.size() == 0 ? x.size() : at.size();
  std::string var = at.size() == 0 ? "x" : "at";

  checks::check_k(k, nn, var);
  checks::check_lag(lag, nn, var);
  checks::check_idx(idx, n, var);
  checks::check_at(at);

  Rcpp::Vector<otype>  res(nn);
  IntegerVector b(2);

  // Simple windows-------
  if (at.size() == 0) {
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
      } else if (k.size() == 0 && lag.size() > 1) {
        for (int i = 0; i < n; i++) {
          b = utils::window_ul(i, n, lag(i), n, na_pad, true);
          if (b.size() == 0) {
            res(i) = Rcpp::Vector<otype>::get_na();
          } else {
            res(i) = fun(x, b(1), b(0), na_rm);
          }
        }
      } else if (k.size() == 0 && lag.size() == 1) {
        for (int i = 0; i < n; i++) {
          b = utils::window_ul(i, n, lag(0), n, na_pad, true);
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
        } else {
          for (int i = 0; i < n; i++) {
            b = utils::window_ul_dl(idx, i, k(i), lag(0), n, na_pad, false);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<otype>::get_na();
            } else {
              res(i) = fun(x, b(1), b(0), na_rm);
            }
          }
        }
      } else if (k.size() == 0) {
        if (lag.size() > 1) {
          for (int i = 0; i < n; i++) {
            b = utils::window_ul_dl(idx, i, n, lag(i), n, na_pad, true);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<otype>::get_na();
            } else {
              res(i) = fun(x, b(1), b(0), na_rm);
            }
          }
        } else {
          for (int i = 0; i < n; i++) {
            b = utils::window_ul_dl(idx, i, n, lag(0), n, na_pad, true);
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
        } else {
          for (int i = 0; i < n; i++) {
            b = utils::window_ul_dl(idx, i, k(0), lag(0), n, na_pad, false);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<otype>::get_na();
            } else {
              res(i) = fun(x, b(1), b(0), na_rm);
            }
          }
        }
      }
    }
  } else {
    if (idx.size() == 0) {
      if (k.size() > 1 && lag.size() > 1) {
        for (int i = 0; i < nn; i++) {
          b = utils::window_ul(at(i) - 1, k(i), lag(i), n, na_pad);
          if (b.size() == 0) {
            res(i) = Rcpp::Vector<otype>::get_na();
          } else {
            res(i) = fun(x, b(1), b(0), na_rm);
          }
        }
      } else if (k.size() > 1 && lag.size() == 1) {
        for (int i = 0; i < nn; i++) {
          b = utils::window_ul(at(i) - 1, k(i), lag(0), n, na_pad);
          if (b.size() == 0) {
            res(i) = Rcpp::Vector<otype>::get_na();
          } else {
            res(i) = fun(x, b(1), b(0), na_rm);
          }
        }
      } else if (k.size() == 0 && lag.size() > 1) {
        for (int i = 0; i < nn; i++) {
          b = utils::window_ul(at(i) - 1, n, lag(i), n, na_pad, true);
          if (b.size() == 0) {
            res(i) = Rcpp::Vector<otype>::get_na();
          } else {
            res(i) = fun(x, b(1), b(0), na_rm);
          }
        }
      } else if (k.size() == 0 && lag.size() == 1) {
        for (int i = 0; i < nn; i++) {
          b = utils::window_ul(at(i) - 1, n, lag(0), n, na_pad, true);
          if (b.size() == 0) {
            res(i) = Rcpp::Vector<otype>::get_na();
          } else {
            res(i) = fun(x, b(1), b(0), na_rm);
          }
        }
      } else if (k.size() == 1 && lag.size() > 1) {
        for (int i = 0; i < nn; i++) {
          b = utils::window_ul(at(i) - 1, k(0), lag(i), n, na_pad);
          if (b.size() == 0) {
            res(i) = Rcpp::Vector<otype>::get_na();
          } else {
            res(i) = fun(x, b(1), b(0), na_rm);
          }
        }
      } else if (k.size() == 1 && lag.size() == 1) {
        for (int i = 0; i < nn; i++) {
          b = utils::window_ul(at(i) - 1, k(0), lag(0), n, na_pad);
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
          for (int i = 0; i < nn; i++) {
            b = utils::window_ul_at(idx, at(i), k(i), lag(i), n, na_pad, false);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<otype>::get_na();
            } else {
              res(i) = fun(x, b(1), b(0), na_rm);
            }
          }
        } else if (lag(0) != 0){
          for (int i = 0; i < nn; i++) {
            b = utils::window_ul_at(idx, at(i), k(i), lag(0), n, na_pad, false);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<otype>::get_na();
            } else {
              res(i) = fun(x, b(1), b(0), na_rm);
            }
          }
        } else {
          for (int i = 0; i < nn; i++) {
            b = utils::window_ul_at(idx, at(i), k(i), 0, n, na_pad, false);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<otype>::get_na();
            } else {
              res(i) = fun(x, b(1), b(0), na_rm);
            }
          }
        }
      } else if (k.size() == 0) {
        if (lag.size() > 1) {
          for (int i = 0; i < nn; i++) {
            b = utils::window_ul_at(idx, at(i), n, lag(i), n, na_pad, true);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<otype>::get_na();
            } else {
              res(i) = fun(x, b(1), b(0), na_rm);
            }
          }
        } else if (lag(0) != 0){
          for (int i = 0; i < nn; i++) {
            b = utils::window_ul_at(idx, at(i), n, lag(0), n, na_pad, true);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<otype>::get_na();
            } else {
              res(i) = fun(x, b(1), b(0), na_rm);
            }
          }
        } else {
          for (int i = 0; i < nn; i++) {
            b = utils::window_ul_at(idx, at(i), n, 0, n, na_pad, true);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<otype>::get_na();
            } else {
              res(i) = fun(x, b(1), b(0), na_rm);
            }
          }
        }
      } else if (k.size() == 1) {
        if (lag.size() > 1) {
          for (int i = 0; i < nn; i++) {
            b = utils::window_ul_at(idx, at(i), k(0), lag(i), n, na_pad, false);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<otype>::get_na();
            } else {
              res(i) = fun(x, b(1), b(0), na_rm);
            }
          }
        } else if (lag(0) != 0) {
          for (int i = 0; i < nn; i++) {
            b = utils::window_ul_at(idx, at(i), k(0), lag(0), n, na_pad, false);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<otype>::get_na();
            } else {
              res(i) = fun(x, b(1), b(0), na_rm);
            }
          }
        } else {
          for (int i = 0; i < nn; i++) {
            b = utils::window_ul_at(idx, at(i), k(0), 0, n, na_pad, false);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<otype>::get_na();
            } else {
              res(i) = fun(x, b(1), b(0), na_rm);
            }
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
//'
//' @param x \code{numeric} vector which running function is calculated on
//'
//' @param k (\code{integer}` vector or single value)\cr
//'  Denoting size of the running window. If \code{k} is a single value then window
//'  size is constant for all elements, otherwise if \code{length(k) == length(x)}
//'  different window size for each element.
//'
//' @param lag (\code{integer} vector or single value)\cr
//'  Denoting window lag. If \code{lag} is a single value then window lag is constant
//'  for all elements, otherwise if \code{length(lag) == length(x)} different window
//'  size for each element. Negative value shifts window forward.
//'
//' @param idx (\code{integer}, \code{Date}, \code{POSIXt})\cr
//'  Optional integer vector containing sorted (ascending) index of observation.
//'  By default \code{idx} is index incremented by one. User can provide index with
//'  varying increment and with duplicated values. If specified then \code{k} and \code{lag}
//'  are depending on \code{idx}. Length of \code{idx} have to be equal of length \code{x}.
//'
//' @param at (\code{integer}, \code{Date}, \code{POSIXt}, \code{character} vector)\cr
//'  Vector of any size and any value defining output data points. Values of the
//'  vector defines the indexes which data is computed at.
//'
//' @param na_rm \code{logical} single value (default \code{na_rm = TRUE}) -
//' if \code{TRUE} sum is calculating excluding \code{NA}.
//'
//' @inheritParams runner
//'
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
    IntegerVector k = IntegerVector(0),
    IntegerVector lag = IntegerVector(1),
    IntegerVector idx = IntegerVector(0),
    IntegerVector at = IntegerVector(0),
    bool na_rm = true,
    bool na_pad = false) {

  if (k.size() == 0 &&
      lag.size() == 1 &&
      lag(0) == 0 &&
      idx.size() == 0 &&
      at.size() == 0) {
    return aggr::cumsum(x, na_rm);
  } else {
    return runner_vec<14>(x, aggr::calc_sum, k, lag, idx, at, na_rm, na_pad);
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
    IntegerVector k = IntegerVector(0),
    IntegerVector lag = IntegerVector(1),
    IntegerVector idx = IntegerVector(0),
    IntegerVector at = IntegerVector(0),
    bool na_rm = true,
    bool na_pad = false) {

  if (k.size() == 0 &&
      lag.size() == 1 &&
      lag(0) == 0 &&
      idx.size() == 0 &&
      at.size() == 0) {
    return aggr::cummean(x, na_rm);
  } else {
    return runner_vec<14>(x, aggr::calc_mean, k, lag, idx, at, na_rm, na_pad);
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
    Rcpp::NumericVector x,
    Rcpp::IntegerVector k = IntegerVector(0),
    Rcpp::IntegerVector lag = IntegerVector(1),
    Rcpp::IntegerVector idx = IntegerVector(0),
    Rcpp::IntegerVector at = IntegerVector(0),
    bool na_rm = true,
    bool na_pad = false) {

  if (k.size() == 0 &&
      lag.size() == 1 &&
      lag(0) == 0 &&
      idx.size() == 0 &&
      at.size() == 0) {
    return aggr::cummax(x, na_rm);
  } else {
    return runner_vec<14>(x, aggr::calc_max, k, lag, idx, at, na_rm, na_pad);
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
    IntegerVector k = IntegerVector(0),
    IntegerVector lag = IntegerVector(1),
    IntegerVector idx = IntegerVector(0),
    IntegerVector at = IntegerVector(0),
    bool na_rm = true,
    bool na_pad = false) {

  if (k.size() == 0 &&
      lag.size() == 1 &&
      lag(0) == 0 &&
      idx.size() == 0 &&
      at.size() == 0) {
    return aggr::cummin(x, na_rm);
  } else {
    return runner_vec<14>(x, aggr::calc_min, k, lag, idx, at, na_rm, na_pad);
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
    Rcpp::IntegerVector k = IntegerVector(0),
    Rcpp::IntegerVector lag = IntegerVector(1),
    Rcpp::IntegerVector idx = IntegerVector(0),
    Rcpp::IntegerVector at = IntegerVector(0),
    bool na_rm = true,
    bool na_pad = false) {

  if (k.size() == 0 &&
      lag.size() == 1 &&
      lag(0) == 0 &&
      idx.size() == 0 &&
      at.size() == 0) {
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
    case INTSXP: return runner_vec<13>(as<IntegerVector>(x),
                                       aggr::calc_streak_i,
                                       k, lag, idx, at, na_rm, na_pad);
    case REALSXP: return runner_vec<13>(as<NumericVector>(x),
                                        aggr::calc_streak_n,
                                        k, lag, idx, at, na_rm, na_pad);
    case STRSXP: return runner_vec<13>(as<StringVector>(x),
                                       aggr::calc_streak_s,
                                       k, lag, idx, at, na_rm, na_pad);
    case LGLSXP: return runner_vec<13>(as<LogicalVector>(x),
                                       aggr::calc_streak_l,
                                       k, lag, idx, at, na_rm, na_pad);
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
    IntegerVector k = IntegerVector(0),
    IntegerVector lag = IntegerVector(1),
    IntegerVector idx = IntegerVector(0),
    IntegerVector at = IntegerVector(0),
    std::string which = "last",
    bool na_rm = true,
    bool na_pad = false) {

  if (which != "last" && which != "first") {
    stop("which value should be either 'first' or 'last'");
  }

  if (which == "first") {
    if (k.size() == 0 &&
        lag.size() == 1 &&
        lag(0) == 0 &&
        idx.size() == 0 &&
        at.size() == 0) {
      return aggr::cumwhichf(x, na_rm);
    } else {
      return runner_vec<13>(x, aggr::calc_whichf, k, lag, idx, at, na_rm, na_pad);
    }

  } else {
    if (k.size() == 0 &&
        lag.size() == 1 &&
        lag(0) == 0 &&
        idx.size() == 0 &&
        at.size() == 0) {
      return aggr::cumwhichl(x, na_rm);
    } else {
      return runner_vec<13>(x, aggr::calc_whichl, k, lag, idx, at, na_rm, na_pad);
    }
  }

  return R_NilValue;
}

template <int ITYPE>
Rcpp::List
  window_create(
    Rcpp::Vector<ITYPE> const& x,
    Rcpp::IntegerVector const& k,
    Rcpp::IntegerVector const& lag,
    Rcpp::IntegerVector const& idx,
    Rcpp::IntegerVector const& at,
    bool na_pad) {

    int n = x.size();
    int nn = at.size() == 0 ? x.size() : at.size();
    std::string var = at.size() == 0 ? "x" : "at";

    checks::check_k(k, nn, var);
    checks::check_lag(lag, nn, var);
    checks::check_idx(idx, n, var);
    checks::check_at(at);

    Rcpp::List  res(nn);
    IntegerVector b(2);

    // Simple windows-------
    if (at.size() == 0) {
      if (idx.size() == 0) {
        if (k.size() > 1 && lag.size() > 1) {
          for (int i = 0; i < n; i++) {
            b = utils::window_ul(i, k(i), lag(i), n, na_pad);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<ITYPE>(0);
            } else {
              res(i) = listfuns::get_window(x, b(1), b(0));
            }
          }
        } else if (k.size() > 1 && lag.size() == 1) {
          for (int i = 0; i < n; i++) {
            b = utils::window_ul(i, k(i), lag(0), n, na_pad);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<ITYPE>(0);
            } else {
              res(i) = listfuns::get_window(x, b(1), b(0));
            }
          }
        } else if (k.size() == 0 && lag.size() > 1) {
          for (int i = 0; i < n; i++) {
            b = utils::window_ul(i, n, lag(i), n, na_pad, true);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<ITYPE>(0);
            } else {
              res(i) = listfuns::get_window(x, b(1), b(0));
            }
          }
        } else if (k.size() == 0 && lag.size() == 1) {
          for (int i = 0; i < n; i++) {
            b = utils::window_ul(i, n, lag(0), n, na_pad, true);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<ITYPE>(0);
            } else {
              res(i) = listfuns::get_window(x, b(1), b(0));
            }
          }
        } else if (k.size() == 1 && lag.size() > 1) {
          for (int i = 0; i < n; i++) {
            b = utils::window_ul(i, k(0), lag(i), n, na_pad);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<ITYPE>(0);
            } else {
              res(i) = listfuns::get_window(x, b(1), b(0));
            }
          }
        } else if (k.size() == 1 && lag.size() == 1) {
          for (int i = 0; i < n; i++) {
            b = utils::window_ul(i, k(0), lag(0), n, na_pad);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<ITYPE>(0);
            } else {
              res(i) = listfuns::get_window(x, b(1), b(0));
            }
          }
        }

      } else {
        if (k.size() > 1) {
          if (lag.size() > 1) {
            for (int i = 0; i < n; i++) {
              b = utils::window_ul_dl(idx, i, k(i), lag(i), n, na_pad, false);
              if (b.size() == 0) {
                res(i) = Rcpp::Vector<ITYPE>(0);
              } else {
                res(i) = listfuns::get_window(x, b(1), b(0));
              }
            }
          } else {
            for (int i = 0; i < n; i++) {
              b = utils::window_ul_dl(idx, i, k(i), lag(0), n, na_pad, false);
              if (b.size() == 0) {
                res(i) = Rcpp::Vector<ITYPE>(0);
              } else {
                res(i) = listfuns::get_window(x, b(1), b(0));
              }
            }
          }
        } else if (k.size() == 0) {
          if (lag.size() > 1) {
            for (int i = 0; i < n; i++) {
              b = utils::window_ul_dl(idx, i, n, lag(i), n, na_pad, true);
              if (b.size() == 0) {
                res(i) = Rcpp::Vector<ITYPE>(0);
              } else {
                res(i) = listfuns::get_window(x, b(1), b(0));
              }
            }
          } else {
            for (int i = 0; i < n; i++) {
              b = utils::window_ul_dl(idx, i, n, lag(0), n, na_pad, true);
              if (b.size() == 0) {
                res(i) = Rcpp::Vector<ITYPE>(0);
              } else {
                res(i) = listfuns::get_window(x, b(1), b(0));
              }
            }
          }
        } else {
          if (lag.size() > 1) {
            for (int i = 0; i < n; i++) {
              b = utils::window_ul_dl(idx, i, k(0), lag(i), n, na_pad, false);
              if (b.size() == 0) {
                res(i) = Rcpp::Vector<ITYPE>(0);
              } else {
                res(i) = listfuns::get_window(x, b(1), b(0));
              }
            }
          } else {
            for (int i = 0; i < n; i++) {
              b = utils::window_ul_dl(idx, i, k(0), lag(0), n, na_pad, false);
              if (b.size() == 0) {
                res(i) = Rcpp::Vector<ITYPE>(0);
              } else {
                res(i) = listfuns::get_window(x, b(1), b(0));
              }
            }
          }
        }
      }
    } else {
      if (idx.size() == 0) {
        if (k.size() > 1 && lag.size() > 1) {
          for (int i = 0; i < nn; i++) {
            b = utils::window_ul(at(i) - 1, k(i), lag(i), n, na_pad);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<ITYPE>(0);
            } else {
              res(i) = listfuns::get_window(x, b(1), b(0));
            }
          }
        } else if (k.size() > 1 && lag.size() == 1) {
          for (int i = 0; i < nn; i++) {
            b = utils::window_ul(at(i) - 1, k(i), lag(0), n, na_pad);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<ITYPE>(0);
            } else {
              res(i) = listfuns::get_window(x, b(1), b(0));
            }
          }
        } else if (k.size() == 0 && lag.size() > 1) {
          for (int i = 0; i < nn; i++) {
            b = utils::window_ul(at(i) - 1, n, lag(i), n, na_pad, true);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<ITYPE>(0);
            } else {
              res(i) = listfuns::get_window(x, b(1), b(0));
            }
          }
        } else if (k.size() == 0 && lag.size() == 1) {
          for (int i = 0; i < nn; i++) {
            b = utils::window_ul(at(i) - 1, n, lag(0), n, na_pad, true);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<ITYPE>(0);
            } else {
              res(i) = listfuns::get_window(x, b(1), b(0));
            }
          }
        } else if (k.size() == 1 && lag.size() > 1) {
          for (int i = 0; i < nn; i++) {
            b = utils::window_ul(at(i) - 1, k(0), lag(i), n, na_pad);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<ITYPE>(0);
            } else {
              res(i) = listfuns::get_window(x, b(1), b(0));
            }
          }
        } else if (k.size() == 1 && lag.size() == 1) {
          for (int i = 0; i < nn; i++) {
            b = utils::window_ul(at(i) - 1, k(0), lag(0), n, na_pad);
            if (b.size() == 0) {
              res(i) = Rcpp::Vector<ITYPE>(0);
            } else {
              res(i) = listfuns::get_window(x, b(1), b(0));
            }
          }
        }

      } else {
        if (k.size() > 1) {
          if (lag.size() > 1) {
            for (int i = 0; i < nn; i++) {
              b = utils::window_ul_at(idx, at(i), k(i), lag(i), n, na_pad, false);
              if (b.size() == 0) {
                res(i) = Rcpp::Vector<ITYPE>(0);
              } else {
                res(i) = listfuns::get_window(x, b(1), b(0));
              }
            }
          } else if (lag(0) != 0){
            for (int i = 0; i < nn; i++) {
              b = utils::window_ul_at(idx, at(i), k(i), lag(0), n, na_pad, false);
              if (b.size() == 0) {
                res(i) = Rcpp::Vector<ITYPE>(0);
              } else {
                res(i) = listfuns::get_window(x, b(1), b(0));
              }
            }
          } else {
            for (int i = 0; i < nn; i++) {
              b = utils::window_ul_at(idx, at(i), k(i), 0, n, na_pad, false);
              if (b.size() == 0) {
                res(i) = Rcpp::Vector<ITYPE>(0);
              } else {
                res(i) = listfuns::get_window(x, b(1), b(0));
              }
            }
          }
        } else if (k.size() == 0) {
          if (lag.size() > 1) {
            for (int i = 0; i < nn; i++) {
              b = utils::window_ul_at(idx, at(i), n, lag(i), n, na_pad, true);
              if (b.size() == 0) {
                res(i) = Rcpp::Vector<ITYPE>(0);
              } else {
                res(i) = listfuns::get_window(x, b(1), b(0));
              }
            }
          } else if (lag(0) != 0){
            for (int i = 0; i < nn; i++) {
              b = utils::window_ul_at(idx, at(i), n, lag(0), n, na_pad, true);
              if (b.size() == 0) {
                res(i) = Rcpp::Vector<ITYPE>(0);
              } else {
                res(i) = listfuns::get_window(x, b(1), b(0));
              }
            }
          } else {
            for (int i = 0; i < nn; i++) {
              b = utils::window_ul_at(idx, at(i), n, 0, n, na_pad, true);
              if (b.size() == 0) {
                res(i) = Rcpp::Vector<ITYPE>(0);
              } else {
                res(i) = listfuns::get_window(x, b(1), b(0));
              }
            }
          }
        } else if (k.size() == 1) {
          if (lag.size() > 1) {
            for (int i = 0; i < nn; i++) {
              b = utils::window_ul_at(idx, at(i), k(0), lag(i), n, na_pad, false);
              if (b.size() == 0) {
                res(i) = Rcpp::Vector<ITYPE>(0);
              } else {
                res(i) = listfuns::get_window(x, b(1), b(0));
              }
            }
          } else if (lag(0) != 0) {
            for (int i = 0; i < nn; i++) {
              b = utils::window_ul_at(idx, at(i), k(0), lag(0), n, na_pad, false);
              if (b.size() == 0) {
                res(i) = Rcpp::Vector<ITYPE>(0);
              } else {
                res(i) = listfuns::get_window(x, b(1), b(0));
              }
            }
          } else {
            for (int i = 0; i < nn; i++) {
              b = utils::window_ul_at(idx, at(i), k(0), 0, n, na_pad, false);
              if (b.size() == 0) {
                res(i) = Rcpp::Vector<ITYPE>(0);
              } else {
                res(i) = listfuns::get_window(x, b(1), b(0));
              }
            }
          }
        }
      }
    }

    return res;

  }

//' List of running windows
//'
//' Creates \code{list} of windows with given arguments settings.
//' Length of output \code{list} is equal
//' @inheritParams runner
//' @return list of vectors (windows). Length of list is the same as
//' \code{length(x)} or \code{length(at)} if specified, and length of each
//'  window is defined by \code{k} (unless window is out of range).
//' @examples
//' window_run(1:10, k = 3, lag = -1)
//' window_run(letters[1:10], k = c(1, 2, 2, 4, 5, 5, 5, 5, 5, 5))
//' @export
// [[Rcpp::export]]
SEXP window_run(SEXP x,
                IntegerVector k = IntegerVector(0),
                IntegerVector lag = IntegerVector(1),
                IntegerVector idx = IntegerVector(0),
                IntegerVector at = IntegerVector(0),
                bool na_pad = false) {

  switch (TYPEOF(x)) {
  case INTSXP:  return window_create(as<IntegerVector>(x), k, lag, idx, at, na_pad);
  case REALSXP: return window_create(as<NumericVector>(x), k, lag, idx, at, na_pad);
  case STRSXP:  return window_create(as<CharacterVector>(x), k, lag, idx, at, na_pad);
  case LGLSXP:  return window_create(as<LogicalVector>(x), k, lag, idx, at, na_pad);
  default: {
    stop("Invalid 'x' type - only integer, numeric, character, factor, date and logical vectors are possible.");
  }
  }

  return R_NilValue;
}
