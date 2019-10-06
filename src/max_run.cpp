#include <Rcpp.h>
using namespace Rcpp;
#include "max_run.h"

//' Running minimum
//'
//'
//' \code{min_run} calculates running min on given \code{x} numeric vector, specified \code{k} window size.
//' @inheritParams runner
//' @inheritParams sum_run
//' @return numeric vector of length equals length of \code{x} containing running min in \code{k}-long window.
//' @examples
//' set.seed(11)
//' x1 <- sample( c(1,2,3), 15, replace=TRUE)
//' x2 <- sample( c(NA,1,2,3), 15, replace=TRUE)
//' k  <- sample( 1:4, 15, replace=TRUE)
//' min_run(x1) # simple cumulative minimum
//' min_run(x2, na_rm = TRUE) # cumulative minimum with removing NA.
//' min_run(x2, na_rm = TRUE, k=4) # minimum in 4-element window
//' min_run(x2, na_rm = FALSE, k=k) # minimum in varying k window size
//' @export
// [[Rcpp::export]]
NumericVector max_run(
    NumericVector x,
    IntegerVector k = 0,
    IntegerVector lag = 0,
    bool na_rm = true,
    bool na_pad = false,
    IntegerVector idx = IntegerVector(0)
){

  int n = x.size();

  if(k.size() == 1 && k(0) == 0) {
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

  if (lag.size() != n and lag.size() > 1) {
    stop("length of lag and length of x differs. length(lag) should be 1 or equal to x");
  } else if (Rcpp::any(Rcpp::is_na(lag))) {
    stop("Function doesn't accept NA values in lag vector");
  }

  NumericVector res;

  /* not windowed - cum_max */
  if((k(0) <= 1 or k(0) == n ) and k.size() == 1){
    res = min::window_max(x, na_rm);

    /* windowed */
  } else if(k.size() == 1){
    res = idx.size() == 1 ? min::window_max21(x, k, na_rm) : min::window_max31(x, k, idx, na_rm);

    /* varying window size */
  } else if( k.size() > 0){
    res = idx.size() == 1 ? min::window_max22(x, k, na_rm) : min::window_max32(x, k, idx, na_rm);
  }

  /* pad NA's */
  if(na_pad)
    std::fill(res.begin(), res.end() - n + k(0) - 1 , NA_REAL);

  return res;
}

