#include <Rcpp.h>
using namespace Rcpp;
#include "max_run.h"

//' Running minimum
//'
//'
//' \code{min_run} calculates running min on given \code{x} numeric vector, specified \code{k} window size.
//' @param x input numeric vector where running minimum is calculated.
//' @param k Running window size. By default window size equals \code{length(x)}. Allow varying window size specified by vector of \code{length(x)}
//' @param na_pad logical (default \code{na_pad=FALSE}) - if \code{TRUE} first k-results will be filled by \code{NA}. If k is not specified na_pad=F by default.
//' @param na_rm logical (default \code{na_rm=TRUE}) - if \code{TRUE} \code{NA} are replaced by last observed minimum prior to element.
//' @param idx an optional integer vector containing idx numbers of observation.
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
    bool na_rm = true,
    bool na_pad = false,
    IntegerVector idx = 0
){

  int n = x.size();
  NumericVector res;

  if( k(0) == 0 ){
    k(0) = n;
  } else if(k.size() != n and k.size() > 1){
    stop("length of k and length x differs. k=0 and k=length(x) only allowed");
  } else if( Rcpp::any(Rcpp::is_na(k)) ){
    stop("Function doesn't accept NA values in k vector");
  }

  /* not windowed - cum_max */
  if( ( k(0) <= 1 or k(0) == n ) and k.size() == 1 ){
    res = min::window_max(x, na_rm);

    /* windowed */
  } else if( k.size() == 1 ){
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
