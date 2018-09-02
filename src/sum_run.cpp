#include <Rcpp.h>
using namespace Rcpp;
#include "sums.h"

//' Running sum
//'
//' Running sum in specified window of numeric vector.
//' @param x vector of any type where running sum is calculated
//' @param k Running window size.  Not yet implemented.
//' @param na_rm logical (default \code{na_rm=TRUE}) - if \code{TRUE} sum is calulating excluding \code{NA}.
//' @param na_pad logical (default \code{na_pad=FALSE}) - if \code{TRUE} first k-results will be filled by \code{NA}. If k is not specified na_pad=F by default.
//' @return numeric vector of length equals length of \code{x} containing running sum in \code{k}-long window.
//' @examples
//' set.seed(11)
//' x1 <- rnorm(15)
//' x2 <- sample(c(rep(NA,5),rnorm(15)), 15, replace=TRUE)
//' k <- sample(1:15, 15, replace=TRUE)
//' sum_run(x1)
//' sum_run(x2, na_rm = TRUE)
//' sum_run(x2, na_rm = FALSE )
//' sum_run(x2, na_rm = TRUE, k=4)
//' @export
// [[Rcpp::export]]
NumericVector sum_run(
    NumericVector x,
    IntegerVector k=0,
    bool na_rm = true,
    bool na_pad = false) {

  int n = x.size();
  NumericVector res(n);
  NumericVector nas(n);

  if( k(0) == 0 ){
    k(0) = n;
  } else if( k.size() != n and k.size() > 1 ){
    stop("length of k and length x differs. k=0 and k=length(x) only allowed");
  } else if( Rcpp::any(Rcpp::is_na(k)) ){
    stop("Function doesn't accept NA values in k vector");
  }

  /* calculate window sum */
  if( k.size() == 1 ){
    res = impl::calc_sum_window(x, res, k(0) );
    if(!na_rm){
      /* calculate number of NA's in window */
      nas = impl::count_na_window(x, k(0) );
      for(int i =0; i < n; i++)
        if(nas ( i )>0 )
          res( i ) = NumericVector::get_na();
    }

  } else {
    res = impl::calc_sum_window2(x, res, k );
  }


  /* pad NA at from 0:(k-1) */
  if(na_pad)
    std::fill(res.begin(), res.end() - n + k(0) - 1 , NA_REAL);

  return res;
}

//' Running mean
//'
//'
//' @param x vector of any type on which running mean is calculated
//' @param k running window size. Not yet implemented.
//' @param na_rm logical (default \code{na_rm=TRUE}) - if \code{TRUE} mean is calulating excluding \code{NA}.
//' @param na_pad logical (default \code{na_pad=FALSE}) - if \code{TRUE} first k-results will be filled by \code{NA}. If k is not specified na_pad=F by default.
//' @return numeric vector of length equals length of \code{x} containing running mean in \code{k}-long window.
//' @examples
//' set.seed(11)
//' x1 <- rnorm(15)
//' x2 <- sample(c(rep(NA,5),rnorm(15)), 15, replace=TRUE)
//' k <- sample(1:15, 15, replace=TRUE)
//' mean_run(x1)
//' mean_run(x2, na_rm = TRUE)
//' mean_run(x2, na_rm = FALSE )
//' mean_run(x2, na_rm = TRUE, k=4)
//' @export
// [[Rcpp::export]]
NumericVector mean_run(
    NumericVector x,
    IntegerVector k=0,
    bool na_rm = true,
    bool na_pad = false) {

  int n = x.size();
  NumericVector res(n);
  NumericVector nas(n);

  if( k(0) == 0 ){
    k(0) = n;
  } else if( k.size() != n and k.size() > 1 ){
    stop("length of k and length x differs. k=0 and k=length(x) only allowed");
  } else if( Rcpp::any(Rcpp::is_na(k)) ){
    stop("Function doesn't accept NA values in k vector");
  }

  if(k.size() == 1 ){
    res = impl::calc_sum_window(x, res, k(0) );
    nas = impl::count_na_window(x, k(0) );

    if(!na_rm){
      for(int i =0; i < n; i++)
        if(nas ( i )>0 )
          res( i ) = NumericVector::get_na();
    }

    /* mean = sum/(k - number_of_nas) */
    for(int i = 0; i < n; i ++)
      if(k(0)<=i)
        res( i ) = res( i )/( k(0) - nas(i) );
      else
        res( i ) = res( i )/( i + 1 - nas(i) );

  } else {
    res = impl::calc_sum_window2(x, res, k );
    nas = impl::count_na_window2(x, k );

    if(!na_rm){
      for(int i =0; i < n; i++)
        if(nas ( i )>0 )
          res( i ) = NumericVector::get_na();
    }

    /* mean = sum/(k - number_of_nas) */
    for(int i = 0; i < n; i ++)
      if(k(i)<=i)
        res( i ) = res( i )/( k(i) - nas(i) );
      else
        res( i ) = res( i )/( i + 1 - nas(i) );
  }

  /* pad first-k elements with NA */
  if(na_pad)
    std::fill(res.begin(), res.end() - n + k(0) - 1 , NA_REAL);

  return res;
}

