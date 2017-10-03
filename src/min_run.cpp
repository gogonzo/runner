#include <Rcpp.h>
using namespace Rcpp;
#include "helps.h"

//' Running maximum window
//'
//' Running maximum window
//'
//' @param x input numeric vector where running minimum is calculated.
//' @param k Running window size. By default window size equals the length of x vector argument.
//' @param na_pad logical (default=FALSE) - if TRUE first k-results will be filled by NA's. If k is not specified na_pad=F by default.
//' @param na_rm logical (default=TRUE) - if TRUE NA's is replaced by last observed minimum.
//' @examples
//' x <- c( NA, 1, -1, NA, -1, -2, -6, 4)
//'
//' min_run( x, k=2, na_rm=TRUE, na_pad=FALSE )
//' @export
// [[Rcpp::export]]
NumericVector min_run(
  NumericVector x,
  IntegerVector k = IntegerVector::create(0),
  bool na_pad = false,
  bool na_rm = true
  ){

    // check for negative k and return error //
    // check if k.size == x.size or k.size == 1

    int i1;
    int n = x.size();
    NumericVector res(n);

    if( ( k(0) <= 1 or k(0) == n ) and k.size() == 1 ){
      // if not windowed - ponly run
      double cur_min = NumericVector::get_na();
      for(int i = 0; i < n; i++){
        if(!na_rm and NumericVector::is_na(x(i)) ){
          res(i) = NumericVector::get_na();
          continue;
        }

        cur_min = impl::calc_min(x(i), cur_min);
        res( i ) = cur_min;
      }

    } else if( k.size() == 1 ){
      // for k const
      for(int i = 0; i < n; i++){
        if(!na_rm and NumericVector::is_na(x(i)) ){
          res(i) = NumericVector::get_na();
          continue;
        }

        double cur_min = NumericVector::get_na();
        i1 = impl::window_index(i, k(0));
        for(int j = i1; j <= i ; ++j){
          cur_min = impl::calc_min( x(j), cur_min);
        }
        res( i ) = cur_min;
      }

    } else {
      // for k as vector
      for(int i = 0; i < n; i++){
        if(!na_rm and NumericVector::is_na(x(i)) ){
          res(i) = NumericVector::get_na();
          continue;
        }
        double cur_min = NumericVector::get_na();
        i1 = impl::window_index( i, k( i ) );
        for(int j = i1; j <= i ; ++j){
          cur_min = impl::calc_min( x( j ), cur_min);
        }
        res( i ) = cur_min;
      }
    }

    // if na pad
    if(na_pad)
      std::fill(res.begin(), res.end() - n + k(0) - 1 , NA_REAL);

    return res;
}


//' Running maximum window
//'
//' Running maximum window
//'
//' @param x input numeric vector where running maximum is calculated.
//' @param k Running window size. By default window size equals the length of x vector argument.
//' @param na_pad logical (default=FALSE) - if TRUE first k-results will be filled by NA's. If k is not specified na_pad=F by default.
//' @param na_rm logical (default=TRUE) - if TRUE NA's is replaced by last observed maximum.
//' @examples
//' x <- c(NA,1,-1,NA, -1, -2,-6,4)
//' max_run(x, k=2, na_rm=TRUE, na_pad=FALSE)
//' @export
// [[Rcpp::export]]
NumericVector max_run(
    NumericVector x,
    IntegerVector k = IntegerVector::create(0),
    bool na_pad = false,
    bool na_rm = true
){

  // check for negative k and return error //
  // check if k.size == x.size or k.size == 1

  int i1;
  int n = x.size();
  NumericVector res(n);

  if( ( k(0) <= 1 or k(0) == n ) and k.size() == 1 ){
    // if not windowed - ponly run
    double cur_max = NumericVector::get_na();
    for(int i = 0; i < n; i++){
      if(!na_rm and NumericVector::is_na(x(i)) ){
        res(i) = NumericVector::get_na();
        continue;
      }
      cur_max = impl::calc_max(x(i), cur_max);
      res( i ) = cur_max;
    }

  } else if( k.size() == 1 ){
    // for k const
    for(int i = 0; i < n; i++){
      if(!na_rm and NumericVector::is_na(x(i)) ){
        res(i) = NumericVector::get_na();
        continue;
      }

      double cur_max = NumericVector::get_na();
      i1 = impl::window_index(i, k(0));
      for(int j = i1; j <= i ; ++j){
        cur_max = impl::calc_max( x(j), cur_max);
      }
      res( i ) = cur_max;
    }

  } else {
    // for k as vector
    for(int i = 0; i < n; i++){
      if(!na_rm and NumericVector::is_na(x(i)) ){
        res(i) = NumericVector::get_na();
        continue;
      }
      double cur_max = NumericVector::get_na();
      i1 = impl::window_index( i, k( i ) );
      for(int j = i1; j <= i ; ++j){
        cur_max = impl::calc_max( x( j ), cur_max);
      }
      res( i ) = cur_max;
    }
  }

  // if na pad
  if(na_pad)
    std::fill(res.begin(), res.end() - n + k(0) - 1 , NA_REAL);

  return res;
}

//' Streak length of vector elements
//'
//' Calculates series of consecutive elements
//'
//' @param x Vector of any type on which consecutive streak is calculated
//' @param k Running window size. By default window size equals the length of x vector argument.
//' @param na_pad logical - if TRUE first k-results will be filled by NA's. If k is not specified na_pad=F by default.
//' @examples
//' x <- c(1,1,0,0,1,0,3,3,1,1)
//' streak_run(x, k=2, na_pad=FALSE)
//' @export
// [[Rcpp::export]]
IntegerVector streak_run(SEXP x, IntegerVector k = IntegerVector::create(0), bool na_pad = false) {
  switch (TYPEOF(x)) {
  case INTSXP: {
    return impl::streak_run_impl(as<IntegerVector>(x), k, na_pad);
  }
  case REALSXP: {
    return impl::streak_run_impl(as<NumericVector>(x), k, na_pad);
  }
  case STRSXP: {
    return impl::streak_run_impl(as<CharacterVector>(x), k, na_pad);
  }
  case LGLSXP: {
    return impl::streak_run_impl(as<LogicalVector>(x), k, na_pad);
  }
  case CPLXSXP: {
    return impl::streak_run_impl(as<ComplexVector>(x), k, na_pad);
  }
  default: {
    warning(
      "Invalid SEXPTYPE %d (%s).\n",
      TYPEOF(x), type2name(x)
    );
    return 0;
  }
  }
}


