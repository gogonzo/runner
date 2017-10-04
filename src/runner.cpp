#include <Rcpp.h>
using namespace Rcpp;
#include "helps.h"
#include "extremes.h"
#include "sums.h"
#include "streak.h"
#include "whiches.h"
#include "others.h"

//' Running maximum window
//'
//' \code{min_run} calculates window-running max on given \code{x} numeric vector, specified \code{k} window size and additional options (\code{na_pad},\code{na_rm}) to handle missing values.
//' @param x input numeric vector where running minimum is calculated.
//' @param k Running window size. By default window size equals the length of \code{x}.
//' @param na_pad logical (default \code{na_pad=FALSE}) - if \code{TRUE} first k-results will be filled by \code{NA}. If k is not specified na_pad=F by default.
//' @param na_rm logical (default \code{na_rm=TRUE}) - if \code{TRUE} \code{NA} are replaced by last observed minimum prior to element.
//' @return numeric vector of length equals length of \code{x} containing running min in \code{k}-long window.
//' @examples
//' x <- c( NA, 1, -1, NA, -1, -2, -6, 4)
//'
//' min_run( x, k=2, na_rm=TRUE, na_pad=FALSE )
//' @export
// [[Rcpp::export]]
NumericVector min_run(
  NumericVector x,
  IntegerVector k = 0,
  bool na_pad = false,
  bool na_rm = true,
  bool na_fill = true
  ){

    int n = x.size();
    impl::check_for_valid_k2(n, k);
    NumericVector res;

    /* not windowed - cum_min */
    if( ( k(0) <= 1 or k(0) == n ) and k.size() == 1 ){
      res = impl::cum_min(x, na_rm, na_fill );

    /* windowed */
    } else if( k.size() == 1 ){
      res = impl::window_min(x, k(0), na_rm, na_fill);

    /* varying window size */
    } else {
      res = impl::window_min2(x, k, na_rm, na_fill);
    }

    /* pad NA's */
    if(na_pad)
      std::fill(res.begin(), res.end() - n + k(0) - 1 , NA_REAL);

    return res;
}


//' Running maximum window
//'
//' \code{min_run} calculates window-running min on given \code{x} numeric vector, specified \code{k} window size and additional options (\code{na_pad},\code{na_rm}) to handle missing values.
//' @param x input numeric vector where running maximum is calculated.
//' @param k Running window size. By default window size equals the length of \code{x}.
//' @param na_pad logical (default \code{na_pad=FALSE}) - if \code{TRUE} first k-results will be filled by \code{NA}. If k is not specified na_pad=F by default.
//' @param na_rm logical (default \code{na_rm=TRUE}) - if \code{TRUE} \code{NA} are replaced by last observed maximum prior to element.
//' @return numeric vector of length equals length of \code{x} containing running max in \code{k}-long window.
//' @examples
//' x <- c(NA,1,-1,NA, -1, -2,-6,4)
//' max_run(x, k=2, na_rm=TRUE, na_pad=FALSE)
//' @export
// [[Rcpp::export]]
NumericVector max_run(
    NumericVector x,
    IntegerVector k = IntegerVector::create(0),
    bool na_pad = false,
    bool na_rm = true,
    bool na_fill = true
){

  int n = x.size();
  impl::check_for_valid_k2(n, k);
  NumericVector res;

  /* not windowed - cum_min */
  if( ( k(0) <= 1 or k(0) == n ) and k.size() == 1 ){
    res = impl::cum_max( x, na_rm, na_fill );

  /* windowed */
  } else if( k.size() == 1 ){
    res = impl::window_max(x, k(0), na_rm, na_fill);

  /* varying window size */
  } else {
    res = impl::window_max2(x, k, na_rm, na_fill);
  }

  /* pad NA's */
  if(na_pad)
    std::fill(res.begin(), res.end() - n + k(0) - 1 , NA_REAL);

  return res;
}

//' Streak length of vector elements
//'
//' Calculates series of consecutive elements
//'
//' @param x Vector of any type on which consecutive streak is calculated
//' @param k Running window size. By default window size equals the length of \code{x}.
//' @param na_pad logical (default \code{na_pad=FALSE}) - if \code{TRUE} first k-results will be filled by \code{NA}. If k is not specified na_pad=F by default.
//' @return numeric vector of length equals length of \code{x} containing running streak length in \code{k}-long window.
//' @examples
//' x <- c(1,1,0,0,1,0,3,3,1,1)
//' streak_run(x, k=2, na_pad=FALSE)
//' @export
// [[Rcpp::export]]
IntegerVector streak_run(SEXP x, IntegerVector k=0, bool na_pad = false) {

  impl::check_for_valid_k(x, k);

  switch (TYPEOF(x)) {
  case INTSXP: {
    return impl::streak_run_(as<IntegerVector>(x), k, na_pad);
  }
  case REALSXP: {
    return impl::streak_run_(as<NumericVector>(x), k, na_pad);
  }
  case STRSXP: {
    return impl::streak_run_(as<CharacterVector>(x), k, na_pad);
  }
  case LGLSXP: {
    return impl::streak_run_(as<LogicalVector>(x), k, na_pad);
  }
  case CPLXSXP: {
    return impl::streak_run_(as<ComplexVector>(x), k, na_pad);
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


//' Fill NA with previous non-NA element
//'
//'
//' @param x A single integer.
//' @examples
//' x <- c(NA,1,-1,NA, 1, 2,6,4)
//' max_run(x)
//' @export
// [[Rcpp::export]]

SEXP fill_run(SEXP x, bool run_for_first = false) {

  switch (TYPEOF(x)) {
  case INTSXP: {
    return impl::fill_run_impl(as<IntegerVector>(x), run_for_first);
  }
  case REALSXP: {
    return impl::fill_run_impl(as<NumericVector>(x), run_for_first);
  }
  case STRSXP: {
    return impl::fill_run_impl(as<CharacterVector>(x), run_for_first);
  }
  case LGLSXP: {
    return impl::fill_run_impl(as<LogicalVector>(x), run_for_first);
  }
  case CPLXSXP: {
    return impl::fill_run_impl(as<ComplexVector>(x), run_for_first);
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



//' Running window mean
//'
//' @param x Vector of any type on which window mean is calculated
//' @param k Running window size. By default window size equals the length of \code{x}.
//' @param na_pad logical (default \code{na_pad=FALSE}) - if \code{TRUE} first k-results will be filled by \code{NA}. If k is not specified na_pad=F by default.
//' @return numeric vector of length equals length of \code{x} containing running mean in \code{k}-long window.
//' @examples
//' x <- c(NA,3,4,3,5,7,NA,2,4,9)
//' mean_run(x , k = 3, na_rm = T, na_pad = F)
//' @export
// [[Rcpp::export]]
NumericVector mean_run(

    NumericVector x,
    IntegerVector k=0,
    bool na_pad = false,
    bool na_rm = true ) {

  int n = x.size();
  NumericVector res(n);
  NumericVector nas(n);
  impl::check_for_valid_k2(n, k);

  if(k.size() == 1 ){
    /* calculate window sum */
    res = impl::calc_sum_window(x, res, k(0) );
    if(!na_rm){
      /* calculate number of NA's in window */
      nas = impl::count_na_window(x, k(0) );

      for(int i =0; i < n; i++)
        if(nas ( i )>0 )
          res( i ) = NumericVector::get_na();

    } else {




    }

    /* mean = sum/(k - number_of_nas) */
    for(int i = 0; i < n; i ++)
      res( i ) = res( i )/( k(0) - nas(0) );
}

  /* pad first-k elements with NA */
  if(na_pad)
    std::fill(res.begin(), res.end() - n + k(0) - 1 , NA_REAL);

  return res;
}


#include <Rcpp.h>
using namespace Rcpp;
//' Running window sum
//'
//'
//' @param x Vector of any type on which running sum is calculated
//' @param k Running window size. By default window size equals the length of \code{x}.
//' @param na_pad logical (default \code{na_pad=FALSE}) - if \code{TRUE} first k-results will be filled by \code{NA}. If k is not specified na_pad=F by default.
//' @return numeric vector of length equals length of \code{x} containing running window sum in \code{k}-long window.
//' @examples
//' x <- c(NA,3,4,3,5,7,NA,2,4,9)
//' sum_run(x , k = 3, na_rm = T, na_pad = F)
//' @export
// [[Rcpp::export]]
NumericVector sum_run(
    NumericVector x,
    IntegerVector k=0,
    bool na_pad = false,
    bool na_rm = true ) {

  int n = x.size();
  NumericVector res(n);
  NumericVector nas(n);
  impl::check_for_valid_k2(n, k);

  /* calculate window sum */
  res = impl::calc_sum_window(x, res, k(0) );
  if(!na_rm){
    /* calculate number of NA's in window */
    nas = impl::count_na_window(x, k(0) );

    for(int i =0; i < n; i++)
      if(nas ( i )>0 )
        res( i ) = NumericVector::get_na();
  }

  /* pad NA at from 0:(k-1) */
  if(na_pad)
    std::fill(res.begin(), res.end() - n + k(0) - 1 , NA_REAL);

  return res;
}
