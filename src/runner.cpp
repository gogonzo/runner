#include <Rcpp.h>
using namespace Rcpp;
#include "helps.h"
#include "extremes.h"
#include "which_extremes.h"
#include "sums.h"
#include "streak.h"
#include "whiches.h"
#include "others.h"

//' List of running windows
//'
//' Creates list of windows
//' @param x Vector of any type
//' @param k integer vector which specifies window length
//' @examples
//' window_run(1:10, k=3)
//' window_run(letters[1:10],k=c(1,2,2,4,5,5,5,5,5,5))
//' @export
// [[Rcpp::export]]
SEXP window_run(SEXP x, IntegerVector k = 0) {

  switch (TYPEOF(x)) {
  case INTSXP: return impl::window_to_list(as<IntegerVector>(x), k);
  case REALSXP: return impl::window_to_list(as<NumericVector>(x), k);
  case STRSXP: return impl::window_to_list(as<CharacterVector>(x), k);
  default: {
    warning(
      "Invalid SEXPTYPE %d (%s).\n",
      TYPEOF(x), type2name(x)
    );
    return R_NilValue;
  }
  }
  return R_NilValue;
}


//' List of running windows
//'
//' Creates list of windows
//' @param x Vector of any type
//' @param k integer vector which specifies window length
//' @examples
//' unique_run(1:10, k=3)
//' unique_run(letters[1:10],k=c(1,2,2,4,5,5,5,5,5,5))
//' @export
// [[Rcpp::export]]
SEXP unique_run( SEXP x, IntegerVector k=0 ) {
  switch( TYPEOF(x) ) {
    case INTSXP:  return impl::unique_to_list<INTSXP>(x, k);
    case REALSXP: return impl::unique_to_list<REALSXP>(x, k);
    case STRSXP:  return impl::unique_to_list<STRSXP>(x, k);
  default: {
    warning(
      "Invalid SEXPTYPE %d (%s).\n",
      TYPEOF(x), type2name(x)
    );
    return R_NilValue;
    }
  }
  return R_NilValue;
}

//' Running minimum
//'
//'
//' \code{min_run} calculates running min on given \code{x} numeric vector, specified \code{k} window size.
//' @param x input numeric vector where running minimum is calculated.
//' @param k Running window size. By default window size equals \code{length(x)}. Allow varying window size specified by vector of \code{length(x)}
//' @param na_pad logical (default \code{na_pad=FALSE}) - if \code{TRUE} first k-results will be filled by \code{NA}. If k is not specified na_pad=F by default.
//' @param na_rm logical (default \code{na_rm=TRUE}) - if \code{TRUE} \code{NA} are replaced by last observed minimum prior to element.
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
NumericVector min_run(
  NumericVector x,
  IntegerVector k = 0,
  bool na_rm = true,
  bool na_pad = false
  ){

    int n = x.size();
    impl::check_for_valid_k2(n, k);
    NumericVector res;

    /* not windowed - cum_min */
    if( ( k(0) <= 1 or k(0) == n ) and k.size() == 1 ){
      res = impl::cum_min(x, na_rm);

    /* windowed */
    } else if( k.size() == 1 ){
      res = impl::window_min(x, k(0), na_rm);

    /* varying window size */
    } else {
      res = impl::window_min2(x, k, na_rm);
    }

    /* pad NA's */
    if(na_pad)
      std::fill(res.begin(), res.end() - n + k(0) - 1 , NA_REAL);

    return res;
}


//' Running maximum
//'
//' \code{max_run} calculates running max on given \code{x} numeric vector and specified \code{k} window size.
//' @param x input numeric vector where running maximum is calculated.
//' @param k Running window size. By default window size equals \code{length(x)}. Allow varying window size specified by vector of \code{length(x)}
//' @param na_pad logical (default \code{na_pad=FALSE}) - if \code{TRUE} first k-results will be filled by \code{NA}. If k is not specified na_pad=F by default.
//' @param na_rm logical (default \code{na_rm=TRUE}) - if \code{TRUE} \code{NA} are replaced by last observed maximum prior to element.
//' @return numeric vector of length equals length of \code{x} containing running max in \code{k}-long window.
//' @examples
//' set.seed(11)
//' x1 <- sample(c(1,2,3), 15, replace=TRUE)
//' x2 <- sample(c(NA,1,2,3), 15, replace=TRUE)
//' k  <- sample(1:4, 15,replace=TRUE)
//' max_run(x1) # simple cumulative minimum
//' max_run(x2, na_rm = TRUE) # cumulative minimum with removing NA.
//' max_run(x2, na_rm = TRUE, k=4) # minimum in 4-element window
//' max_run(x2, na_rm = FALSE, k=k) # minimum in varying k window size
//' @export
// [[Rcpp::export]]
NumericVector max_run(
    NumericVector x,
    IntegerVector k = 0,
    bool na_rm = true,
    bool na_pad = false
){

  int n = x.size();
  impl::check_for_valid_k2(n, k);
  NumericVector res;

  /* not windowed - cum_min */
  if( ( k(0) <= 1 or k(0) == n ) and k.size() == 1 ){
    res = impl::cum_max( x, na_rm );

  /* windowed */
  } else if( k.size() == 1 ){
    res = impl::window_max(x, k(0), na_rm);

  /* varying window size */
  } else {
    res = impl::window_max2(x, k, na_rm);
  }

  /* pad NA's */
  if(na_pad)
    std::fill(res.begin(), res.end() - n + k(0) - 1 , NA_REAL);

  return res;
}

//' Running streak length
//'
//' Calculates running series of consecutive elements
//' @param x vector of any type where running streak is calculated
//' @param k running window size. By default window size equals \code{length(x)}. Allow varying window size specified by vector of \code{length(x)}
//' @param na_rm logical (default \code{na_rm=TRUE}) - if \code{TRUE} \code{NA} are replaced by last observed streak prior to element.
//' @param na_pad logical (default \code{na_pad=FALSE}) - if \code{TRUE} first k-results will be filled by \code{NA}. If k is not specified na_pad=F by default.
//' @return numeric vector of length equals length of \code{x} containing running streak length in \code{k}-long window.
//' @examples
//' set.seed(11)
//' x1 <- sample(c("a","b"),15,replace=TRUE)
//' x2 <- sample(c(NA_character_,"a","b"),15,replace=TRUE)
//' k <- sample(1:4,15,replace=TRUE)
//' streak_run(x1) # simple streak run
//' streak_run(x1, k=2) # streak run within 2-element window
//' streak_run(x2, na_pad=TRUE, k=3) # streak run within k=3 with padding NA
//' streak_run(x1, k=k) # streak run within varying window size specified by vector k
//' @export
// [[Rcpp::export]]
IntegerVector streak_run(
    SEXP x,
    IntegerVector k=0,
    bool na_rm = true,
    bool na_pad = false) {

  impl::check_for_valid_k(x, k);

  switch (TYPEOF(x)) {
    case INTSXP: return impl::streak_run_(as<IntegerVector>(x), k, na_rm, na_pad);
    case REALSXP: return impl::streak_run_(as<NumericVector>(x), k, na_rm, na_pad);
    case STRSXP: return impl::streak_run_(as<CharacterVector>(x), k, na_rm, na_pad);
    case LGLSXP: return impl::streak_run_(as<LogicalVector>(x), k, na_rm, na_pad);
    case CPLXSXP: return impl::streak_run_(as<ComplexVector>(x), k, na_rm, na_pad);
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
//' Fill \code{NA} with last non-NA element.
//' @param x Vector of any type where \code{NA} are replaced
//' @param run_for_first If first elements are filled with \code{NA}, \code{run_for_first = TRUE } allows to fill all initial \code{NA} with nearest non-NA value. By befault \code{run_for_first = TRUE}
//' @param only_within \code{NA} are replaced only if previous and next non-NA values are the same. By befault \code{only_within = TRUE}
//' @return numeric vector of length equals length of \code{x} containing all \code{x} elements with \code{NA} replaced with previous non-NA element.
//' @examples
//' fill_run(c(NA,NA,1:10, NA, NA), run_for_first=TRUE)
//' fill_run(c(NA,NA,1:10, NA, NA), run_for_first=TRUE)
//' fill_run(c(NA,NA,1:10, NA, NA), run_for_first=FALSE)
//' fill_run(c(NA,NA,1,2,NA,NA,2,2,NA,NA,1, NA, NA), run_for_first=TRUE,only_within = TRUE)
//' @export
// [[Rcpp::export]]
SEXP fill_run(SEXP x, bool run_for_first = false, bool only_within=false) {

  switch (TYPEOF(x)) {
    case INTSXP: return impl::fill_run_impl(  as<IntegerVector>(x), run_for_first,only_within);
    case REALSXP: return impl::fill_run_impl( as<NumericVector>(x), run_for_first,only_within);
    case STRSXP: return impl::fill_run_impl(  as<CharacterVector>(x), run_for_first,only_within);
    case LGLSXP: return impl::fill_run_impl(  as<LogicalVector>(x), run_for_first,only_within);
    case CPLXSXP: return impl::fill_run_impl( as<ComplexVector>(x), run_for_first,only_within);
  default: {
    warning(
      "Invalid SEXPTYPE %d (%s).\n",
      TYPEOF(x), type2name(x)
    );
    return 0;
  }
  }
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
  impl::check_for_valid_k2(n, k);

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
  impl::check_for_valid_k2(n, k);

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


//' Running which-true function
//'
//' \code{whicht_run} checks \code{which} element has value TRUE within specified running window.
//' @param x input logical vector where running which-true is calculated.
//' @param k Running window size. By default window size equals \code{length(x)}. Allow varying window size specified by vector of \code{length(x)}
//' @param which specifies whether \code{"first"} or \code{"last"} index is returned.
//' @param na_pad logical (default \code{na_pad=FALSE}) - if \code{TRUE} first k-results will be filled by \code{NA}. If k is not specified na_pad=F by default.
//' @param na_rm logical (default \code{na_rm=TRUE}) - if \code{TRUE} \code{NA} are replaced by last observed minimum prior to element.
//' @return numeric vector of length equals length of \code{x} containing running index in \code{k}-long window.
//' @examples
//' x <- c( NA, FALSE, TRUE, NA, TRUE, FALSE, TRUE, TRUE)
//' whicht_run( x, k=2, na_rm=TRUE, na_pad=FALSE )
//' @export
// [[Rcpp::export]]
IntegerVector whicht_run(
    LogicalVector x,
    IntegerVector k = 0,
    std::string which = "last",
    bool na_rm = true,
    bool na_pad = false
){

  int n = x.size();
  impl::check_for_valid_k2(n, k);
  IntegerVector res(n);


  /* not windowed */
  if( ( k(0) <= 1 or k(0) == n ) and k.size() == 1 ){
    if( which == "last")
      res = impl::cum_whicht_l(x, res, na_rm);
    else
      res = impl::cum_whicht_f(x, res, na_rm);

    /* windowed */
  } else if( k.size() == 1 ){

    if( which == "last")
      res = impl::window_whicht_l(x, res, k(0), na_rm);
    else
      res = impl::window_whicht_f(x, res, k(0), na_rm);

    /* varying window size */
  } else {
    if( which == "last")
      res = impl::window_whicht2_l(x, res, k, na_rm);
    else
      res = impl::window_whicht2_f(x, res, k, na_rm);
  }

  /* pad NA's */
  if(na_pad)
    std::fill(res.begin(), res.end() - n + k(0) - 1 , NA_REAL);

  return res;
}

//' Index of previous, different element
//'
//' Index of previous element different than current
//' @param x vector of any type where running index is calculated
//' @param k running window size. By default window size equals \code{length(x)}. Allow varying window size specified by vector of \code{length(x)}
//' @param na_pad logical (default \code{na_pad=FALSE}) - if \code{TRUE} first k-results will be filled by \code{NA}. If k is not specified na_pad=F by default.
//' @return numeric vector of length equals length of \code{x} containing running index length in \code{k}-long window.
//' @examples
//' set.seed(11)
//' x1 <- sample(c("a","b"),15,replace=TRUE)
//' x2 <- sample(c(NA_character_,"a","b"),15,replace=TRUE)
//' k  <- sample(1:4,15,replace=TRUE)
//' whichd_run(x1)
//' whichd_run(x1, k=2)
//' whichd_run(x2, na_pad=TRUE, k=3)
//' whichd_run(x1, k=k)
//' @export
// [[Rcpp::export]]
IntegerVector whichd_run(
    SEXP x,
    IntegerVector k=0,
    bool na_pad = false) {

  impl::check_for_valid_k(x, k);

  switch (TYPEOF(x)) {
    case INTSXP: return impl::whichd_run_(as<IntegerVector>(x), k, na_pad);
    case REALSXP: return impl::whichd_run_(as<NumericVector>(x), k, na_pad);
    case STRSXP: return impl::whichd_run_(as<CharacterVector>(x), k, na_pad);
    case LGLSXP: return impl::whichd_run_(as<LogicalVector>(x), k, na_pad);
    case CPLXSXP: return impl::whichd_run_(as<ComplexVector>(x), k, na_pad);
  default: {
    warning(
      "Invalid SEXPTYPE %d (%s).\n",
      TYPEOF(x), type2name(x)
    );
    return 0;
  }
  }
}


//' Running which.max
//'
//' Running index of the (first or last) maximum.
//' @param x numeric (or integer) vector for which running whichrun is calculated.
//' @param k Running window size. By default window size equals \code{length(x)}. Allow varying window size specified by vector of \code{length(x)}
//' @param which specifies whether \code{"first"} or \code{"last"} index is returned.
//' @param na_pad logical (default \code{na_pad=FALSE}) - if \code{TRUE} first k-results will be filled by \code{NA}. If k is not specified na_pad=F by default.
//' @param na_rm logical (default \code{na_rm=TRUE}) - if \code{TRUE} \code{NA} are replaced by last observed minimum prior to element.
//' @return Numeric vector of length equals length of \code{x} containing running index of maximum in \code{k}-long window.
//' @examples
//' x1 <- c(1, 1, 2, 1, 1, 3, 1, 1, 3, 1, 1, 2, 3, 3, 3)
//' x2 <- c(2, 1, 1, NA, 3, 2, 1, NA, 1, NA, NA, NA, 1, 2, 1)
//' k  <- c(5, 1, 8, 1, 1, 15, 2, 5, 14, 2, 3, 7, 14, 13, 12)
//' whichmax_run(x1, which="first")
//' whichmax_run(x2, na_rm = TRUE, which="last")
//' whichmax_run(x2, k=3, na_rm = TRUE, which="last")
//' whichmax_run(x2 , k=k, na_rm = FALSE, which="first")
//' @export
// [[Rcpp::export]]
IntegerVector whichmax_run(
    NumericVector x,
    IntegerVector k = 0,
    std::string which = "last",
    bool na_rm = true,
    bool na_pad = false
){

  int n = x.size();
  impl::check_for_valid_k2(n, k);
  IntegerVector res;

  /* not windowed */
  if( ( k(0) <= 1 or k(0) == n ) and k.size() == 1 ){
    if( which == "last" )
      res = impl::cum_extr_l( x, na_rm );
    else
      res = impl::cum_extr_f( x, na_rm );

    /* windowed */
  } else if( k.size() == 1 ){
    if( which == "last" )
      res = impl::window_extr_l(x, k(0), na_rm);
    else
      res = impl::window_extr_f(x, k(0), na_rm);

    /* varying window size */
  } else {
    if( which == "last" )
      res = impl::window_extr2_l(x, k, na_rm);
    else
      res = impl::window_extr2_f(x, k, na_rm);
  }

  /* pad NA's */
  if(na_pad)
    std::fill(res.begin(), res.end() - n + k(0) - 1 , NA_REAL);

  return res;
}


//' Running which.min
//'
//' Running index of the (first or last) maximum.
//' @param x input logical vector where running whichrun is calculated.
//' @param k Running window size. By default window size equals \code{length(x)}. Allow varying window size specified by vector of \code{length(x)}
//' @param which specifies whether \code{"first"} or \code{"last"} index is returned.
//' @param na_pad logical (default \code{na_pad=FALSE}) - if \code{TRUE} first k-results will be filled by \code{NA}. If k is not specified na_pad=F by default.
//' @param na_rm logical (default \code{na_rm=TRUE}) - if \code{TRUE} \code{NA} are replaced by last observed minimum prior to element.
//' @return numeric vector of length equals length of \code{x} containing running index of maximum in \code{k}-long window.
//' @examples
//' set.seed(11)
//' x1 <- c(1, 1, 2, 1, 1, 3, 1, 1, 3, 1, 1, 2, 3, 3, 3)
//' x2 <- c(2, 1, 1, NA, 3, 2, 1, NA, 1, NA, NA, NA, 1, 2, 1)
//' k  <- c(5, 1, 8, 1, 1, 15, 2, 5, 14, 2, 3, 7, 14, 13, 12)
//' whichmin_run( x1 , which="first") # running index of minimum
//' whichmin_run(x1, which="last")
//' whichmin_run( x2, na_rm = TRUE , which="last" ) # running min-index ommiting NA
//' whichmin_run(x2 , k=3, na_rm = TRUE, which="first") # running min-index in 3-element window
//' whichmin_run( x2 , k = k , na_rm = TRUE, which = "last") # running min-index in varying window size
//' @export
// [[Rcpp::export]]
IntegerVector whichmin_run(
    NumericVector x,
    IntegerVector k = 0,
    std::string which = "last",
    bool na_rm = true,
    bool na_pad = false
){

  int n = x.size();
  impl::check_for_valid_k2(n, k);
  IntegerVector res;

  /* not windowed */
  if( ( k(0) <= 1 or k(0) == n ) and k.size() == 1 ){
    if( which == "last" )
      res = impl::cum_extr_l( -x, na_rm );
    else
      res = impl::cum_extr_f( -x, na_rm );

    /* windowed */
  } else if( k.size() == 1 ){
    if( which == "last" )
      res = impl::window_extr_l(-x, k(0), na_rm);
    else
      res = impl::window_extr_f(-x, k(0), na_rm);

    /* varying window size */
  } else {
    if( which == "last" )
      res = impl::window_extr2_l(-x, k, na_rm);
    else
      res = impl::window_extr2_f(-x, k, na_rm);
  }

  /* pad NA's */
  if(na_pad)
    std::fill(res.begin(), res.end() - n + k(0) - 1 , NA_REAL);

  return res;
}

