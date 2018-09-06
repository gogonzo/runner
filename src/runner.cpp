#include <Rcpp.h>
using namespace Rcpp;
#include "errors.h"
#include "which_extremes.h"
#include "whiches.h"

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

