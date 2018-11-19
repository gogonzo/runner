#include <Rcpp.h>
using namespace Rcpp;
#include "errors.h"
#include "which_run.h"

//' Running which-true function
//'
//' \code{whicht_run} checks \code{which} element has value TRUE within specified running window.
//' @param x input logical vector where running which-true is calculated.
//' @param k Running window size. By default window size equals \code{length(x)}. Allow varying window size specified by vector of \code{length(x)}
//' @param which specifies whether \code{"first"} or \code{"last"} index is returned.
//' @param na_pad logical (default \code{na_pad=FALSE}) - if \code{TRUE} first k-results will be filled by \code{NA}. If k is not specified na_pad=F by default.
//' @param na_rm logical (default \code{na_rm=TRUE}) - if \code{TRUE} \code{NA} are replaced by last observed minimum prior to element.
//' @param idx an optional integer vector containing indexes numbers of observation.
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
    bool na_pad = false,
    IntegerVector idx = 0
){
  int n = x.size(), i1, na1, true1;
  LogicalVector x2(n);
  IntegerVector res(n);
  IntegerVector na = impl::which_NA(x);

  // replace NA -> false
  for(int i=0;i<n;i++) if( x(i) == true ) x2(i) = true;

  // true idx
  IntegerVector w = impl::whicht_vector(x2);

  // CUMULATIVE
  if(idx.size()==1){
    if( (k.size()==1) & ((k(0) == 0)|(k(0)==n)) ){
      if(na_rm){
        if( which == "first" ){
          for(int i=0;i<n;i++){
            true1 = impl::first(w[w<=i]);
            res(i) = (true1 < n ) ? true1 + 1 : NA_INTEGER;
          }
        } else if( which == "last" ){
          for(int i=0;i<n;i++){
            true1 = impl::last(w[w<=i]);
            res(i) = (true1 >= 0) ? true1 + 1 : NA_INTEGER;
          }
        }
      } else {

        if( which == "first" ){
          for(int i=0;i<n;i++){
            true1  = impl::first(w[w<=i]);
            na1    = impl::first(na[na<=i]);
            res(i) = (true1 < na1) ? true1 + 1 : NA_INTEGER;
          }
        } else if( which == "last" ){
          for(int i=0;i<n;i++){
            true1  = impl::last(w[w<=i]);
            na1    = impl::last(na[na<=i]);
            res(i) = (true1 > na1) ? true1 + 1 : NA_INTEGER;
          }
        }
      }

      // CONST WINDOW
    } else if(k.size()==1){
      if(na_rm){
        if( which == "first" ){
          for(int i=0;i<n;i++){
            i1 = (i - k(0) + 1) < 0 ? 0 : i-k(0)+1;
            true1 = impl::first(w[(w<=i) & (w>=i1)]);
            res(i) = (true1 < n ) ? true1 + 1 : NA_INTEGER;
          }
        } else if(which == "last") {
          for(int i=0;i<n;i++){
            i1 = (i - k(0) + 1) < 0 ? 0 : i-k(0)+1;
            true1 = impl::last(w[(w<=i) & (w>=i1)]);
            res(i) = (true1 >= 0) ? true1 + 1 : IntegerVector::get_na();
          }
        }
      } else {
        if( which == "first" ){
          for(int i=0;i<n;i++){
            i1 = (i-k(0)+1) < 0 ? 0 : i-k(0)+1;
            na1   = impl::first(na[(na<=i) & (na>=i1)]);
            true1 = impl::first(w[(w<=i) & (w>=i1)]);
            res(i) = (true1 < na1) ? true1 + 1 : NA_INTEGER;
          }
        } else if(which == "last") {
          for(int i=0;i<n;i++){
            i1 = (i - k(0) + 1) < 0 ? 0 : i-k(0)+1;
            na1   = impl::last(na[(na<=i) & (na>=i1)]);
            true1 = impl::last(w[(w<=i) & (w>=i1)]);
            res(i) = (true1 > na1) ? true1 + 1 : NA_INTEGER;
          }
        }
      }

      // VARYING WINDOW
    } else if(k.size() > 1){
      if(na_rm){
        if( which == "first" ){
          for(int i=0;i<n;i++){
            i1 = (i - k(i) + 1) < 0 ? 0 : i-k(i)+1;
            true1 = impl::first(w[(w<=i) & (w>=i1)]);
            res(i) = (true1 < n ) ? true1 + 1 : NA_INTEGER;
          }
        } else if(which == "last") {
          for(int i=0;i<n;i++){
            i1 = (i - k(i) + 1) < 0 ? 0 : i-k(i)+1;
            true1 = impl::last(w[(w<=i) & (w>=i1)]);
            res(i) = (true1 >= 0) ? true1 + 1 : NA_INTEGER;
          }
        }
      } else {
        if( which == "first" ){
          for(int i=0;i<n;i++){
            i1 = (i-k(i)+1) < 0 ? 0 : i-k(i)+1;
            na1   = impl::first(na[(na<=i) & (na>=i1)]);
            true1 = impl::first(w[(w<=i) & (w>=i1)]);
            res(i) = (true1 < na1) ? true1 + 1 : NA_INTEGER;
          }
        } else if(which == "last") {
          for(int i=0;i<n;i++){
            i1 = (i - k(i) + 1) < 0 ? 0 : i-k(i)+1;
            na1   = impl::last(na[(na<=i) & (na>=i1)]);
            true1 = impl::last(w[(w<=i) & (w>=i1)]);
            res(i) = (true1 > na1) ? true1 + 1 : NA_INTEGER;
          }
        }
      }
    }
  } else if(idx.size()==n){
    if( (k.size()==1) & (k(0) == 0) ){
      if(na_rm){
        if( which == "first" ){
          for(int i=0;i<n;i++){
            true1 = impl::first(w[w<=i]);
            res(i) = (true1 < n ) ? true1 + 1 : NA_INTEGER;
          }
        } else if( which == "last" ){
          for(int i=0;i<n;i++){
            true1 = impl::last(w[w<=i]);
            res(i) = (true1 >= 0) ? true1 + 1 : NA_INTEGER;
          }
        }
      } else {

        if( which == "first" ){
          for(int i=0;i<n;i++){
            true1  = impl::first(w[w<=i]);
            na1    = impl::first(na[na<=i]);
            res(i) = (true1 < na1) ? true1 + 1 : NA_INTEGER;
          }
        } else if( which == "last" ){
          for(int i=0;i<n;i++){
            true1  = impl::last(w[w<=i]);
            na1    = impl::last(na[na<=i]);
            res(i) = (true1 > na1) ? true1 + 1 : NA_INTEGER;
          }
        }
      }

      // CONST WINDOW
    } else if(k.size()==1){
      if(na_rm){
        if( which == "first" ){
          for(int i=0;i<n;i++){
            i1 = impl::get_window_start(i,k(0), idx );
            true1 = impl::first(w[(w<=i) & (w>=i1)]);
            res(i) = (true1 < n) ? true1 + 1 : NA_INTEGER;
          }
        } else if(which == "last") {
          for(int i=0;i<n;i++){
            i1 = impl::get_window_start(i,k(0), idx );
            true1 = impl::last(w[(w<=i) & (w>=i1)]);
            res(i) = (true1 >= 0) ? true1 + 1 : NA_INTEGER;
          }
        }
      } else {
        if( which == "first" ){
          for(int i=0;i<n;i++){
            i1 = impl::get_window_start(i,k(0), idx );
            na1   = impl::first(na[(na<=i) & (na>=i1)]);
            true1 = impl::first(w[(w<=i) & (w>=i1)]);
            res(i) = (true1 < na1) ? true1 + 1 : NA_INTEGER;
          }
        } else if(which == "last") {
          for(int i=0;i<n;i++){
            i1 = impl::get_window_start(i,k(0), idx );
            na1   = impl::last(na[(na<=i) & (na>=i1)]);
            true1 = impl::last(w[(w<=i) & (w>=i1)]);
            res(i) = (true1 > na1) ? true1 + 1 : NA_INTEGER;
          }
        }
      }

      // VARYING WINDOW
    } else if(k.size() > 1){
      if(na_rm){
        if( which == "first" ){
          for(int i=0;i<n;i++){
            i1 = impl::get_window_start(i,k(i), idx );
            true1 = impl::first(w[(w<=i) & (w>=i1)]);
            res(i) = (true1 < n) ? true1 + 1 : NA_INTEGER;
          }
        } else if(which == "last") {
          for(int i=0;i<n;i++){
            i1 = impl::get_window_start(i,k(i), idx );
            true1 = impl::last(w[(w<=i) & (w>=i1)]);
            res(i) = (true1 >= 0) ? true1 + 1 : NA_INTEGER;
          }
        }
      } else {
        if( which == "first" ){
          for(int i=0;i<n;i++){
            i1 = impl::get_window_start(i,k(i), idx );
            na1   = impl::first(na[(na<=i) & (na>=i1)]);
            true1 = impl::first(w[(w<=i) & (w>=i1)]);
            res(i) = (true1 < na1) ? true1 + 1 : NA_INTEGER;
          }
        } else if(which == "last") {
          for(int i=0;i<n;i++){
            i1 = impl::get_window_start(i,k(i), idx );
            na1   = impl::last(na[(na<=i) & (na>=i1)]);
            true1 = impl::last(w[(w<=i) & (w>=i1)]);
            res(i) = (true1 > na1) ? true1 + 1 : NA_INTEGER;
          }
        }
      }
    }
  }
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
    case INTSXP:  return impl::whichd_run_(as<IntegerVector>(x), k, na_pad);
    case REALSXP: return impl::whichd_run_(as<NumericVector>(x), k, na_pad);
    case STRSXP:  return impl::whichd_run_(as<CharacterVector>(x), k, na_pad);
    case LGLSXP:  return impl::whichd_run_(as<LogicalVector>(x), k, na_pad);
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
