#include <Rcpp.h>
using namespace Rcpp;
#include "streak_run.h"

//' Running streak length
//'
//' Calculates running series of consecutive elements
//' @param x vector of any type where running streak is calculated
//' @param k running window size. By default window size equals \code{length(x)}. Allow varying window size specified by vector of \code{length(x)}
//' @param na_rm logical (default \code{na_rm=TRUE}) - if \code{TRUE} \code{NA} are replaced by last observed streak prior to element.
//' @param na_pad logical (default \code{na_pad=FALSE}) - if \code{TRUE} first k-results will be filled by \code{NA}. If k is not specified na_pad=F by default.
//' @param idx an optional integer vector containing indexes numbers of observation.
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
    bool na_pad = false,
    IntegerVector idx=1) {

  int n = Rf_length(x);

  if( k(0) == 0 ){
    k(0) = n;
  } else if(k.size() != n and k.size() > 1){
    stop("length of k and length x differs. k=0 and k=length(x) only allowed");
  } else if( Rcpp::any(Rcpp::is_na(k)) ){
    stop("Function doesn't accept NA values in k vector");
  }

  if( idx.size() == 1){
    switch (TYPEOF(x)) {
    case INTSXP: return streak::streak_run1(as<IntegerVector>(x), k, na_rm, na_pad);
    case REALSXP: return streak::streak_run1(as<NumericVector>(x), k, na_rm, na_pad);
    case STRSXP: return streak::streak_run1(as<CharacterVector>(x), k, na_rm, na_pad);
    case LGLSXP: return streak::streak_run1(as<LogicalVector>(x), k, na_rm, na_pad);
    case CPLXSXP: return streak::streak_run1(as<ComplexVector>(x), k, na_rm, na_pad);
    default: {
      warning(
        "Invalid SEXPTYPE %d (%s).\n",
        TYPEOF(x), type2name(x)
      );
      return 0;
    }
    }

  } else {
    switch (TYPEOF(x)) {
    case INTSXP: return streak::streak_run2(as<IntegerVector>(x), k, na_rm, na_pad, idx);
    case REALSXP: return streak::streak_run2(as<NumericVector>(x), k, na_rm, na_pad, idx);
    case STRSXP: return streak::streak_run2(as<CharacterVector>(x), k, na_rm, na_pad, idx);
    case LGLSXP: return streak::streak_run2(as<LogicalVector>(x), k, na_rm, na_pad, idx);
    case CPLXSXP: return streak::streak_run2(as<ComplexVector>(x), k, na_rm, na_pad, idx);
    default: {
      warning(
        "Invalid SEXPTYPE %d (%s).\n",
        TYPEOF(x), type2name(x)
      );
      return 0;
    }
    }


  }

}
