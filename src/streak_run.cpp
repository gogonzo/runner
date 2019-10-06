#include <Rcpp.h>
using namespace Rcpp;
#include "streak_run.h"

//' Running streak length
//'
//' Calculates running series of consecutive elements
//' @inheritParams runner
//' @inheritParams sum_run
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
    IntegerVector k = 0,
    IntegerVector lag = 0,
    bool na_rm = true,
    bool na_pad = false,
    IntegerVector idx = IntegerVector(0)) {

  int n = Rf_length(x);

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
  } else if (lag.size() == 1 & lag(0) >= n & idx.size() == 0) {
    warning("lag value is greater than length of x");
  }

  if (idx.size() == 0) {
    switch (TYPEOF(x)) {
    case INTSXP: return streak::streak_run1(as<IntegerVector>(x),   k, lag, na_rm, na_pad);
    case REALSXP: return streak::streak_run1(as<NumericVector>(x),  k, lag, na_rm, na_pad);
    case STRSXP: return streak::streak_run1(as<CharacterVector>(x), k, lag, na_rm, na_pad);
    case LGLSXP: return streak::streak_run1(as<LogicalVector>(x),   k, lag, na_rm, na_pad);
    case CPLXSXP: return streak::streak_run1(as<ComplexVector>(x),  k, lag, na_rm, na_pad);
    default: {
      stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
    }
    }

  } else {
    switch (TYPEOF(x)) {
    case INTSXP: return streak::streak_run2(as<IntegerVector>(x),   k, lag, na_rm, na_pad, idx);
    case REALSXP: return streak::streak_run2(as<NumericVector>(x),  k, lag, na_rm, na_pad, idx);
    case STRSXP: return streak::streak_run2(as<CharacterVector>(x), k, lag, na_rm, na_pad, idx);
    case LGLSXP: return streak::streak_run2(as<LogicalVector>(x),   k, lag, na_rm, na_pad, idx);
    case CPLXSXP: return streak::streak_run2(as<ComplexVector>(x),  k, lag, na_rm, na_pad, idx);
    default: {
        stop("Invalid data type - only integer, numeric, character, factor, date, logical, complex vectors are possible.");
    }
    }


  }

}
