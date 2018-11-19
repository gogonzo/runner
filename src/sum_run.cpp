#include <Rcpp.h>
using namespace Rcpp;
#include "sum_run.h"

//' Running mean
//'
//'
//' @param x vector of any type on which running mean is calculated
//' @param k running window size. Not yet implemented.
//' @param na_rm logical (default \code{na_rm=TRUE}) - if \code{TRUE} mean is calulating excluding \code{NA}.
//' @param na_pad logical (default \code{na_pad=FALSE}) - if \code{TRUE} first k-results will be filled by \code{NA}. If k is not specified na_pad=F by default.
//' @param idx an optional integer vector containing idx numbers of observation.
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
    bool na_pad = false,
    IntegerVector idx=1
){

  int n = x.size(), first_na = -1,fnn, i1;
  double x1, nas1, non1;

  NumericVector res(n), sums(n);
  NumericVector nas = impl::calc_na_vector( x );
  NumericVector non = impl::calc_nonna_vector( x );

  // CHECK FOR POSSIBLE ERRORS
  if( (k.size() != n) & (k.size() > 1) ){
    stop("length of k and length x differs. k=0 and k=length(x) only allowed");
  } else if( Rcpp::any(Rcpp::is_na(k)) ){
    stop("Function doesn't accept NA values in k vector");
  }


  // RUN FOR FIRST NON-NA
  for(int i = 0; i < n; i++)
    if(!ISNAN(x(i))){
      fnn = i;
      std::fill(res.begin(), res.end() - n + i , NA_REAL);
      break;
    }

    // NORMAL CUMMEAN ------------
    if( (k.size() == 1) & (k(0) == 0) ){
      if(na_rm){
        res = impl::calc_sum_vector( x ) / non;
      } else {
        res = impl::calc_sum_vector( x );
        first_na = impl::first_na_index( x );
        if( first_na != -1)
          std::fill(res.begin() + first_na, res.end(), NA_REAL);
      }

      // CONST. WINDOW ------------
    } else if( (k.size()==1) & (idx.size()==1) ){
      sums = impl::calc_sum_vector0( x );
      for(int i = fnn; i<n; i++){

        if( i >= k(0) ){
          i1   = i - k(0);
          x1   = sums( i1 );
          nas1 = nas( i1 );
          non1 = non( i1 );
        } else {
          i1   = 0;
          x1   = 0.0;
          nas1 = 0.0;
          non1 = 0.0;
        }

        res( i ) = ( sums(i) - x1 ) / (non(i) - non1);
        if( ( nas(i) - nas1 )==k(0) ){
          res(i) = NA_REAL;
          continue;
        }
        if(!na_rm)
          if( nas(i) > nas1 )
            res(i) = NA_REAL;
      }


      // VARYING WINDOW -----------
    } else if( (k.size() > 1)  & (idx.size()==1) ){
      sums = impl::calc_sum_vector0( x );
      for(int i = fnn; i<n; i++){
        if( i >= k(i) ){
          i1   = i - k(i);
          x1   = sums( i1 );
          nas1 = nas( i1 );
          non1 = non( i1 );
        } else {
          i1   = 0;
          x1   = 0.0;
          nas1 = 0.0;
          non1 = 0.0;
        }

        res( i ) = ( sums(i) - x1 ) / (non(i) - non1);
        if( ( nas(i) - nas1 )==k(i) ){
          res(i) = NA_REAL;
          continue;
        }
        if(!na_rm)
          if( nas(i) > nas1 )
            res(i) = NA_REAL;
      }

      // IDX WINDOW -----------
    } else if( (k.size() == 1) & (idx.size() > 1) ){
      sums = impl::calc_sum_vector0( x );
      for(int i = fnn; i<n; i++){
        for(int j=i; j>=0; j--)
          if( (idx(i) - idx(j)) >= k(0) ){
            i1   = j;
            x1   = sums( j );
            nas1 = nas( j );
            non1 = non( j );
            break;
          } else if(j == 0){
            i1   = 0;
            x1   = 0.0;
            nas1 = 0.0;
            non1 = 0.0;
          }
          res( i ) = ( sums(i) - x1 ) / (non(i) - non1);
          if( ( nas(i) - nas1 )==k(0) ){
            res(i) = NA_REAL;
            continue;
          }
          if(!na_rm)
            if( nas(i) > nas1 )
              res(i) = NA_REAL;
      }

      // IDX VARYING WINDOW -----------
    } else if( (k.size() > 1) & (idx.size() > 1) ) {
      sums = impl::calc_sum_vector0( x );
      for(int i = fnn; i<n; i++){
        for(int j=i; j>=0; j--)
          if( (idx(i) - idx(j)) >= k(i) ){
            i1   = j;
            x1   = sums( j );
            nas1 = nas( j );
            non1 = non( j );
            break;
          } else if(j == 0){
            i1   = 0;
            x1   = 0.0;
            nas1 = 0.0;
            non1 = 0.0;
          }
          res( i ) = ( sums(i) - x1 ) / (non(i) - non1);
          if( ( nas(i) - nas1 )==k(i) ){
            res(i) = NA_REAL;
            continue;
          }
          if(!na_rm)
            if( nas(i) > nas1 )
              res(i) = NA_REAL;
      }
    }

    /* pad NA at from 0:(k-1) */
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
//' @param idx an optional integer vector containing idx numbers of observation.
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
    bool na_pad = false,
    IntegerVector idx=1) {

  int n = x.size(), i1, fnn, first_na = -1;
  double x1, nas1;
  NumericVector res(n), sums(n), nas = impl::calc_na_vector( x );


  // CHECK FOR POSSIBLE ERRORS
  if( (k.size() != n) and (k.size() > 1) ){
    stop("length of k and length x differs. k=0 and k=length(x) only allowed");
  } else if( Rcpp::any(Rcpp::is_na(k)) ){
    stop("Function doesn't accept NA values in k vector");
  }


  // RUN FOR FIRST NON-NA
  for(int i = 0; i < n; i++)
    if(!ISNAN(x(i))){
      fnn = i;
      std::fill(res.begin(), res.end() - n + i , NA_REAL);
      break;
    }

  // NORMAL CUMSUM ------------
  if( (k.size() == 1) & ((k(0) == 0)|(k(0)==n)) ){
    if(na_rm){
      res = impl::calc_sum_vector( x );
    } else {
      res = impl::calc_sum_vector( x );
      first_na = impl::first_na_index( x );
      if( first_na != -1)
        std::fill(res.begin() + first_na, res.end(), NA_REAL);
    }

  // CONST. WINDOW ------------
  } else if( (k.size()==1) & (idx.size()==1) ){
    sums = impl::calc_sum_vector0( x );
    for(int i = fnn; i<n; i++){

      if( i >= k(0) ){
        i1   = i - k(0);
        x1   = sums( i1 );
        nas1 = nas( i1 );
      } else {
        i1   = 0;
        x1   = 0.0;
        nas1 = 0.0;
      }

      res( i ) = sums(i) - x1;
      if( ( nas(i) - nas1 )==k(0) ){
        res(i) = NA_REAL;
        continue;
      }
      if(!na_rm)
        if( nas(i) > nas1 )
          res(i) = NA_REAL;
    }


  // VARYING WINDOW -----------
  } else if( (k.size() > 1)  & (idx.size()==1) ){
    sums = impl::calc_sum_vector0( x );
    for(int i = fnn; i<n; i++){
      if( i >= k(i) ){
        i1   = i - k(i);
        x1   = sums( i1 );
        nas1 = nas( i1 );
      } else {
        i1   = 0;
        x1   = 0.0;
        nas1 = 0.0;
      }

      res( i ) = ( sums(i) - x1 );
      if( ( nas(i) - nas1 )==k(i) ){
        res(i) = NA_REAL;
        continue;
      }
      if(!na_rm)
        if( nas(i) > nas1 )
          res(i) = NA_REAL;
    }

  // IDX WINDOW -----------
  } else if( (k.size() == 1) & (idx.size()>1) ){
    sums = impl::calc_sum_vector0( x );
    for(int i = fnn; i<n; i++){
      for(int j=i; j>=0; j--)
        if( (idx(i) - idx(j)) >= k(0) ){
          x1   = sums( j );
          nas1 = nas( j );
          break;
        } else if(j == 0){
          x1   = 0.0;
          nas1 = 0.0;
        }
      res( i ) = sums(i) - x1;
      if( ( nas(i) - nas1 )==k(0) ){
        res(i) = NA_REAL;
        continue;
      }
      if(!na_rm)
        if( nas(i) > nas1 )
          res(i) = NA_REAL;
    }

  // IDX VARYING WINDOW -----------
  } else if( (k.size() > 1) & (idx.size()>1) ) {
    sums = impl::calc_sum_vector0( x );
    for(int i = fnn; i<n; i++){
      for(int j=i; j>=0; j--)
        if( (idx(i) - idx(j)) >= k(i) ){
          x1   = sums( j );
          nas1 = nas( j );
          break;
        } else if(j == 0){
          x1   = 0.0;
          nas1 = 0.0;
        }
      res( i ) = ( sums(i) - x1 );
      if( ( nas(i) - nas1 )==k(i) ){
        res(i) = NA_REAL;
        continue;
      }
      if(!na_rm)
        if( nas(i) > nas1 )
          res(i) = NA_REAL;
    }
  }

  /* pad NA at from 0:(k-1) */
  if(na_pad)
    std::fill(res.begin(), res.end() - n + k(0) - 1 , NA_REAL);

  return res;
}
