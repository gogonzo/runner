#include <Rcpp.h>
using namespace Rcpp;

namespace impl {

  NumericVector calc_sum_window( NumericVector x, NumericVector res, int k){
    int n = x.size();
    double first_sum = NumericVector::get_na();

    /* sum on window end*/
    for(int i = 0; i < n; i++){
      if( ISNAN( first_sum ) ){
        first_sum = x( i );
      } else if( !ISNAN( x( i ) )) {
        first_sum += x( i );
      }
      res( i ) = first_sum;
    }

    /* minus sum on window biginning*/
    if(k > 0 and k < n){
      first_sum = 0;
      for(int i=0; i < n-k; i++){
        if( NumericVector::is_na( first_sum )){
          first_sum = x( i );
        } else if( !NumericVector::is_na( x( i ) )) {
          first_sum += x( i );
        }
        res( i + k ) -= first_sum;
      }
    }


    return res;
  }
  IntegerVector count_na_window(NumericVector x, int k){
    int n = x.size();
    int count = 0 ;
    IntegerVector counts(n);

    for(int i = 0; i < n; i++ ){
      if(ISNAN( x( i ) ))
        count += 1;
      counts( i ) = count;
    }

    if(k > 0 and k < n){
      count = 0;
      for(int i = 0;  i < n-k; i++){
        if( ISNAN( x(i) ) )
          count += 1;
        counts( i + k ) = counts( i + k ) - count;
      }
    }
    return counts;
  }

  IntegerVector count_nonna_window(NumericVector x, int k){
    int n = x.size();
    int count = 0 ;
    IntegerVector counts(n);
    for(int i = 0; i < n; i++ ){
      if(!ISNAN( x( i ) ))
        count += 1;
      counts( i ) = count;
    }

    if(k > 0 and k < n){
      count = 0;
      for(int i = 0;  i < n-k; i++){
        if( !ISNAN( x(i) ) )
          count += 1;
        counts( i + k ) = counts( i + k ) - count;
      }
    }
    return counts;
  }

  NumericVector calc_sum_window2( NumericVector x, NumericVector res, IntegerVector k){
    int n = x.size();
    int i1;
    double cur_sum;

    for(int i = 0; i < n; i++){
      cur_sum = NumericVector::get_na();
      i1 = impl::window_index( i, k( i ) );
      for(int j = i1; j <= i ; ++j){
        if( ISNAN( cur_sum ) ){
          cur_sum = x( i );
        } else if( !ISNAN( x( i ) )) {
          cur_sum += x( i );
        }
      }
      res( i ) = cur_sum;
    }
    return res;
  }
  IntegerVector count_na_window2( NumericVector x, IntegerVector k){
    int n = x.size();
    int i1;
    int count;
    IntegerVector counts(n);

    for(int i = 0; i < n; i++){
      count = 0;
      i1 = impl::window_index( i, k( i ) );
      for(int j = i1; j <= i ; ++j){
        if(ISNAN( x( j ) ))
          count += 1;
      }
      counts( i ) = count;
    }
    return counts;
  }
}
