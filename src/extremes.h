#include <Rcpp.h>
using namespace Rcpp;

namespace impl {

  double calc_max(double x, double cur_max){
    if (x > cur_max){
      return x;
    } else if(x <= cur_max){
      return cur_max;
    } else if (NumericVector::is_na(cur_max)){
      return x;
    } else {
      return cur_max;
    }
  }

  NumericVector cum_max(NumericVector x, bool na_rm){
    int n = x.size();
    double cur_max = NumericVector::get_na();
    NumericVector res(n);

    for(int i = 0; i < x.size(); i ++){

      // handling na
      if( !na_rm and NumericVector::is_na( x(i) ) ){
        std::fill(res.begin() + i, res.end(), NumericVector::get_na() );
        break;
      }

      cur_max = calc_max( x( i ) , cur_max );
      res(i) = cur_max;
    }

    return res;
  }

  NumericVector window_max2( NumericVector x, IntegerVector k, bool na_rm ){

    int n = x.size();
    int i1;
    double cur_max;
    NumericVector res(n);

    for(int i = 0; i < n; i++){

      cur_max = NumericVector::get_na();
      i1 = impl::window_index( i, k( i ) );

      for(int j = i1; j <= i ; ++j){
        if(!na_rm and NumericVector::is_na(x(j)) ){
          cur_max = NumericVector::get_na();
          break;
        }
        cur_max = calc_max( x( j ), cur_max);
      }
      res( i ) = cur_max;
    }
    return res;
  }

  NumericVector window_max(NumericVector x, int k, bool na_rm){
    int n = x.size();
    int i1;
    double cur_max;
    NumericVector res(n);

    for(int i = 0; i < n; i++){

      cur_max = NumericVector::get_na();
      i1 = impl::window_index( i, k );
      for(int j = i1; j <= i ; ++j){
        if(!na_rm and NumericVector::is_na(x(j)) ){
          cur_max = NumericVector::get_na();
          break;
        }
        cur_max = calc_max( x( j ), cur_max);
      }
      res( i ) = cur_max;
    }
    return res;
  }



  double calc_min(double x, double cur_min){
    if (x < cur_min){
      return x;
    } else if(x >=cur_min ){
      return cur_min;
    } else if (NumericVector::is_na(cur_min)){
      return x;
    } else {
      return cur_min;
    }
  }

  NumericVector cum_min(NumericVector x, bool na_rm ){
    int n = x.size();
    double cur_min = NumericVector::get_na();
    NumericVector res(n);

    for(int i = 0; i < x.size(); i ++){
      // handling NAs
      if( !na_rm and NumericVector::is_na( x(i) ) ){
        std::fill(res.begin() + i, res.end(), NumericVector::get_na() );
        break;
      }

      cur_min = calc_min( x( i ) , cur_min );
      res(i) = cur_min;
    }
    return res;
  }

  NumericVector window_min2(NumericVector x, IntegerVector k, bool na_rm){
    int n = x.size();
    int i1;
    double cur_min;
    NumericVector res(n);

    for(int i = 0; i < n; i++){

      cur_min = NumericVector::get_na();
      i1 = impl::window_index( i, k( i ) );
      for(int j = i1; j <= i ; ++j){
        if(!na_rm and NumericVector::is_na(x(j)) ){
          cur_min = NumericVector::get_na();
          break;
        }

        cur_min = calc_min( x( j ), cur_min);
      }
      res( i ) = cur_min;
    }


    return res;
  }

  NumericVector window_min(NumericVector x, int k, bool na_rm){
    int n = x.size();
    int i1;
    double cur_min;
    NumericVector res(n);

    for(int i = 0; i < n; i++){

      cur_min = NumericVector::get_na( );
      i1 = impl::window_index( i, k );

      for(int j = i1; j <= i ; ++j){
        if(!na_rm and NumericVector::is_na(x(j)) ){
          cur_min = NumericVector::get_na();
          break;
        }
        cur_min = calc_min( x( j ) , cur_min);
      }
      res( i ) = cur_min;
    }
    return res;
  }

}
