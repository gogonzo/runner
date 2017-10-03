#include <Rcpp.h>
using namespace Rcpp;

namespace impl {

  int window_index(int i, int k){
    int begin;
    if( (i - k + 1) < 0) begin = 0; else begin = i - k + 1;
    return begin;
  }

  // function comparing values
  double calc_min(double x, double cur_min){
    if (x < cur_min){
      return x;
    } else if (NumericVector::is_na(cur_min)){
      return x;
    } else {
      return cur_min;
    }
  }

  double calc_max(double x, double cur_max){
    if (x < cur_max){
      return x;
    } else if (NumericVector::is_na(cur_max)){
      return x;
    } else {
      return cur_max;
    }
  }

  template <int RTYPE>
  IntegerVector streak_run_impl(const Vector<RTYPE>& x, IntegerVector k, bool na_pad)
  {

    int i2;
    int cur_streak = 0;
    int n = x.size();
    IntegerVector res(n);
    if( k(0) == 0 ) k(0) = n;

    if ( Vector<RTYPE>::is_na( x(0) ) ){
      res(0) = NumericVector::get_na();
    } else {
      res(0) = 1;
    }

    for(int i = 1; i < n; ++i) {

      if( k.size() == 1 ){
        i2 = window_index(i, k(0) );
      } else {
        i2 = window_index(i, k(i) );
      }


      for(int j = i; j >= i2 ; --j) {
        if( j == i ){
          // first iteration gives i=1 or na
          if(Vector<RTYPE>::is_na( x( j ) )){
            cur_streak = NumericVector::get_na();
            break;
          } else {
            cur_streak = 1;
          }

        } else if( x( j ) == x( j + 1 ) ){
          cur_streak += 1;
        } else {
          break;
        }
      }
      res( i ) = cur_streak;
    }

    if(na_pad)
      std::fill(res.begin(), res.end() - n + k(0) - 1 , NA_REAL);

    return res;
  }
}
