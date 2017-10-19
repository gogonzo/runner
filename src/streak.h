#include <Rcpp.h>
using namespace Rcpp;

namespace impl {

  template <int RTYPE>
  int  calc_actual_streak(const Vector<RTYPE>& x, int i, int i2)
  {
    int cur_streak;

    for(int j = i; j >= i2 ; --j) {
      if( j == i ){
        if(!Vector<RTYPE>::is_na( x( j ) )){
          cur_streak = 1;
        } else {
          return IntegerVector::get_na();
        }
      } else {
        if( x( j ) == x( j + 1 ) ){
          cur_streak += 1;
        } else if( Vector<RTYPE>::is_na( x( j )) ) {
          return IntegerVector::get_na();
        } else {
           return cur_streak;
        }
      }
    }
    return cur_streak;
  }

  template <int RTYPE>
  IntegerVector streak_run_(const Vector<RTYPE>& x, IntegerVector k, bool na_pad)
  {

    int i2;
    int n = x.size();
    int nk = k.size();
    int cur_streak;
    IntegerVector res(n);

    /*  initial streak */
    if ( Vector<RTYPE>::is_na( x(0) ) ){
      res(0) = cur_streak = NumericVector::get_na();
    } else {
      res(0) = cur_streak = 1;
    }


    if(nk==1 and ( k(0)==0 or k(0)==n ) ){
      /* streak run full */
      for(int i=1; i < n ; i++) {
        if( Vector<RTYPE>::is_na( x( i ) )) {
          cur_streak = IntegerVector::get_na();
        } else if( x( i ) == x( i - 1 ) ){
          cur_streak += 1;
        } else {
          cur_streak = 1;
        }
        res( i ) = cur_streak;
      }
    } else {
    /* streak_run window */
      for(int i = 1; i < n; ++i) {
        if( nk == 1 ){
          i2 = window_index(i, k(0) );
        } else {
          i2 = window_index(i, k(i) );
        }
        res( i ) = calc_actual_streak(x, i, i2);
      }
    }

    /* if padding with NA */
    if(na_pad)
      std::fill(res.begin(), res.end() - n + k(0) - 1 , NA_REAL);

    return res;
  }

}
