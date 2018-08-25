#include <Rcpp.h>
using namespace Rcpp;

namespace impl {

  template <int RTYPE>
  int  calc_actual_streak(const Vector<RTYPE>& x, int i, int i2, bool na_rm)
  {
    int j_f = IntegerVector::get_na();
    int cur_streak=1;

    // run for first finite
    for(int j = i; j >= i2 ; --j)
      if( Vector<RTYPE>::is_na(x(j)) ){
        if(!na_rm){
          return IntegerVector::get_na();}
      } else {
        j_f = j;
        break;
      }

    if( IntegerVector::is_na(j_f))
        return IntegerVector::get_na();


    for(int j = j_f; j >= i2 ; --j) {
      if( j < j_f ){
        if( x( j ) == x( j_f ) ){
          cur_streak += 1;
          j_f = j;
        } else if( !Vector<RTYPE>::is_na(x(j)) ){
          return cur_streak;
        } else {
          if(!na_rm)
            return cur_streak;
        }
      }
    }
    return cur_streak;
  }

  template <int RTYPE>
  IntegerVector streak_run_(const Vector<RTYPE>& x, IntegerVector k,  bool na_rm, bool na_pad)
  {

    int i2;
    int n = x.size();
    int nk = k.size();
    int cur_streak;
    int j_f = IntegerVector::get_na();
    IntegerVector res(n);

    /*  initial streak */
    for(int i=0; i < n ; i++)
    if ( Vector<RTYPE>::is_na( x(i) ) ){
      res(i) = NumericVector::get_na();
    } else {
      j_f = i;
      res(i) = cur_streak = 1;
      break;
    }


    if(nk==1 and ( k(0)==0 or k(0)==n ) ){
      /* streak run full */
      for(int i=j_f; i < n ; i++) {
        if( i > j_f){
          if( x( i ) == x( j_f ) ){
            cur_streak += 1;
            j_f = i;
          } else if( Vector<RTYPE>::is_na( x( i )  ) ) {
            if(!na_rm){
              cur_streak = 0;
              res( i ) = IntegerVector::get_na();
              continue;}
          } else {
            cur_streak = 1;
            j_f = i;
          }
        }
        res( i ) = cur_streak == 0 ? IntegerVector::get_na() : cur_streak;
      }
    } else {
    /* streak_run window */
      for(int i = 1; i < n; ++i) {
        if( nk == 1 ){
          i2 = window_index(i, k(0) );
        } else {
          i2 = window_index(i, k(i) );
        }
        res( i ) = calc_actual_streak(x, i, i2, na_rm);
      }
    }

    /* if padding with NA */
    if(na_pad)
      std::fill(res.begin(), res.end() - n + k(0) - 1 , NA_REAL);

    return res;
  }

}
