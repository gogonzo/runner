#include <Rcpp.h>
using namespace Rcpp;

namespace impl {

  IntegerVector cum_whicht_l(LogicalVector x, IntegerVector res, bool na_rm ){
    int n = x.size();


    if(  !na_rm and LogicalVector::is_na( x(0) ) ){
      std::fill(res.begin(), res.end(), IntegerVector::get_na() );
      return res;
    } else if ( x(0)==true ){
      res(0) = 1;
    } else {
      res(0) = LogicalVector::get_na();
    }

    for(int i = 1; i < n; ++i) {
      if( !na_rm and LogicalVector::is_na( x(i) ) ){
        std::fill(res.begin() + i, res.end(), IntegerVector::get_na() );
        return res;
      }

      if( x(i)==true  ){
        res(i) =  i + 1;
      } else {
        res( i ) = res(i - 1 );
      }
    }

    return res;
  }

  IntegerVector cum_whicht_f(LogicalVector x, IntegerVector res, bool na_rm ){
    int n = x.size();

    if( !na_rm and LogicalVector::is_na( x(0) ) ){
      std::fill(res.begin(), res.end(), IntegerVector::get_na() );
      return res;
    } else if ( x(0)==true ){
      std::fill(res.begin(), res.end(), 1 );
      return res;
    } else {
      res(0) = LogicalVector::get_na();
    }

    for(int i = 1; i < n; ++i) {
      if( !na_rm and LogicalVector::is_na( x(i) ) ){
        std::fill(res.begin() + i, res.end(), IntegerVector::get_na() );
        return res;
      }

      if( x(i)==true    ){
        std::fill(res.begin() + i, res.end(), i + 1 );
        break;
      } else {
        res(i) = LogicalVector::get_na();
      }
    }

    return res;
  }

  IntegerVector window_whicht_l(LogicalVector x, IntegerVector res, int k, bool na_rm ){
    int n = x.size();
    int i1;
    int idx;

    for(int i = 0; i < n; i++){
      i1 = impl::window_index( i, k );
      idx = IntegerVector::get_na();
      for(int j = i; j >= i1 ; --j){
        if( !na_rm and LogicalVector::is_na( x(j) ) ){
          idx = IntegerVector::get_na();
          break;
        }
        if( x( j )==true ){
          idx = j + 1;
          break;
        }
      }
      res( i ) = idx;
    }

    return res;
  }

  IntegerVector window_whicht_f(LogicalVector x, IntegerVector res, int k, bool na_rm ){
    int n = x.size();
    int i1;
    int idx;

    for(int i = 0; i < n; i++){
      i1 = impl::window_index( i, k );
      idx = IntegerVector::get_na();

      for(int j = i1; j <= i ; ++j){
        if( !na_rm and LogicalVector::is_na( x(j) ) ){
          idx = IntegerVector::get_na();
          break;
        }

        if( x( j )==true ){
          idx = j + 1;
          break;
        }
      }
      res( i ) = idx;
    }

    return res;
  }

  IntegerVector window_whicht2_l(LogicalVector x, IntegerVector res, IntegerVector k, bool na_rm ){
    int n = x.size();
    int i1;
    int idx;

    for(int i = 0; i < n; i++){
      i1 = impl::window_index( i, k(i) );
      idx = IntegerVector::get_na();
      for(int j = i; j >= i1 ; --j){
        if( !na_rm and LogicalVector::is_na( x(j) ) ){
          idx = IntegerVector::get_na();
          break;
        }
        if( x( j )==true ){
          idx = j + 1;
          break;
        }
      }
      res( i ) = idx;
    }

    return res;
  }

  IntegerVector window_whicht2_f(LogicalVector x, IntegerVector res, IntegerVector k, bool na_rm ){
    int n = x.size();
    int i1;
    int idx;

    for(int i = 0; i < n; i++){
      i1 = impl::window_index( i, k(i) );
      idx = IntegerVector::get_na();

      for(int j = i1; j <= i ; ++j){
        if( !na_rm and LogicalVector::is_na( x(j) ) ){
          idx = IntegerVector::get_na();
          break;
        }

        if( x( j )==true ){
          idx = j + 1;
          break;
        }
      }
      res( i ) = idx;
    }

    return res;
  }

}

