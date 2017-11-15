#include <Rcpp.h>
using namespace Rcpp;

namespace impl {

  template <int RTYPE>
  Vector<RTYPE> fill_run_impl(const Vector<RTYPE>& x, bool run_for_first, bool only_within)
  {

    int n = x.size();
    Vector<RTYPE> res(n);

    /* if all are NA throw warning */
    int next_non_na = run_for_non_na(x,0);
    if(next_non_na < 0){
      warning("All x values are NA");
      return x;
    }

    /* run_for_first option fills prior to first non-na */
    if( run_for_first and next_non_na > 0 ){
      for(int i=0; i < next_non_na; i++)
        res( i ) = x( next_non_na );
    } else {
      for(int i=0; i < next_non_na; i++)
        res( i ) = x( i );
    }

    /* if first_non_na is a the end*/
    if( next_non_na == n)
      return res;

    /* actual function - if na fill with previous */
    if(!only_within){
      for( int i = next_non_na; i < n; ++i ) {
        if( !Vector<RTYPE>::is_na( x(i) ) ) {
          res( i ) = x( i );
        } else {
          res( i ) = res( i - 1 );
        }
      }

    /* only_within */
    } else {
      for( int i = next_non_na; i < n; ++i ) {
        if( !Vector<RTYPE>::is_na( x(i) ) ) {
          res( i ) = x( i );
        } else {
          next_non_na = run_for_non_na(x, i);

          // if no finite till the end
          if(next_non_na==-1){
            for(int j = i; j < n; j++)
              res( j ) = Vector<RTYPE>::get_na();
            return res;
          }

          // if two non-na ends equals
          if( x( i-1 ) == x( next_non_na) ){
            for(int j = i; j<next_non_na; j++)
              res( j ) = res( i - 1 );
            i = next_non_na-1;

          // if two non-na ends differs
          } else {
            for(int j = i; j<next_non_na; j++)
              res( j ) = Vector<RTYPE>::get_na();
            i = next_non_na-1;

          }
        }
      }
    }

    return res;
  }

  template <int RTYPE>
  Rcpp::List window_to_list(const Vector<RTYPE>& x, IntegerVector k)
  {
    int n = x.size();
    IntegerVector idx;
    List res(n);

    impl::check_for_valid_k2(n, k);

    if(k.size() > 1){
      for(int i=0; i < n; i ++){
        idx = impl::window_idx(i, k(i) );
        res(i) = x[idx];
      }

    } else{
      for(int i=0; i < n; i ++){
        idx = impl::window_idx(i, k(0) );
        res(i) = x[idx];
      }
    }

    return res;
  }


  template <int RTYPE>
  SEXP unique_to_list( const Vector<RTYPE>& x, IntegerVector k ) {
    int n = x.size();
    IntegerVector idx;
    List res(n);
    Vector<RTYPE> levs;

    impl::check_for_valid_k2(n, k);

    if(k.size() > 1){
      for(int i=0; i < n; i ++){
        idx = impl::window_idx(i, k(i) );
        levs = x[idx];
        res(i) = unique(levs);
      }

    } else{
      for(int i=0; i < n; i ++){
        idx = impl::window_idx(i, k(0) );
        levs = x[idx];
        res(i) = unique(levs);
      }
    }
    return res;
  }


}
