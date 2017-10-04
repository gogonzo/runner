#include <Rcpp.h>
using namespace Rcpp;

namespace impl {

  template <int RTYPE>
  Vector<RTYPE> fill_run_impl(const Vector<RTYPE>& x, bool run_for_first)
  {

    int n = x.size();
    Vector<RTYPE> res(n);

    /* if all are NA throw warning */
    int first_non_na = run_for_first_na(x);
    if(first_non_na < 0){
      warning("All x values are NA");
      return x;
    }

    /* run_for_first option fills prior to first non-na */
    if( run_for_first and first_non_na > 0 ){
      for(int i=0; i <= first_non_na; i++)
        res( i ) = x( first_non_na );
    } else {
      for(int i=0; i <= first_non_na; i++)
        res( i ) = x( i );
    }

    /* if first_non_na is a the end*/
    if( first_non_na == n)
      return res;

    /* actual function - if na fill with previous */
    for( int i = first_non_na + 1; i < n; ++i ) {
      if( !Vector<RTYPE>::is_na( x(i) ) ) {
        res( i ) = x( i );
      } else {
        res( i ) = res( i - 1 );
      }
    }

    return res;
  }

}
