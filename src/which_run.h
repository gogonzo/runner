using namespace Rcpp;

namespace impl {

  IntegerVector which_NA(LogicalVector x) {
    IntegerVector v = Rcpp::seq(0, x.size()-1);
    return v[Rcpp::is_na(x)];
  }

  IntegerVector whicht_vector(LogicalVector x){
    IntegerVector v = Rcpp::seq(0, x.size()-1);
    return v[ x ];
  }

  int first(IntegerVector x){
    if(x.size()>0)
      return x(0);
    return std::numeric_limits<int>::max();
  }
  int last(IntegerVector x){
    int n = x.size();
    if(n>0)
      return x(n-1);
    return -1;
  }

  int get_window_start(int i, int k, IntegerVector indexes){
    for(int j=i; j>=0; j--)
      if( (indexes(i) - indexes(j)) > ( k - 1) )
        return j + 1;
    return 0;
  }

}

#include <Rcpp.h>
using namespace Rcpp;

namespace impl {

template <int RTYPE>
int calc_whichd(const Vector<RTYPE>& x, int i, int i2){
  int cur_whichd  = NA_INTEGER;

  for(int j = i; j > i2 ; --j) {
      if( x( j ) == x( j - 1 ) ){
        ;

      } else {
        cur_whichd = j;
      }
  }
  return cur_whichd;
}

template <int RTYPE>
IntegerVector whichd_run_(const Vector<RTYPE>& x, IntegerVector k,bool na_pad){

  int i2;
  int n = x.size();
  int nk = k.size();
  int cur_whichd;
  IntegerVector res(n);

  /*  initial whichd */
  res(0) = cur_whichd = NA_INTEGER;


  if(nk==1 and ( (k(0)==0) or (k(0)==n) ) ){
    /* whichd run full */
    for(int i=1; i < n ; i++) {
      if( Vector<RTYPE>::is_na(x(i)) ) {
        cur_whichd = NA_INTEGER;
      } else if( x( i - 1 ) == x( i ) ){
      } else {
        cur_whichd = i;
      }
      res( i ) = cur_whichd;
    }
  } else {
    /* whichd_run window */
    for(int i = 1; i < n; ++i) {
      if( nk == 1 ){
        i2 = window_index(i, k(0) );
      } else {
        i2 = window_index(i, k(i) );
      }
      res( i ) = calc_whichd(x, i, i2);
    }
  }

  /* if padding with NA */
  if(na_pad)
    std::fill(res.begin(), res.end() - n + k(0) - 1 , NA_INTEGER);

  return res;
}
}

