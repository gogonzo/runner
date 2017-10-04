#include <Rcpp.h>
using namespace Rcpp;

namespace impl {

  LogicalVector isNA_int(IntegerVector x) {
    int n = x.size();
    LogicalVector out(n);

    for (int i = 0; i < n; ++i)
      out( i ) = IntegerVector::is_na(x( i ));

    return out;
  }
  LogicalVector isNA_num(NumericVector x) {
    int n = x.size();
    LogicalVector out(n);

    for (int i = 0; i < n; ++i) {
      out[i] = NumericVector::is_na(x[i]);
    }
    return out;
  }

  bool any_NA(NumericVector x){
    // Note the use of is_true to return a bool type
    return is_true(any(is_na(x)));
  }

  template <int RTYPE>
  int run_for_first_na(const Vector<RTYPE>& x){
    int first_non_na = -1;
    int n = x.size();

    for(int j=0; j < n; j++){
      if( !Vector<RTYPE>::is_na(x(j)) ){
        first_non_na = j;
        break;
      }
    }
    return first_non_na;
  }

  NumericVector na_when_na(NumericVector x, NumericVector res ){
    int n = x.size();
    int first_na = run_for_first_na( x );

    if( first_na > -1 )
      for(int i=first_na; i < n; i++)
        if( NumericVector::is_na(x(i)))
          res( i ) = NumericVector::get_na();

    return res;
  }

  void check_for_valid_k(SEXP x, IntegerVector k){
    int n = Rf_length(x);

    if( k(0) == 0 ){
      k(0) = n;
    } else if(k.size() != n and k.size() > 1){
      stop("length of k and length x differs. k=0 and k=length(x) only allowed");
    } else if( Rcpp::all(Rcpp::is_na(k)) ){
      stop("Function doesn't accept NA values in k vector");
    }
  }
  void check_for_valid_k2(int n, IntegerVector k){

    if( k(0) == 0 ){
      k(0) = n;
    } else if( k.size() != n and k.size() > 1 ){
      stop("length of k and length x differs. k=0 and k=length(x) only allowed");
    } else if( Rcpp::all(Rcpp::is_na(k)) ){
      stop("Function doesn't accept NA values in k vector");
    }
  }

  int window_index(int i, int k){
    int begin;
    if( (i - k + 1) < 0) begin = 0; else begin = i - k + 1;
    return begin;
  }

}
