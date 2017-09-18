#include <Rcpp.h>
using namespace Rcpp;
//' Running index of maximum prior to i-th element
//'
//' Running index of maximum prior to i-th element
//'
//' @param x A single integer.
//' @examples
//' x <- c(NA,NA,0,NA, 1, 2,-6,4)
//' whichmax_run(x)
//' @export
// [[Rcpp::export]]
NumericVector whichmax_run(NumericVector vec) {
  int n = vec.size();
  double max  = -INFINITY;
  NumericVector idx(n);

  if (NumericVector::is_na(vec[0])){
    idx[0] = NumericVector::get_na();
  } else {
    idx[0] = 1;
    max = vec[0];
  }

  for(int i = 1; i < n; ++i) {

    if( vec[i] > max){
      idx[i] =  i + 1;
      max    = vec[i];

    } else if(NumericVector::is_na(vec[i])){
      idx[i] = idx[i - 1];

    } else if(NumericVector::is_na(idx[i-1])){
      idx[i] = i + 1;
      max = vec[i];

    } else {
      idx[i] = idx[ i - 1 ];

    }
  }
  return idx;
}
