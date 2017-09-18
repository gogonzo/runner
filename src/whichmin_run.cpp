#include <Rcpp.h>
using namespace Rcpp;
//' Running length of consecutive occurence
//'
//' Running length of consecutive occurence
//'
//' @param x A single integer.
//' @examples
//' x <- c(NA,NA,99999999999,3.01, 3, 4,-6,-7)
//' whichmin_run(x)
//' @export
// [[Rcpp::export]]
NumericVector whichmin_run(NumericVector vec) {
  int n = vec.size();
  double min  = INFINITY;
  NumericVector idx(n);

  if (NumericVector::is_na(vec[0])){
    idx[0] = NumericVector::get_na();
  } else {
    idx[0] = 1;
    min = vec[0];
  }

  for(int i = 1; i < n; ++i) {

    if( vec[i] < min){
      idx[i] =  i + 1;
      min    = vec[i];

    } else if(NumericVector::is_na(vec[i])){
      idx[i] = idx[i - 1];

    } else if(NumericVector::is_na(idx[i-1])){
      idx[i] = i + 1;
      min = vec[i];

    } else {
      idx[i] = idx[ i - 1 ];

    }
  }
  return idx;
}

