#include <Rcpp.h>
using namespace Rcpp;
//' Running index of last TRUE element
//'
//' Running index of last TRUE element
//'
//' @param x An logical vector
//' @examples
//' x <- c(NA,FALSE,FALSE,TRUE, FALSE, TRUE,FALSE,NA)
//' whichtrue_run(x)
//' @export
// [[Rcpp::export]]
NumericVector whichtrue_run(LogicalVector vec) {
  int n = vec.size();
  NumericVector idx(n);

  if (vec[0] == TRUE){
    idx[0] = 1;
  } else {
    idx[0] = NumericVector::get_na();
  }

  for(int i = 1; i < n; ++i) {
    if( vec[i] == TRUE){
      idx[i] =  i + 1;
    } else {
      idx[i] = idx[ i - 1 ];
    }
  }
  return idx;
}
