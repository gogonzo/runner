#include <Rcpp.h>
using namespace Rcpp;
//' Streak length of vector elements
//'
//' Calculates series of consecutive elements
//'
//' @param x A single integer.
//' @examples
//' x <- c(1,3,4,3,5,7,8,2,4,9)
//' mean_run(x)
//' @export
// [[Rcpp::export]]
NumericVector mean_run(
    NumericVector x,
    int n  = 0
) {


  int sz = x.size();
  if(n == 0) n = sz;
  NumericVector res(sz);

  // sum the values from the beginning of the vector to n
  for(int i = 0; i < sz; i++){
    res[ i ] = std::accumulate( x.begin(), x.begin() + i + 1 , 0.0 );
  }


  for(int i = n - 1; i < sz; i++) {
    res[ i ] = (res[ i ] - std::accumulate( x.begin(), x.begin() + i - n  + 1 , 0.0 ))/n;
  }

  std::fill(res.begin(), res.end() - sz + n - 1 , NA_REAL);


  return res;
}
