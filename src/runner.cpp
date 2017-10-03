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
NumericVector runner(
  NumericVector x,
  int k  = 0,
  int lag = 0
) {


  int n = x.size();
  if(k == 0) k = n;
  NumericVector res(n);

  // sum the values from the beginning of the vector to n
  for(int i = 0; i < n; i++){
  res[ i ] = std::accumulate( x.begin(), x.begin() + i + 1 , 0.0 );
  }


  for(int i = k - 1; i < n; i++) {
  res[ i ] = (res[ i ] - std::accumulate( x.begin(), x.begin() + i - n  + 1 , 0.0 ))/k;
  }

  std::fill(res.begin(), res.end() - n + k - 1 , NA_REAL);


return res;
}
