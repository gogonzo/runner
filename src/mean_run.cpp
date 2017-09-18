#include <Rcpp.h>
using namespace Rcpp;
//' Streak length of vector elements
//'
//' Calculates series of consecutive elements
//'
//' @param x A single integer.
//' @examples
//' x <- c(1,1,0,0,1,0,3,3,1,1)
//' mean_run(x)
//' @export
// [[Rcpp::export]]
#include <Rcpp.h>
using namespace Rcpp;
//' Streak length of vector elements
//'
//' Calculates series of consecutive elements
//'
//' @param x A single integer.
//' @examples
//' x <- c(1,1,0,0,1,0,3,3,1,1)
//' mean_run(x)
//' @export
// [[Rcpp::export]]
NumericVector mean_run(NumericVector x, int n) {

  int sz = x.size();
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
