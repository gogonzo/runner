#include <Rcpp.h>
using namespace Rcpp;
//' Streak length of vector elements
//'
//' Calculates series of consecutive elements
//'
//' @param x A single integer.
//' @examples
//' x <- c(1,1,0,0,1,0,3,3,1,1)
//' streak_run(x)
//' @export
// [[Rcpp::export]]
NumericVector streak_run(NumericVector vec, int k = NA_INTEGER) {
  int n = vec.size();
  NumericVector streak(n);

  if (NumericVector::is_na(vec[0])){
    streak[0] = NumericVector::get_na();
  } else {
    streak[0] = 1;
  }

  for(int i = 1; i < n; ++i) {
    if( vec[i] == vec[i-1]){
      streak[i] =  streak[i - 1] + 1;

    } else if(NumericVector::is_na(vec[i])){
      streak[i] = NumericVector::get_na();

    } else {
      streak[i] = 1;

    }
  }
  return streak;
}

