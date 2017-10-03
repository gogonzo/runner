#include <Rcpp.h>
using namespace Rcpp;
//' Repeat non-NA (fill further)
//'
//' Fills replace NA values (or specified) with previous non-NA
//'
//' @param x A single integer.
//' @examples
//' x <- c(NA,1,-1,NA, 1, 2,6,4)
//' max_run(x)
//' @export
// [[Rcpp::export]]

NumericVector fill_along(NumericVector vec) {

    return vec;
}
