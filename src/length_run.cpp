#include <Rcpp.h>
using namespace Rcpp;
#include "window_run.h"

//' Length of running windows
//'
//' Number of elements in window specified by idx
//' @param k integer vector which specifies window length
//' @param idx an optional integer vector containing index of observations.
//' @examples
//' length_run(k=3,idx=c(1,2,2,4,5,5,5,5,5,5))
//' @export
// [[Rcpp::export]]
IntegerVector length_run(IntegerVector k = 0, IntegerVector idx = 0) {

  int n = idx.size();
  IntegerVector res(n);


  if( (k.size() == 1) ) {
    for(int i = 0; i<n; i++){
      for(int j=i; j>=0; j--)
        if( (idx(i) - idx(j)) > (k(0) - 1) ){
          res(i) = i - j;
          break;
        } else if(j == 0){
          res(i) = i + 1;
        }
    }

    // IDX VARYING WINDOW -----------
  } else if( (k.size() > 1) ) {
    for(int i = 0; i<n; i++){
      for(int j=i; j>=0; j--)
        if( (idx(i) - idx(j)) > (k(i) - 1) ){
          res(i) = i - j;
          break;
        } else if(j == 0){
          res(i) = i + 1;
        }
    }
  }


  return res;
}
