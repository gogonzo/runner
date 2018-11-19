#include <Rcpp.h>
using namespace Rcpp;

namespace min {

double calc_min(double x, double cur_min){
  if (x < cur_min){
    return x;
  } else if(x >=cur_min ){
    return cur_min;
  } else if (NumericVector::is_na(cur_min)){
    return x;
  } else {
    return cur_min;
  }
}

NumericVector window_min(NumericVector x, bool na_rm ){
  int n = x.size();
  double cur_min = NumericVector::get_na();
  NumericVector res(n);

  for(int i = 0; i < x.size(); i ++){
    // handling NAs
    if( !na_rm and NumericVector::is_na( x(i) ) ){
      std::fill(res.begin() + i, res.end(), NA_REAL );
      break;
    }

    cur_min = calc_min( x( i ) , cur_min );
    res(i) = cur_min;
  }
  return res;
}


NumericVector window_min21(NumericVector x, IntegerVector k, bool na_rm){
  int n = x.size();
  int i1;
  double cur_min;
  NumericVector res(n);

  for(int i = 0; i < n; i++){

    cur_min = NA_REAL;
    if( (i - k(0) + 1) < 0) i1 = 0; else i1 = i - k(0) + 1;

    for(int j = i1; j <= i ; ++j){
      if(!na_rm and NumericVector::is_na(x(j)) ){
        cur_min = NA_REAL;
        break;
      }

      cur_min = calc_min( x( j ), cur_min);
    }
    res( i ) = cur_min;
  }

  return res;
}



NumericVector window_min22(NumericVector x, IntegerVector k, bool na_rm){
  int n = x.size();
  int i1;
  double cur_min;
  NumericVector res(n);

  for(int i = 0; i < n; i++){

    cur_min = NA_REAL;
    if( (i - k(i) + 1) < 0) i1 = 0; else i1 = i - k(i) + 1;

    for(int j = i1; j <= i ; ++j){
      if(!na_rm and NumericVector::is_na(x(j)) ){
        cur_min = NA_REAL;
        break;
      }

      cur_min = calc_min( x( j ), cur_min);
    }
    res( i ) = cur_min;
  }
  return res;
}

NumericVector window_min31( NumericVector x, IntegerVector k, IntegerVector indexes, bool na_rm){
  double cur_min;
  NumericVector res( x.size() );
  IntegerVector idx;

  for(int i=0; i < x.size(); i++){
    cur_min = R_PosInf;
    for(int j=i;j>=0;j--){
      if( (indexes(i) - indexes(j) > (k(0) - 1) ))
        break;
      if (x(j) < cur_min){
        cur_min = x(j);
      } else if (NumericVector::is_na(cur_min) & !na_rm){
        cur_min = NA_REAL;
        break;
      }

    }
    res(i) = cur_min;
  }

  return( res );
}

NumericVector window_min32( NumericVector x, IntegerVector k, IntegerVector indexes, bool na_rm){
  double cur_min;
  NumericVector res( x.size() );
  IntegerVector idx;

  for(int i=0; i < x.size(); i++){
    cur_min = R_PosInf;
    for(int j=i;j>=0;j--){
      if( (indexes(i) - indexes(j) > (k(i) - 1) ))
        break;
      if (x(j) < cur_min){
        cur_min = x(j);
      } else if (NumericVector::is_na(cur_min) & !na_rm){
        cur_min = NA_REAL;
        break;
      }

    }
    res(i) = cur_min;
  }

  return( res );
}


}
