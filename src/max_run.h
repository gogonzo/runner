using namespace Rcpp;

namespace min {
double calc_max(double x, double cur_max){
  if (x > cur_max){
    return x;
  } else if(x <=cur_max ){
    return cur_max;
  } else if (NumericVector::is_na(cur_max)){
    return x;
  } else {
    return cur_max;
  }
}

NumericVector window_max(NumericVector x, bool na_rm ){
  int n = x.size();
  double cur_max = NumericVector::get_na();
  NumericVector res(n);

  for(int i = 0; i < x.size(); i ++){
    // handling NAs
    if( !na_rm and NumericVector::is_na( x(i) ) ){
      std::fill(res.begin() + i, res.end(), NumericVector::get_na() );
      break;
    }

    cur_max = calc_max( x( i ) , cur_max );
    res(i) = cur_max;
  }
  return res;
}


NumericVector window_max21(NumericVector x, IntegerVector k, bool na_rm){
  int n = x.size();
  int i1;
  double cur_max;
  NumericVector res(n);

  for(int i = 0; i < n; i++){

    cur_max = NumericVector::get_na();
    if( (i - k(0) + 1) < 0) i1 = 0; else i1 = i - k(0) + 1;

    for(int j = i1; j <= i ; ++j){
      if(!na_rm and NumericVector::is_na(x(j)) ){
        cur_max = NA_REAL;
        break;
      }

      cur_max = calc_max( x( j ), cur_max);
    }
    res( i ) = cur_max;
  }

  return res;
}



NumericVector window_max22(NumericVector x, IntegerVector k, bool na_rm){
  int n = x.size();
  int i1;
  double cur_max;
  NumericVector res(n);

  for(int i = 0; i < n; i++){

    cur_max = NA_REAL;
    if( (i - k(i) + 1) < 0) i1 = 0; else i1 = i - k(i) + 1;

    for(int j = i1; j <= i ; ++j){
      if(!na_rm and NumericVector::is_na(x(j)) ){
        cur_max = NA_REAL;
        break;
      }

      cur_max = calc_max( x( j ), cur_max);
    }
    res( i ) = cur_max;
  }
  return res;
}

NumericVector window_max31( NumericVector x, IntegerVector k, IntegerVector indexes, bool na_rm){
  double cur_max;
  NumericVector res( x.size() );
  IntegerVector idx;

  for(int i=0; i < x.size(); i++){
    cur_max = R_NegInf;
    for(int j=i;j>=0;j--){
      if( (indexes(i) - indexes(j) > (k(0) - 1) ))
        break;
      if (x(j) > cur_max){
        cur_max = x(j);
      } else if (NumericVector::is_na(cur_max) & !na_rm){
        cur_max = NA_REAL;
        break;
      }

    }
    res(i) = cur_max;
  }

  return( res );
}

NumericVector window_max32( NumericVector x, IntegerVector k, IntegerVector indexes, bool na_rm){
  double cur_max;
  NumericVector res( x.size() );
  IntegerVector idx;

  for(int i=0; i < x.size(); i++){
    cur_max = R_NegInf;
    for(int j=i;j>=0;j--){
      if( (indexes(i) - indexes(j) > (k(i) - 1) ))
        break;
      if (x(j) > cur_max){
        cur_max = x(j);
      } else if (NumericVector::is_na(cur_max) & !na_rm){
        cur_max = NA_REAL;
        break;
      }

    }
    res(i) = cur_max;
  }

  return( res );
}


}
