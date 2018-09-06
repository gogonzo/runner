using namespace Rcpp;

namespace impl {


  int first_na_index(NumericVector x){
    for(int i = 0; i < x.size();i++)
      if( NumericVector::is_na(x(i)))
        return i;

    return -1;
  }
  NumericVector calc_sum_vector(NumericVector x){
    int n = x.size();
    double first_sum = NumericVector::get_na();
    NumericVector res(n);

    /* sum on window end*/
    for(int i = 0; i < n; i++){
      if( ISNAN( first_sum ) ){
        first_sum = x( i );
      } else if( !ISNAN( x( i ) )) {
        first_sum += x( i );
      }
      res( i ) = first_sum;
    }

    return res;
  }
  NumericVector calc_na1_vector(NumericVector x){
    int n = x.size();
    NumericVector res(n);

    /* sum on window end*/
    for(int i = 0; i < n; i++)
      if( ISNAN( x( i ) ))
        res( i ) = 1.0;

    return res;
  }
  NumericVector calc_sum_vector0(NumericVector x){
    int n = x.size();
    double cumsum = 0.0;
    NumericVector res(n);

    /* sum on window end*/
    for(int i = 0; i < n; i++){
    if( !ISNAN( x( i ) ))
      cumsum += x( i );

      res( i ) = cumsum;
    }

    return res;
  }
  NumericVector calc_na_vector(NumericVector x){
    double count = 0.0;
    int n = x.size();
    NumericVector nacount(n);

    for(int i = 0; i < n; i++){
      if(ISNAN( x( i ) ))
        count += 1.0;
      nacount(i) = count;
    }

      return nacount;
  }
  NumericVector calc_nonna_vector(NumericVector x){
    double count = 0.0;
    int n = x.size();
    NumericVector nacount(n);

    for(int i = 0; i < n; i++){
      if(!ISNAN( x( i ) ))
        count += 1.0;
       nacount(i) = count;
    }

    return nacount;
  }


}
