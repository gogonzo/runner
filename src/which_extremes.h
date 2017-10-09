namespace impl {

  IntegerVector cum_extr_f(NumericVector x, bool na_rm){
    int n = x.size();
    int idx = IntegerVector::get_na();
    double cur_max = NumericVector::get_na();
    IntegerVector res(n);

    for(int i = 0; i < x.size(); i ++){

      // handling na
      if( !na_rm and NumericVector::is_na( x(i) ) ){
        std::fill(res.begin() + i, res.end(), NumericVector::get_na() );
        break;
      }

      if (x(i) > cur_max){
        cur_max = x(i);
        idx = i + 1;
      } else if(NumericVector::is_na(cur_max) and !NumericVector::is_na(x(i))) {
        cur_max = x(i);
        idx = i + 1;
      }

      res(i) = idx;
    }

    return res;
  }

  IntegerVector window_extr2_f( NumericVector x, IntegerVector k, bool na_rm ){

    int n = x.size();
    int i1;
    int idx;
    double cur_extr;
    IntegerVector res(n);

    for(int i = 0; i < n; i++){

      idx = IntegerVector::get_na();
      cur_extr = NumericVector::get_na();
      i1 = impl::window_index( i, k( i ) );

      for(int j = i1; j <= i ; ++j){
        if( !na_rm and NumericVector::is_na( x(j) ) ){
          idx =  IntegerVector::get_na();
          break;
        }

        if (x(j) > cur_extr){
          cur_extr = x(j);
          idx = j + 1;
        } else if(NumericVector::is_na(cur_extr) and !NumericVector::is_na(x(j))) {
          cur_extr = x(j);
          idx = j + 1;
        }

      }
      res( i ) = idx;
    }
    return res;
  }

  IntegerVector window_extr_f(NumericVector x, int k, bool na_rm){
    int n = x.size();
    int i1;
    int idx;
    double cur_extr;
    IntegerVector res(n);

    for(int i = 0; i < n; i++){

      idx = IntegerVector::get_na();
      cur_extr = NumericVector::get_na();
      i1 = impl::window_index( i, k );

      for(int j = i1; j <= i ; ++j){
        if( !na_rm and NumericVector::is_na( x(j) ) ){
          idx =  IntegerVector::get_na();
          break;
        }

        if (x(j) > cur_extr){
          cur_extr = x(j);
          idx = j + 1;
        } else if(NumericVector::is_na(cur_extr) and !NumericVector::is_na(x(j))) {
          cur_extr = x(j);
          idx = j + 1;
        }

      }
      res( i ) = idx;
    }
    return res;
  }

  IntegerVector cum_extr_l(NumericVector x, bool na_rm){
    int n = x.size();
    int idx = IntegerVector::get_na();
    double cur_max = NumericVector::get_na();
    IntegerVector res(n);

    for(int i = 0; i < x.size(); i ++){

      // handling na
      if( !na_rm and NumericVector::is_na( x(i) ) ){
        std::fill(res.begin() + i, res.end(), NumericVector::get_na() );
        break;
      }

      if (x(i) >= cur_max){
        cur_max = x(i);
        idx = i + 1;
      } else if(NumericVector::is_na(cur_max) and !NumericVector::is_na(x(i))) {
        cur_max = x(i);
        idx = i + 1;
      }

      res(i) = idx;
    }

    return res;
  }

  IntegerVector window_extr2_l( NumericVector x, IntegerVector k, bool na_rm ){

    int n = x.size();
    int i1;
    int idx;
    double cur_extr;
    IntegerVector res(n);

    for(int i = 0; i < n; i++){

      idx = IntegerVector::get_na();
      cur_extr = NumericVector::get_na();
      i1 = impl::window_index( i, k( i ) );

      for(int j = i1; j <= i ; ++j){
        if( !na_rm and NumericVector::is_na( x(j) ) ){
          idx =  IntegerVector::get_na();
          break;
        }

        if (x(j) >= cur_extr){
          cur_extr = x(j);
          idx = j + 1;
        } else if(NumericVector::is_na(cur_extr) and !NumericVector::is_na(x(j))) {
          cur_extr = x(j);
          idx = j + 1;
        }

      }
      res( i ) = idx;
    }
    return res;
  }

  IntegerVector window_extr_l(NumericVector x, int k, bool na_rm){
    int n = x.size();
    int i1;
    int idx;
    double cur_extr;
    IntegerVector res(n);

    for(int i = 0; i < n; i++){

      idx = IntegerVector::get_na();
      cur_extr = NumericVector::get_na();
      i1 = impl::window_index( i, k );

      for(int j = i1; j <= i ; ++j){
        if( !na_rm and NumericVector::is_na( x(j) ) ){
          idx =  IntegerVector::get_na();
          break;
        }

        if (x(j) >= cur_extr){
          cur_extr = x(j);
          idx = j + 1;
        } else if(NumericVector::is_na(cur_extr) and !NumericVector::is_na(x(j))) {
          cur_extr = x(j);
          idx = j + 1;
        }

      }
      res( i ) = idx;
    }
    return res;
  }


}
