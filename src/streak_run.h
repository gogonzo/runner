using namespace Rcpp;

namespace streak {
  int window_index(int i, int k){
    int begin;
    if( (i - k + 1) < 0) begin = 0; else begin = i - k + 1;
    return begin;
  }

  template <int RTYPE>
  int  calc_actual_streak(const Vector<RTYPE>& x, int i, int i2, bool na_rm)
  {
    int j_f = NA_INTEGER;
    int cur_streak=1;

    // run for first finite
    for(int j = i; j >= i2 ; --j)
      if( Vector<RTYPE>::is_na(x(j)) ){
        if(!na_rm){
          return NA_INTEGER;}
      } else {
        j_f = j;
        break;
      }

    if( IntegerVector::is_na(j_f))
        return NA_INTEGER;


    for(int j = j_f; j >= i2 ; --j) {
      if( j < j_f ){
        if( x( j ) == x( j_f ) ){
          cur_streak += 1;
          j_f = j;
        } else if( !Vector<RTYPE>::is_na(x(j)) ){
          return cur_streak;
        } else {
          if(!na_rm)
            return cur_streak;
        }
      }
    }
    return cur_streak;
  }

  template <int RTYPE>
  IntegerVector streak_run1(const Vector<RTYPE>& x, IntegerVector k,  bool na_rm, bool na_pad)
  {

    int i2;
    int n = x.size();
    int nk = k.size();
    int cur_streak;
    int j_f = IntegerVector::get_na();
    IntegerVector res(n);

    /*  initial streak */
    for(int i=0; i < n ; i++)
    if ( Vector<RTYPE>::is_na( x(i) ) ){
      res(i) = NA_INTEGER;
    } else {
      j_f = i;
      res(i) = cur_streak = 1;
      break;
    }


    if(nk==1 and ( k(0)==0 or k(0)==n ) ){
      /* streak run full */
      for(int i=j_f; i < n ; i++) {
        if( i > j_f){
          if( x( i ) == x( j_f ) ){
            cur_streak += 1;
            j_f = i;
          } else if( Vector<RTYPE>::is_na( x( i )  ) ) {
            if(!na_rm){
              cur_streak = 0;
              res( i ) = NA_INTEGER;
              continue;}
          } else {
            cur_streak = 1;
            j_f = i;
          }
        }
        res( i ) = cur_streak == 0 ? NA_INTEGER : cur_streak;
      }
    } else {
    /* streak_run window */
      for(int i = 1; i < n; ++i) {
        if( nk == 1 ){
          i2 = window_index(i, k(0) );
        } else {
          i2 = window_index(i, k(i) );
        }
        res( i ) = calc_actual_streak(x, i, i2, na_rm);
      }
    }

    /* if padding with NA */
    if(na_pad)
      std::fill(res.begin(), res.end() - n + k(0) - 1 , NA_INTEGER);

    return res;
  }

  template <int RTYPE>
  IntegerVector streak_run2( const Vector<RTYPE>& x, IntegerVector k, bool na_rm, bool na_pad, IntegerVector indexes){
    int n = x.size();
    IntegerVector res( n );
    IntegerVector idx;
    if( k.size()==1 ){
      for(int i=0; i < n; i++){
        for(int j=i; j>=0; j--){
          if( (indexes(i) - indexes(j) > (k(0) - 1) )){
            res(i) = calc_actual_streak(x, i, j+1, na_rm);
            break;
          } else if(j==0){
            res(i) = calc_actual_streak(x, i, 0, na_rm);
          }
        }
      }
    } else {
      for(int i=0; i < n; i++){
        for(int j=i; j>=0; j--){
          if( (indexes(i) - indexes(j) > (k(i) - 1) )){
            res(i) = calc_actual_streak(x, i, j+1, na_rm);
            break;
          } else if(j==0){
            res(i) = calc_actual_streak(x, i, 0, na_rm);
          }
        }
      }
    }
    return( res );
  }
}
