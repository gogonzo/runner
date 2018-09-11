using namespace Rcpp;

namespace window {

template <int RTYPE>

Rcpp::List window_to_list(const Vector<RTYPE>& x, IntegerVector k)
{
  int n = x.size();
  IntegerVector idx;
  List res(n);

  if(k.size() > 1){
    for(int i=0; i < n; i ++){
      if( (i - k(i) + 1) < 0)
        idx = Rcpp::seq_len(i+1)-1; else
        idx = i - k(i) + Rcpp::seq_len(k(i));
      res(i) = x[idx];
    }

  } else{
    for(int i=0; i < n; i ++){
      if( (i - k(0) + 1) < 0)
        idx = Rcpp::seq_len(i+1)-1; else
        idx = i - k(0) + Rcpp::seq_len(k(0));
      res(i) = x[idx];
    }
  }

  return res;
}

template <int RTYPE>
Rcpp::List window_to_list_int(const Vector<RTYPE>& x, IntegerVector k, IntegerVector indexes)
{
  int n = x.size();
  IntegerVector idx;
  List res(n);

  if(k.size() > 1){
    for(int i=0; i < n; i++){
      for(int j=i; j>=0; j--){
        if( (indexes(i) - indexes(j) > (k(i) - 1) )){

          idx = j + Rcpp::seq_len(i - j);
          break;
        } else if(j==0){
          idx = Rcpp::seq_len(i+1) - 1;
        }
      }
      res(i) = x[idx];
    }
  } else{
    for(int i=0; i < n; i++){
      for(int j=i; j>=0; j--){
        if( (indexes(i) - indexes(j) > (k(0) - 1) )){

          idx = j + Rcpp::seq_len(i - j);
          break;
        } else if(j==0){
          idx = Rcpp::seq_len(i+1) - 1;
        }
      }
      res(i) = x[idx];
    }
  }

  return res;
}

}