using namespace Rcpp;

namespace impl {


  template <int RTYPE>
  int run_for_non_na(const Vector<RTYPE>& x, int i){
    int first_non_na = -1;
    int n = x.size();

    for(int j=i; j < n; j++)
      if( !Vector<RTYPE>::is_na(x(j)) )
        return j;

    return first_non_na;
  }

  void check_for_valid_k(SEXP x, IntegerVector k){
    int n = Rf_length(x);

    if( k(0) == 0 ){
      k(0) = n;
    } else if(k.size() != n and k.size() > 1){
      stop("length of k and length x differs. k=0 and k=length(x) only allowed");
    } else if( Rcpp::any(Rcpp::is_na(k)) ){
      stop("Function doesn't accept NA values in k vector");
    }
  }
  void check_for_valid_k2(int n, IntegerVector k){

    if( k(0) == 0 ){
      k(0) = n;
    } else if( k.size() != n and k.size() > 1 ){
      stop("length of k and length x differs. k=0 and k=length(x) only allowed");
    } else if( Rcpp::any(Rcpp::is_na(k)) ){
      stop("Function doesn't accept NA values in k vector");
    }
  }

  int window_index(int i, int k){
    int begin;
    if( (i - k + 1) < 0) begin = 0; else begin = i - k + 1;
    return begin;
  }

  IntegerVector window_idx(int i, int k){
    IntegerVector idx;
    if( (i - k + 1) < 0) idx = seq_len(i+1)-1; else idx = i - k + seq_len(k);
    return idx;
  }

}
