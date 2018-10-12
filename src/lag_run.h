using namespace Rcpp;

namespace lag {

  template <int RTYPE>
  Vector<RTYPE> lag_run11(const Vector<RTYPE>& x, int k){
    int n = x.size();
    Vector<RTYPE> out(n);
    for(int i = 0; i<n; i++)
      if((i-k)>=0){
        out(i) = x(i-k);
      } else {
        out(i) = Vector<RTYPE>::get_na();
      }

   return out;
  }

  template <int RTYPE>
  Vector<RTYPE> lag_run12(const Vector<RTYPE>& x, IntegerVector k){
    int n = x.size();
    Vector<RTYPE> out(n);
    for(int i = 0; i<n; i++)
      if((i-k(i))>=0){
        out(i) = x( i-k(i) );
      } else {
        out(i) = Vector<RTYPE>::get_na();
      }
      return out;
  }

  template <int RTYPE>
  Vector<RTYPE> lag_run21(const Vector<RTYPE>& x, int k, IntegerVector indexes){
    int n = x.size();
    Vector<RTYPE> out(n);
    out(0) = Vector<RTYPE>::get_na();

    for(int i = 1; i<n; i++){
      for(int j = (i-1); j >= 0;j--){
        if(indexes(j) <= (indexes(i)-k)){
          out(i) = x(j);
          break;
        } else if(j==0){
          out(i) = Vector<RTYPE>::get_na();
        }
      }
    }
    return out;
  }

  template <int RTYPE>
  Vector<RTYPE> lag_run22(const Vector<RTYPE>& x, IntegerVector k, IntegerVector indexes){
    int n = x.size();
    Vector<RTYPE> out(n);
    out(0) = Vector<RTYPE>::get_na();

    for(int i = 1; i<n; i++){
      for(int j = (i-1); j >= 0;j--){
        if(indexes(j) <= (indexes(i)-k(i))){
          out(i) = x(j);
          break;
        } else if(j==0){
          out(i) = Vector<RTYPE>::get_na();
        }
      }
    }
    return out;
  }

}
