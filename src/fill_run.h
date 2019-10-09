using namespace Rcpp;

namespace fill {

  template <int RTYPE>
  int run_for_non_na(const Vector<RTYPE>& x, int i){
    int first_non_na = -1;
    int n = x.size();

    for(int j=i; j < n; j++)
      if(!Vector<RTYPE>::is_na(x(j)))
        return j;

      return first_non_na;
  }

  template <int RTYPE>
  Vector<RTYPE> fill_run(const Vector<RTYPE>& x, bool run_for_first, bool only_within)
  {

    int n = x.size();
    Vector<RTYPE> res(n);

    /* if all are NA throw warning */
    int next_non_na = run_for_non_na(x,0);
    if (next_non_na < 0) {
      warning("All x values are NA");
      return x;
    }

    /* run_for_first option fills prior to first non-na */
    if (run_for_first and next_non_na > 0) {
      for (int i = 0; i < next_non_na; i++)
        res(i) = x(next_non_na);
    } else {
      for (int i=0; i < next_non_na; i++)
        res(i) = x(i);
    }

    /* if first_non_na is a the end*/
    if (next_non_na == n)
      return res;

    /* actual function - if na fill with previous */
    if (!only_within) {
      for (int i = next_non_na; i < n; ++i) {
        if (!Vector<RTYPE>::is_na(x(i))) {
          res(i) = x(i);
        } else {
          res(i) = res(i - 1);
        }
      }

    /* only_within */
    } else {
      for(int i = next_non_na; i < n; ++i) {
        if(!Vector<RTYPE>::is_na(x(i))) {
          res(i) = x(i);
        } else {
          next_non_na = run_for_non_na(x, i);

          // if no finite till the end
          if(next_non_na==-1){
            for(int j = i; j < n; j++)
              res(j) = Vector<RTYPE>::get_na();
            return res;
          }

          // if two non-na ends equals
          if(x(i-1) == x(next_non_na)){
            for(int j = i; j<next_non_na; j++)
              res(j) = res(i - 1);
            i = next_non_na-1;

          // if two non-na ends differs
          } else {
            for(int j = i; j<next_non_na; j++)
              res(j) = Vector<RTYPE>::get_na();
            i = next_non_na-1;

          }
        }
      }
    }

    return res;
  }


}
