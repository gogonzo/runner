/*
#include <Rcpp.h>
using namespace Rcpp;

class streak_run2_impl {
  private:
    int i1;
    int i2;

  public:
    streak_run2_impl(int i1, int i2) : i1(i1), i2(i2) {}

    template <int RTYPE>
    IntegerVector operator()(const Vector<RTYPE>& x)
    {

      int cur_streak=1;

      if (Vector<RTYPE>::is_na(x[0])){
        cur_streak = NumericVector::get_na();
      } else {
        cur_streak = 1;
      }

      for(int j = i1; j <= i2 ; ++j) {
        if( x[ j ] == x[ j-1 ] ){
          cur_streak += 1;

        } else if(Vector<RTYPE>::is_na( x[ j ] )){
          cur_streak = NumericVector::get_na();

        } else {
          cur_streak = 1;

        }
      }
      return cur_streak;
    }
};


// [[Rcpp::export]]
RObject streak_run2(RObject x, int i1 = 0, int i2=6)
{
  RCPP_RETURN_VECTOR(streak_run2_impl(i1, i2), x);
}
*/
