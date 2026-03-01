#include <algorithm>

namespace listfuns
{
  template <int RTYPE>
  Rcpp::Vector<RTYPE> get_window(Rcpp::Vector<RTYPE> const &x, int u, int l)
  {
    int n = u - l + 1;
    Rcpp::Vector<RTYPE> res(n);
    std::copy(x.begin() + l, x.begin() + u + 1, res.begin());
    return res;
  }
}
