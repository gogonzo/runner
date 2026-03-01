using namespace Rcpp;
#include <algorithm>

namespace lag {

  template <int RTYPE>
  Vector<RTYPE> lag_run11(const Vector<RTYPE>& x, int k) {
    int n = x.size();
    Vector<RTYPE> out(n);
    for (int i = 0; i < n; i++) {
      if (((i - k) < 0) || ((i - k) >= n)) {
        out(i) = Vector<RTYPE>::get_na();
      } else {
        out(i) = x(i - k);
      }
    }
   return out;
  }

  template <int RTYPE>
  Vector<RTYPE> lag_run12(const Vector<RTYPE>& x, IntegerVector k) {
    int n = x.size();
    Vector<RTYPE> out(n);

    for (int i = 0; i < n; i++) {
      if (((i - k(i)) < 0) || ((i - k(i)) >= n)) {
        out(i) = Vector<RTYPE>::get_na();
      } else {
        out(i) = x(i - k(i));
      }
    }
    return(out);
  }

  template <int RTYPE>
  Vector<RTYPE> lag_run21(const Vector<RTYPE>& x, int k, IntegerVector indexes, bool nearest) {
    int n = x.size();
    Vector<RTYPE> out(n, Vector<RTYPE>::get_na());
    if (k == 0) {
      return x;
    }

    if (nearest) {
      if (k > 0) {
        for (int i = 1; i < n; i++) {
          int target = indexes(i) - k;
          // Find first position in [0, i) with indexes >= target
          auto it = std::lower_bound(indexes.begin(), indexes.begin() + i, target);
          int p = (int)(it - indexes.begin());

          if (p == i) {
            // All positions 0..i-1 have indexes < target → NA
            out(i) = Vector<RTYPE>::get_na();
          } else if (p == 0) {
            // indexes(0) >= target → position 0 is within range
            out(i) = x(0);
          } else {
            // p > 0 and p < i: indexes(p) >= target, indexes(p-1) < target
            if (indexes(p) != indexes(i)) {
              out(i) = x(p);
            } else {
              out(i) = Vector<RTYPE>::get_na();
            }
          }
        }
      } else if (k < 0) {
        for (int i = 0; i < (n - 1); i++) {
          int target = indexes(i) - k;  // indexes(i) + |k|
          // Find last position in [i+1, n-1] with indexes <= target
          auto it = std::upper_bound(indexes.begin() + i + 1, indexes.begin() + n, target);
          int last_le = (int)(it - indexes.begin()) - 1;

          if (last_le < i + 1) {
            // All positions i+1..n-1 have indexes > target
            out(i) = Vector<RTYPE>::get_na();
          } else if (last_le == n - 1) {
            // Reached end, matches original j==n-1 base case
            out(i) = x(n - 1);
          } else {
            // indexes(last_le) <= target, indexes(last_le+1) > target
            if (indexes(last_le) != indexes(i)) {
              out(i) = x(last_le);
            } else {
              out(i) = Vector<RTYPE>::get_na();
            }
          }
        }
      }
    } else if (!nearest) {
      if (k > 0) {
        for (int i = 1; i < n; i++) {
          int target = indexes(i) - k;
          // Find first position in [0, i) with indexes >= target
          auto it = std::lower_bound(indexes.begin(), indexes.begin() + i, target);
          int p = (int)(it - indexes.begin());

          if (p < i && indexes(p) == target) {
            out(i) = x(p);
          } else if (p == 0 && indexes(0) == target) {
            out(i) = x(0);
          } else {
            out(i) = Vector<RTYPE>::get_na();
          }
        }
      } else if (k < 0) {
        for (int i = 0; i < n; i++) {
          int target = indexes(i) - k;  // indexes(i) + |k|
          // Find last position in [i+1, n-1] with indexes == target
          // First, find positions with indexes <= target
          auto it = std::upper_bound(indexes.begin() + i + 1, indexes.begin() + n, target);
          int last_le = (int)(it - indexes.begin()) - 1;

          if (last_le >= i + 1 && indexes(last_le) == target) {
            out(i) = x(last_le);
          } else if (last_le == n - 1 && indexes(n - 1) == target) {
            out(i) = x(n - 1);
          } else {
            out(i) = Vector<RTYPE>::get_na();
          }
        }
      }

    }
    return out;
  }

  template <int RTYPE>
  Vector<RTYPE> lag_run22(const Vector<RTYPE>& x, IntegerVector k, IntegerVector indexes, bool nearest) {
    int n = x.size();
    Vector<RTYPE> out(n, Vector<RTYPE>::get_na());

    if (nearest) {
        for (int i = 0; i < n; i++) {
          if (k(i) == 0) {
            out(i) = x(i);
          } else if (k(i) > 0) {
            if (i == 0) {
              out(i) = Vector<RTYPE>::get_na();
              continue;
            }
            int target = indexes(i) - k(i);
            auto it = std::lower_bound(indexes.begin(), indexes.begin() + i, target);
            int p = (int)(it - indexes.begin());

            if (p == i) {
              out(i) = Vector<RTYPE>::get_na();
            } else if (p == 0) {
              out(i) = x(0);
            } else {
              if (indexes(p) != indexes(i)) {
                out(i) = x(p);
              } else {
                out(i) = Vector<RTYPE>::get_na();
              }
            }
          } else if (k(i) < 0) {
            if (i == n - 1) {
              out(i) = Vector<RTYPE>::get_na();
              continue;
            }
            int target = indexes(i) - k(i);
            auto it = std::upper_bound(indexes.begin() + i + 1, indexes.begin() + n, target);
            int last_le = (int)(it - indexes.begin()) - 1;

            if (last_le < i + 1) {
              out(i) = Vector<RTYPE>::get_na();
            } else if (last_le == n - 1) {
              out(i) = x(n - 1);
            } else {
              if (indexes(last_le) != indexes(i)) {
                out(i) = x(last_le);
              } else {
                out(i) = Vector<RTYPE>::get_na();
              }
            }
          }
        }
    } else if (!nearest) {
      for (int i = 1; i < n; i++) {
        if (k(i) == 0) {
          out(i) = x(i);
        } else if (k(i) > 0) {
          int target = indexes(i) - k(i);
          auto it = std::lower_bound(indexes.begin(), indexes.begin() + i, target);
          int p = (int)(it - indexes.begin());

          if (p < i && indexes(p) == target) {
            out(i) = x(p);
          } else if (p == 0 && indexes(0) == target) {
            out(i) = x(0);
          } else {
            out(i) = Vector<RTYPE>::get_na();
          }

        } else if (k(i) < 0) {
          int target = indexes(i) - k(i);
          auto it = std::upper_bound(indexes.begin() + i + 1, indexes.begin() + n, target);
          int last_le = (int)(it - indexes.begin()) - 1;

          if (last_le >= i + 1 && indexes(last_le) == target) {
            out(i) = x(last_le);
          } else if (last_le == n - 1 && indexes(n - 1) == target) {
            out(i) = x(n - 1);
          } else {
            out(i) = Vector<RTYPE>::get_na();
          }
        }
      }
    }
    return out;
  }
}
