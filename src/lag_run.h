using namespace Rcpp;

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
          for (int j = i - 1; j >= 0; j--) {
            if (indexes(j) < (indexes(i) - k) && (indexes(j + 1) >= (indexes(i) - k)) && (indexes(j + 1) != indexes(i))) {
              out(i) = x(j + 1);
              break;
            } else if (indexes(j) < (indexes(i) - k)) {
              out(i) = Vector<RTYPE>::get_na();
              break;
            } else if (j == 0) {
              out(i) = x(0);
            }
          }
        }
      } else if (k < 0) {
        for (int i = 0; i < (n - 1); i++) {
          for (int j = i + 1; j < n; j++) {
            if (indexes(j) > (indexes(i) - k) && (indexes(j - 1) <= (indexes(i) - k)) && (indexes(j - 1) != indexes(i))) {
              out(i) = x(j - 1);
              break;
            } else if (indexes(j) > (indexes(i) - k)) {
              out(i) = Vector<RTYPE>::get_na();
              break;
            } else if (j == (n - 1)) {
              out(i) = x(n - 1);
            }
          }
        }
      }
    } else if (!nearest) {
      if (k > 0) {
        for (int i = 1; i < n; i++) {
          for (int j = i - 1; j >= 0; j--) {
            if ((indexes(j) < (indexes(i) - k)) && (indexes(j + 1) == (indexes(i) - k))) {
              out(i) = x(j + 1);
              break;
            } else if (j == 0 && indexes(j) == (indexes(i) - k)) {
              out(i) = x(0);
            } else if (indexes(j) < (indexes(i) - k) || j == 0) {
              out(i) = Vector<RTYPE>::get_na();
              break;
            }
          }
        }
      } else if (k < 0) {
        for (int i = 0; i < n; i++) {
          for (int j = i + 1; j < n; j++) {
            if ((indexes(j) > (indexes(i) - k)) && (indexes(j - 1) == (indexes(i) - k))) {
              out(i) = x(j - 1);
              break;
            } else if (j == n - 1 && (indexes(j) == (indexes(i) - k))) {
              out(i) = x(j);
            } else if (indexes(j) > (indexes(i) - k) || j == (n - 1)) {
              out(i) = Vector<RTYPE>::get_na();
              break;
            }
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
            for (int j = i - 1; j >= 0; j--) {
              if (indexes(j) < (indexes(i) - k(i)) && (indexes(j + 1) >= (indexes(i) - k(i))) && (indexes(j + 1) != indexes(i))) {
                out(i) = x(j + 1);
                break;
              } else if (indexes(j) < (indexes(i) - k(i))) {
                out(i) = Vector<RTYPE>::get_na();
                break;
              } else if (j == 0) {
                out(i) = x(0);
              }
            }
          } else if (k(i) < 0) {
            if (i == n - 1) {
              out(i) = Vector<RTYPE>::get_na();
              continue;
            }
            for (int j = i + 1; j < n; j++) {
              if (indexes(j) > (indexes(i) - k(i)) && (indexes(j - 1) <= (indexes(i) - k(i))) && (indexes(j - 1) != indexes(i))) {
                out(i) = x(j - 1);
                break;
              } else if (indexes(j) > (indexes(i) - k(i))) {
                out(i) = Vector<RTYPE>::get_na();
                break;
              } else if (j == (n - 1)) {
                out(i) = x(n - 1);
              }
            }
          }
        }
    } else if (!nearest) {
      for (int i = 1; i < n; i++) {
        if (k(i) == 0) {
          out(i) = x(i);
        } else if (k(i) > 0) {
          for (int j = i - 1; j >= 0; j--) {
            if ((indexes(j) < (indexes(i) - k(i))) && (indexes(j + 1) == (indexes(i) - k(i)))) {
              out(i) = x(j + 1);
              break;
            } else if (j == 0 && indexes(j) == (indexes(i) - k(i))) {
              out(i) = x(0);
            } else if (indexes(j) < (indexes(i) - k(i)) || j == 0) {
              out(i) = Vector<RTYPE>::get_na();
              break;
            }
          }

        } else if (k(i) < 0) {
          for (int j = i + 1; j < n; j++) {
            if ((indexes(j) > (indexes(i) - k(i))) && (indexes(j - 1) == (indexes(i) - k(i)))) {
              out(i) = x(j - 1);
              break;
            } else if (j == n - 1 && (indexes(j) == (indexes(i) - k(i)))) {
              out(i) = x(j);
            } else if (indexes(j) > (indexes(i) - k(i)) || j == (n - 1)) {
              out(i) = Vector<RTYPE>::get_na();
              break;
            }
          }
        }
      }
    }
    return out;
  }
}
