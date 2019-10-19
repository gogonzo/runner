using namespace Rcpp;

namespace apply {
  IntegerVector get_window_idx(int i, int k, int lag, int n) {
    if ((i - lag) < 0 or (i - lag - k + 1) >= n) return IntegerVector(0);

    if ((i - k - lag + 1) <= 0) {
      return Rcpp::seq_len(i + 1 - lag) - 1;
    } else if ((i - lag) >= n) {
      return Rcpp::Range(i - k - lag + 1, n - 1);
    } else {
      return i - lag - k + Rcpp::seq_len(k);
    }
  }

  IntegerVector get_dwindow_idx(IntegerVector idx, int i, int k, int n) {
    IntegerVector idx_out;
    for (int j = i; j >= 0; j--) {
      if ((idx(i) - idx(j) > (k - 1))) {
        return j + Rcpp::seq_len(i - j);
      } else if (j == 0) {
        return Rcpp::seq_len(i + 1) - 1;
      }
    }
    return IntegerVector(0);
  }

  IntegerVector get_dwindow_idx_lag(IntegerVector indexes, int i, int k, int lag, int n) {
    // indexes[(indexes > i - lag - k) & (indexes <= i - lag)];

    if (lag >= 0) {
      for (int u = i; u >= 0; u--) {
        if ((indexes(i) - indexes(u)) < (k + lag)) {
          if ((indexes(i) - indexes(u)) >= lag) {
            for (int l = u; l >= 0; l--) {
              if ((indexes(i) - indexes(l) > (k + lag - 1))) {
                return Rcpp::Range(l + 1, u);
              } else if (l == 0) {
                return Rcpp::Range(0, u);
              }
            }
          }
        } else {
          return IntegerVector(0);
        }
      }
    } else if (lag < 0) {
      for (int u = i; u < n; u++) {
        if ((indexes(u) - indexes(i)) > (k - lag) &&
            (indexes(u) - indexes(i)) <= (-lag)) {
          if ((indexes(i) - indexes(u)) <= lag) {
            for (int l = u; l >= 0; l--) {
              if ((indexes(i) - indexes(l) > (k + lag - 1))) {
                return Rcpp::Range(l + 1, u);
              } else if (l == 0) {
                return Rcpp::Range(0, u);
              }
            }
          }
        } else {
          return IntegerVector(0);
        }
      }
    }

    return IntegerVector(0);
  }

  template <int RTYPE>
  Vector<RTYPE> get_window(const Vector<RTYPE>& x, IntegerVector idx) {
    return x[idx];
  }

  template <int RTYPE>
  double apply_on_window(const Vector<RTYPE>& x, IntegerVector idx, Function f) {
    return Rcpp::as<double>(f(x[idx]));
  }
}
