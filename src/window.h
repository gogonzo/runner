namespace apply {
  IntegerVector get_window_idx(int i, int k, int lag, int n, bool na_pad, bool cum = false) {
    // [ |---]-------     |-------[---  ]
    if (na_pad) {
      if (cum) {
        if ((i - lag >= n) or lag > i) return IntegerVector(0);
      } else {
        if ((i - lag - k + 1) < 0 or (i - lag) >= n) return IntegerVector(0);
      }
    // |---------- [ ]    [ ] |----------
    } else {
      if (cum) {
        if (lag > i) return Rcpp::IntegerVector(0);
      } else {
        if (lag > i or (i - lag - k + 1) >= n) return IntegerVector(0);
      }
    }

    // [ |---------  ]
    if (((i - k - lag + 1) <= 0) & (i - lag >= n)) {
      return Rcpp::Range(0, n - 1);
    // [ |-----]---
    } else if ((i - k - lag + 1) <= 0) {
      return Rcpp::Range(0, i - lag);
    //   [---------  ]
    } else if ((i - lag >= n) & (k == n)) {
      return Rcpp::Range(0, n - 1);
    // |--[--------  ]
    } else if (i - lag >= n) {
      return Rcpp::Range(i - lag - k + 1, n - 1);
    // |--[----]---
    } else {
      return Rcpp::Range(i - lag - k + 1, i - lag);
    }

  }

  IntegerVector get_dwindow_idx_lag(IntegerVector indexes, int i, int k, int lag, int n, bool na_pad, bool cum) {
    if (na_pad) {
      if (cum) {
        if ((indexes(i) - lag > indexes(n - 1)) or (indexes(i) - lag < indexes(0))) return IntegerVector(0);
      } else {
        if (((indexes(i) - lag - k + 1) < indexes(0)) or ((indexes(i) - lag) > indexes(n - 1))) return IntegerVector(0);
      }
      // |---------- [ ]    [ ] |----------
    } else {
      if (cum) {
        if (indexes(i) - lag < indexes(0)) return IntegerVector(0);
      } else {
        if (((indexes(i) - lag) < indexes(0)) or ((indexes(i) - lag - k + 1) > indexes(n - 1)))
          return IntegerVector(0);
      }
    }

    // cumulative ========================================================================
    if (cum) {
      // [-------]-+-->
      if (lag >= 0) {
        for (int u = i; u >= 0; u--) {
          if ((indexes(i) - indexes(u)) >= lag) {
            return Rcpp::Range(0, u);
          } else if (u == 0) {
            return IntegerVector(0);
          }
        }
      // [-------+-]-->
      } else {
        for (int u = i; u < n; u++) {
          if ((indexes(i) - indexes(u)) < lag) {
            return Rcpp::Range(0, u - 1);
          } else if (u == (n - 1)) {
            return Rcpp::Range(0, u);
          }
        }
      }
    }

    // windowed ========================================================================
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
      // l <- i -> u
      if (-lag < k) {
        for (int l = i; l >= -1; l--) {
          if (l == -1 or indexes(l) < (indexes(i) - lag - k + 1)) {
            for (int u = i; u < n; u++) {
              if (indexes(u) > (indexes(i) - lag)) {
                return Rcpp::Range(l + 1, u - 1);
              } else if (u == (n - 1)) {
                return Rcpp::Range(l + 1, u);
              }
            }
          }
        }
      // i -> l -> u
      } else {
        for (int l = i; l < n; l++) {
          if (indexes(l) <= (indexes(i) - lag)) {
            if (indexes(l) >= (indexes(i) - lag - k + 1)) {
              for (int u = l; u < n; u++) {
                if (indexes(u) > (indexes(i) - lag)) {
                  return Rcpp::Range(l, u - 1);
                } else if (u == (n - 1)) {
                  return Rcpp::Range(l, u);
                }
              }
            }
          } else {
            return IntegerVector(0);
          }
        }
      }
    }

    return IntegerVector(0);
  }

  template <int RTYPE>
  Vector<RTYPE> get_window(const Vector<RTYPE>& x, IntegerVector idx) {
    return x[idx];
  }
}
