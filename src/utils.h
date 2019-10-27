using namespace Rcpp;

namespace utils {
  //' Boundaries of the lagged running window
  IntegerVector window_ul(int i, int k, int lag, int n, bool na_pad, bool cum = false) {
    IntegerVector res(2);

    // exceptions
    if (na_pad) {
      if (cum) {
        if (i - lag >= n or lag > i) return IntegerVector(0);
      } else {
        if ((i - lag - k + 1) < 0 or (i - lag) >= n) return IntegerVector(0);
      }
      // |---------- [ ]    [ ] |----------
    } else {
      if (cum) {
        if (lag > i) return IntegerVector(0);
      } else {
        if (lag > i or (i - lag - k + 1) >= n) return IntegerVector(0);
      }

    }

    // upper bound
    if ((i - lag) >= n){
      res(1) = n - 1;
    } else {
      res(1) = i - lag;
    }

    // lower bound
    if (cum || (i - k - lag + 1) <= 0) {
      res(0) = 0;
    } else {
      res(0) = i - k - lag + 1;
    }

    return res;
  }

  //' Boundaries of the lagged running window based on indexes
  IntegerVector window_ul_dl(IntegerVector indexes, int i, int k, int lag, int n, bool na_pad, bool cum = false) {
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

    IntegerVector idx_out(2);
    // cumulative ========================================================================
    if (cum) {
      // [-------]-+-->
      if (lag >= 0) {
        for (int u = i; u >= 0; u--) {
          if ((indexes(i) - indexes(u)) >= lag) {
            idx_out(0) = 0;
            idx_out(1) = u;
            return idx_out;
          } else if (u == 0) {
            return IntegerVector(0);
          }
        }
        // [-------+-]-->
      } else {
        for (int u = i; u < n; u++) {
          if ((indexes(i) - indexes(u)) < lag) {
            idx_out(0) = 0;
            idx_out(1) = u - 1;
            return idx_out;
          } else if (u == (n - 1)) {
            idx_out(0) = 0;
            idx_out(1) = u;
            return idx_out;
          }
        }
      }
    }

    if (lag >= 0) {
      for (int u = i; u >= 0; u--) {
        if ((indexes(i) - indexes(u)) < (k + lag)) {
          if ((indexes(i) - indexes(u)) >= lag) {
            for (int l = u; l >= 0; l--) {
              if ((indexes(i) - indexes(l) > (k + lag - 1))) {
                idx_out(0) = l + 1;
                idx_out(1) = u;
                return idx_out;
              } else if (l == 0) {
                idx_out(1) = u;
                return idx_out;
              }
            }
          }
        } else {
          return IntegerVector(0);
        }
      }
    } else {
      // l <- i -> u
      if (-lag < k) {
        for (int l = i; l >= -1; l--) {
          if (l == -1 or indexes(l) < (indexes(i) - lag - k + 1)) {
            for (int u = i; u < n; u++) {
              if (indexes(u) > (indexes(i) - lag)) {
                idx_out(0) = l + 1;
                idx_out(1) = u - 1;
                return idx_out;
              } else if (u == (n - 1)) {
                idx_out(0) = l + 1;
                idx_out(1) = u;
                return idx_out;
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
                  idx_out(0) = l;
                  idx_out(1) = u - 1;
                  return idx_out;
                } else if (u == (n - 1)) {
                  idx_out(0) = l;
                  idx_out(1) = u;
                  return idx_out;
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
}
