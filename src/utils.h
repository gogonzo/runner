using namespace Rcpp;

namespace utils {
  //' Lower boundary of the lagged running window
  int lower_index(int i, int k, int lag) {
    if ((i - k - lag + 1) < 0) {
      return 0;
    } else {
      return i - k - lag + 1;
    }
  }

  //' Upper boundary of the lagged running window
  int upper_index(int i, int k, int lag) {
    if ((i - lag) < 0) {
      return 0;
    } else {
      return i - lag;
    }
  }

  //' Boundaries of the lagged running window
  IntegerVector window_ul(int i, int k, int lag) {
    IntegerVector res(2);
    if ((i - lag) < 0) {
      return IntegerVector(0);
    } else {
      res(1) = i - lag;
    }
    if ((i - k - lag + 1) < 0) {
      res(0) = 0;
    } else {
      res(0) = i - k - lag + 1;
    }
    return res;
  }

  //' Boundaries of the running window based on indexes
  IntegerVector window_ul_d(IntegerVector idx, int i, int k) {
    IntegerVector idx_out(2);

    for (int j = i; j >= 0; j--) {
      if ((idx(i) - idx(j) > (k - 1))) {
        idx_out(0) = j + 1;
        idx_out(1) = i;
        return idx_out;
      } else if (j == 0) {
        idx_out(1) = i;
        return idx_out;
      }
    }
    return idx_out;
  }

  //' Boundaries of the lagged running window based on indexes
  IntegerVector window_ul_dl(IntegerVector indexes, int i, int k, int lag) {
    IntegerVector idx_out(2);
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
    return IntegerVector(0);
  }
}
