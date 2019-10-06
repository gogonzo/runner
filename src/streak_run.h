using namespace Rcpp;

namespace streak {
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

  template <int RTYPE>
  int  calc_actual_streak(const Vector<RTYPE>& x, int u, int l, bool na_rm) {
    int j_f = NA_INTEGER;
    int cur_streak = 1;
    // run for first finite
    for (int j = u; j >= l ; --j) {
      if (Vector<RTYPE>::is_na(x(j))) {
        if (!na_rm) {
          return NA_INTEGER;
        }
      } else {
        j_f = j;
        break;
      }
    }

    if (IntegerVector::is_na(j_f)) return NA_INTEGER;
    for (int j = j_f; j >= l ; --j) {
      if (j < j_f) {
        if (x(j) == x(j_f)) {
          cur_streak += 1;
          j_f = j;
        } else if (!Vector<RTYPE>::is_na(x(j))) {
          return cur_streak;
        } else {
          if (!na_rm) return cur_streak;
        }
      }
    }
    return cur_streak;
  }

  template <int RTYPE>
  IntegerVector streak_run1(const Vector<RTYPE>& x, IntegerVector k, IntegerVector lag,  bool na_rm, bool na_pad) {
    int n = x.size();
    int j_f = 0;
    int cur_streak;
    IntegerVector b(2);
    IntegerVector res(n);

    /* streak_run window */
    if (k.size() == 1 & lag.size() == 1 & (k(0) == 0 or k(0) == n)) {
      /* streak run full */
      if (lag(0) >= n) {
        std::fill(res.begin(), res.end(), NA_INTEGER);
        return res;
      }

      for(int i = lag(0); i < n ; i++) {
        if (Vector<RTYPE>::is_na(x(i - lag(0)))) {
          res(i) = NA_INTEGER;
        } else {
          j_f = i;
          res(i) = cur_streak = 1;
          break;
        }
      }

      for (int i = j_f; i < n ; i++) {
        if (i > j_f) {
          if (x(i - lag(0)) == x(j_f - lag(0))) {
            cur_streak += 1;
            j_f = i;
          } else if (Vector<RTYPE>::is_na(x(i - lag(0)))) {
            if (!na_rm) {
              cur_streak = 0;
              res(i) = NA_INTEGER;
              continue;}
          } else {
            cur_streak = 1;
            j_f = i;
          }
        }
        res(i) = cur_streak == 0 ? NA_INTEGER : cur_streak;
      }
      std::fill(res.begin(), res.end() - n + lag(0), NA_INTEGER);
    } else if (k.size() == 1 & lag.size() == 1) {
      for (int i = 0; i < n; ++i) {
        b = window_ul(i, k(0), lag(0));
        res(i) = (b.size() == 2) ? calc_actual_streak(x, b(1), b(0), na_rm) : NA_INTEGER;
      }
    } else if (k.size() > 1 & lag.size() == 1) {
      for (int i = 0; i < n; ++i) {
        b = window_ul(i, k(i), lag(0));
        res(i) = (b.size() == 2) ? calc_actual_streak(x, b(1), b(0), na_rm) : NA_INTEGER;
      }
    } else if (k.size() == 1 & lag.size() > 1) {
      for (int i = 0; i < n; ++i) {
        b = window_ul(i, k(0), lag(i));
        res(i) = (b.size() == 2) ? calc_actual_streak(x, b(1), b(0), na_rm) : NA_INTEGER;
      }
    } else if (k.size() > 1 & lag.size() > 1) {
      for (int i = 0; i < n; ++i) {
        b = window_ul(i, k(i), lag(i));
        res(i) = (b.size() == 2) ? calc_actual_streak(x, b(1), b(0), na_rm) : NA_INTEGER;
      }
    }

    /* if padding with NA */
    if (na_pad)
      std::fill(res.begin(), res.end() - n + k(0) - 1 , NA_INTEGER);

    return res;
  }

  template <int RTYPE>
  IntegerVector streak_run2(const Vector<RTYPE>& x, IntegerVector k, IntegerVector lag, bool na_rm, bool na_pad, IntegerVector indexes) {
    IntegerVector b;
    int n = x.size();
    IntegerVector res(n);

    /* streak on current window */
    if (k.size() == 1 & lag.size() == 1 & lag(0) == 0) {
      for (int i = 0; i < n; ++i) {
        b = window_ul_d(indexes, i, k(0));
        res(i) = calc_actual_streak(x, b(1), b(0), na_rm);
      }
    } else if (k.size() > 1 & lag.size() == 1 & lag(0) == 0) {
      for (int i = 0; i < n; ++i) {
        b = window_ul_d(indexes, i, k(i));
        res(i) = calc_actual_streak(x, b(1), b(0), na_rm);
      }
    /* streak on lagged window */
    } else if (k.size() == 1 & lag.size() == 1) {
      for (int i = 0; i < n; ++i) {
        b = window_ul_dl(indexes, i, k(0), lag(0));
        res(i) = (b.size() == 2) ? calc_actual_streak(x, b(1), b(0), na_rm) : NA_INTEGER;
      }
    } else if (k.size() > 1 & lag.size() > 1) {
      for (int i = 0; i < n; ++i) {
        b = window_ul_dl(indexes, i, k(i), lag(i));
        res(i) = (b.size() == 2) ? calc_actual_streak(x, b(1), b(0), na_rm) : NA_INTEGER;
      }
    }

    return res;
  }



}
