namespace aggr {
    template <int RTYPE>
    inline int calc_actual_streak(const Rcpp::Vector<RTYPE>& x, int u, int l, bool na_rm) {
      int uu = u;
      int cur_streak {0};

      if (na_rm) {
        for (int j = u; j >= l ; --j) {
          if (Rcpp::Vector<RTYPE>::is_na(x(j))) continue;
          if (Rcpp::Vector<RTYPE>::is_na(x(uu))) uu = j;
          if (x(j) == x(uu)) {
            cur_streak += 1L;
          } else {
            break;
          }
        }
      } else {
        for (int j = u; j >= l ; --j) {
          if (Rcpp::Vector<RTYPE>::is_na(x(j))) return NA_INTEGER;
          if (Rcpp::Vector<RTYPE>::is_na(x(uu))) uu = j;
          if (x(j) == x(uu)) {
            cur_streak += 1L;
          } else {
            break;
          }
        }
      }

      if (cur_streak == 0) return NA_INTEGER;
      return cur_streak;
    }
    inline double calc_max(Rcpp::NumericVector x, int u, int l, bool na_rm) {
      double cur_max = NA_REAL;

      if (na_rm) {
        for (int i = l; i <= u ; ++i) {
          if (x(i) > cur_max or Rcpp::NumericVector::is_na(cur_max)) {
            cur_max = x(i);
          }
        }
      } else {
        for (int i = l; i <= u; ++i) {
          if (Rcpp::NumericVector::is_na(x(i))) {
            cur_max = NA_REAL;
            return cur_max;
          }
          if (Rcpp::NumericVector::is_na(cur_max) or x(i) > cur_max) {
            cur_max = x(i);
          }
        }
      }
      return cur_max;
    }

    inline double calc_min(Rcpp::NumericVector x, int u, int l, bool na_rm) {
      double cur_min = NA_REAL;

      if (na_rm) {
        for (int i = l; i <= u ; ++i) {
          if (x(i) < cur_min or Rcpp::NumericVector::is_na(cur_min)) {
            cur_min = x(i);
          }
        }
      } else {
        for (int i = l; i <= u; ++i) {
          if (Rcpp::NumericVector::is_na(x(i))) {
            cur_min = NA_REAL;
            return cur_min;
          }
          if (Rcpp::NumericVector::is_na(cur_min) or x(i) < cur_min) {
            cur_min = x(i);
          }
        }
      }
      return cur_min;
    }

    template <typename otype>
    inline otype calc_sum(Rcpp::NumericVector x, int u, int l, bool na_rm) {
      otype cur_sum = NA_REAL;
      if (na_rm) {
        for (int i = l; i <= u ; ++i) {
          if (Rcpp::NumericVector::is_na(cur_sum) & !Rcpp::NumericVector::is_na(x(i))) {
            cur_sum = x(i);
          } else if (!Rcpp::NumericVector::is_na(x(i))) {
            cur_sum += x(i);
          }
        }
      } else {
        for (int i = l; i <= u; ++i) {
          if (Rcpp::NumericVector::is_na(x(i))) {
            return NA_REAL;
          }
          if (Rcpp::NumericVector::is_na(cur_sum) & !Rcpp::NumericVector::is_na(x(i))) {
            cur_sum = x(i);
          } else if (!Rcpp::NumericVector::is_na(x(i))) {
            cur_sum += x(i);
          }
        }
      }
      return cur_sum;
    }

    inline double calc_mean(Rcpp::NumericVector x, int u, int l, bool na_rm) {
      double cur_sum = NA_REAL;
      int nonna = 0;

      if (na_rm) {
        for (int i = l; i <= u ; ++i) {
          if (Rcpp::NumericVector::is_na(cur_sum) & !Rcpp::NumericVector::is_na(x(i))) {
            nonna += 1;
            cur_sum = x(i);
          } else if (!Rcpp::NumericVector::is_na(x(i))) {
            nonna += 1;
            cur_sum += x(i);
          }
        }
      } else {
        for (int i = l; i <= u; ++i) {
          if (Rcpp::NumericVector::is_na(x(i))) {
            return NA_REAL;
          }
          if (Rcpp::NumericVector::is_na(cur_sum) & !Rcpp::NumericVector::is_na(x(i))) {
            nonna += 1;
            cur_sum = x(i);
          } else if (!Rcpp::NumericVector::is_na(x(i))) {
            nonna += 1;
            cur_sum += x(i);
          }
        }
      }
      return cur_sum/nonna;
    }

    inline int calc_whicht(Rcpp::LogicalVector x, int u, int l, bool na_rm, std::string which) {

      if (which == "last") {
        if (na_rm) {
          for (int i = u; i >= l ; --i) {
            if (x(i) == TRUE) {
              return i + 1;
            }
          }
        } else {
          for (int i = u; i >= l; --i) {
            if (Rcpp::LogicalVector::is_na(x(i))) {
              return NA_INTEGER;
            } else if (x(i) == TRUE) {
              return i + 1;
            }
          }
        }
      } else if (which == "first") {
        if (na_rm) {
          for (int i = l; i <= u ; ++i) {
            if (x(i) == TRUE) {
              return i + 1;
            }
          }
        } else {
          for (int i = l; i <= u; ++i) {
            if (Rcpp::LogicalVector::is_na(x(i))) {
              return NA_INTEGER;
            } else if (x(i) == TRUE) {
              return i + 1;
            }
          }
        }
      }
      return NA_INTEGER;
    }

    Rcpp::NumericVector cummax(Rcpp::NumericVector x, bool na_rm) {
      int n = x.size();
      Rcpp::NumericVector res(n);
      double cur_max = NA_REAL;

      if (na_rm) {
        for (int i = 0; i < n ; ++i) {
          if (x(i) > cur_max or Rcpp::NumericVector::is_na(cur_max)) {
            cur_max = x(i);
          }
          res(i) = cur_max;
        }
      } else {
        for (int i = 0; i < n; ++i) {
          if (Rcpp::NumericVector::is_na(x(i))) {
            std::fill(res.begin() + i, res.end(), NA_REAL);
            return res;
          }
          if (Rcpp::NumericVector::is_na(cur_max) or x(i) > cur_max) {
            cur_max = x(i);
          }
          res(i) = cur_max;
        }
      }
      return res;
    }

    Rcpp::NumericVector cummin(Rcpp::NumericVector x, bool na_rm) {
      int n = x.size();
      Rcpp::NumericVector res(n);
      double cur_max = NA_REAL;

      if (na_rm) {
        for (int i = 0; i < n ; ++i) {
          if (x(i) < cur_max or Rcpp::NumericVector::is_na(cur_max)) {
            cur_max = x(i);
          }
          res(i) = cur_max;
        }
      } else {
        for (int i = 0; i < n; ++i) {
          if (Rcpp::NumericVector::is_na(x(i))) {
            std::fill(res.begin() + i, res.end(), NA_REAL);
            return res;
          }
          if (Rcpp::NumericVector::is_na(cur_max) or x(i) < cur_max) {
            cur_max = x(i);
          }
          res(i) = cur_max;
        }
      }
      return res;
    }

    Rcpp::NumericVector cumsum(Rcpp::NumericVector x, bool na_rm) {
      int n = x.size();
      Rcpp::NumericVector res(n);
      double cur_sum = NA_REAL;

      if (na_rm) {
        for (int i = 0; i < n ; ++i) {
          if (Rcpp::NumericVector::is_na(cur_sum) & !Rcpp::NumericVector::is_na(x(i))) {
            cur_sum = x(i);
          } else if (!Rcpp::NumericVector::is_na(x(i))) {
            cur_sum += x(i);
          }
          res(i) = cur_sum;
        }
      } else {
        for (int i = 0; i < n; ++i) {
          if (Rcpp::NumericVector::is_na(x(i))) {
            std::fill(res.begin() + i, res.end(), NA_REAL);
            return res;
          }
          if (Rcpp::NumericVector::is_na(cur_sum) & !Rcpp::NumericVector::is_na(x(i))) {
            cur_sum = x(i);
          } else if (!Rcpp::NumericVector::is_na(x(i))) {
            cur_sum += x(i);
          }
          res(i) = cur_sum;
        }
      }
      return res;
    }

    Rcpp::NumericVector cummean(Rcpp::NumericVector x, bool na_rm) {
      int n = x.size();
      Rcpp::NumericVector res(n);
      double cur_sum = NA_REAL;
      double nonna = 0;

      if (na_rm) {
        for (int i = 0; i < n ; ++i) {
          if (Rcpp::NumericVector::is_na(cur_sum) & !Rcpp::NumericVector::is_na(x(i))) {
            cur_sum = x(i);
            nonna += 1;
          } else if (!Rcpp::NumericVector::is_na(x(i))) {
            cur_sum += x(i);
            nonna += 1;
          }
          res(i) = cur_sum/nonna;
        }
      } else {
        for (int i = 0; i < n; ++i) {
          if (Rcpp::NumericVector::is_na(x(i))) {
            std::fill(res.begin() + i, res.end(), NA_REAL);
            return res;
          }
          if (Rcpp::NumericVector::is_na(cur_sum) & !Rcpp::NumericVector::is_na(x(i))) {
            cur_sum = x(i);
            nonna += 1;
          } else if (!Rcpp::NumericVector::is_na(x(i))) {
            cur_sum += x(i);
            nonna += 1;
          }
          res(i) = cur_sum/nonna;
        }
      }
      return res;
    }

    Rcpp::IntegerVector cumwhicht(Rcpp::LogicalVector x, bool na_rm, std::string which) {
      int n = x.size();
      Rcpp::IntegerVector res(n);
      double whicht = NA_INTEGER;

      if (which == "last") {
        if (na_rm) {
          for (int i = 0; i < n ; ++i) {
            if (x(i) == TRUE) {
              whicht = i + 1;
            }
            res(i) = whicht;
          }
        } else {
          for (int i = 0; i < n ; ++i) {
            if (Rcpp::LogicalVector::is_na(x(i))) {
              whicht = NA_INTEGER;
            } else if (x(i) == TRUE) {
              whicht = i + 1;
            }
            res(i) = whicht;
          }
        }
      } else if (which == "first") {
        if (na_rm) {
          for (int i = 0; i < n ; ++i) {
            if (x(i) == TRUE) {
              std::fill(res.begin() + i, res.end(), i + 1);
              return res;
            }
            res(i) = whicht;
          }
        } else {
          for (int i = 0; i < n ; ++i) {
            if (Rcpp::LogicalVector::is_na(x(i))) {
              std::fill(res.begin() + i, res.end(), NA_INTEGER);
              return res;
            } else if (x(i) == TRUE) {
              std::fill(res.begin() + i, res.end(), i + 1);
              return res;
            }
            res(i) = whicht;
          }
        }
      }
      return res;
    }
}
