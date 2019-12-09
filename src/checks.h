#ifndef checks_h
#define checks_h

namespace checks {
  inline void check_k(Rcpp::IntegerVector k, int n) {
    if (k.size() != n and k.size() > 1) {
      Rcpp::stop("length of k and length of x differs. length(k) should be 1 or equal to x");
    } else if (Rcpp::any(Rcpp::is_na(k))) {
      Rcpp::stop("Function doesn't accept NA values in k vector");
    }
    if (k.size() == 1) {
      if (k(0) < 0) {
        Rcpp::stop("k can't be negative");
      }
    } else if (k.size() > 1) {
      for (int i = 0; i < n; i++) {
        if (k(i) < 0) {
          Rcpp::stop("k can't be negative");
        }
      }
    }
  }

  inline void check_idx(Rcpp::IntegerVector idx, int n) {
    if (idx.size() != n and idx.size() > 1) {
      Rcpp::stop("length of idx and length of x differs. length(idx) should be 1 or equal to x");
    } else if (Rcpp::any(Rcpp::is_na(idx))) {
      Rcpp::stop("Function doesn't accept NA values in idx vector");
    }

    if (idx.size() > 1) {
      for (int i = 1; i < n; i++) {
        if (idx(i) < idx(i - 1)) {
          Rcpp::stop("idx have to be in descending order");
        }
      }
    }
  }

  inline void check_lag(Rcpp::IntegerVector lag, int n) {
    if (lag.size() != n and lag.size() > 1) {
      Rcpp::stop("length of lag and length of x differs. length(lag) should be 1 or equal to x");
    } else if (Rcpp::any(Rcpp::is_na(lag))) {
      Rcpp::stop("Function doesn't accept NA values in lag vector");
    }
  }

}
#endif
