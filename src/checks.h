#ifndef checks_h
#define checks_h

namespace checks {
  inline void check_type(std::string type) {
    if (type != "character" &&
        type != "numeric" &&
        type != "integer" &&
        type != "logical") {
      Rcpp::stop(
        "Invalid output type (" +
          type +
          "). Please specify one of the following: 'character', 'numeric', 'integer', 'logical'");
    }
  }

  inline void check_k(Rcpp::IntegerVector const& k, int n, std::string var) {
    if (k.size() != n and k.size() > 1) {
      Rcpp::stop(
        "length of k and length of " +
        var +
        " differs. length(k) should be 1 or equal to " +
        var
      );
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

  inline void check_idx(Rcpp::IntegerVector const& idx, int n, std::string var) {
    if (idx.size() != n and idx.size() > 1) {
      Rcpp::stop(
        "length of idx and length of " +
        var +
        " differs. length(idx) should be 1 or equal to " +
        var
      );
    } else if (Rcpp::any(Rcpp::is_na(idx))) {
      Rcpp::stop("Function doesn't accept NA values in idx vector");
    }

    if (idx.size() > 1) {
      for (int i = 1; i < n; i++) {
        if (idx(i) < idx(i - 1)) {
          Rcpp::stop("idx have to be in ascending order");
        }
      }
    }
  }

  inline void check_lag(Rcpp::IntegerVector const& lag,
                        int n,
                        std::string var) {
    if (lag.size() != n and lag.size() > 1) {
      Rcpp::stop("length of lag and length of " +
                 var +
                 " differs. length(lag) should be 1 or equal to " +
                 var);
    } else if (Rcpp::any(Rcpp::is_na(lag))) {
      Rcpp::stop("Function doesn't accept NA values in lag vector");
    } else if (lag.size() == 0) {
      Rcpp::stop("length of lag should not be zero. Please specify lag as single value, or don't specify for default value.");
    }
  }

  inline void check_at(Rcpp::IntegerVector const& at) {
    if (Rcpp::any(Rcpp::is_na(at))) {
      Rcpp::stop("Function doesn't accept NA values in at vector");
    }
  }

}
#endif
