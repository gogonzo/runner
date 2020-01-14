namespace listfuns {
  template <int RTYPE>
  Rcpp::Vector<RTYPE> get_window(Rcpp::Vector<RTYPE> const& x, int u, int l) {
    int n = u - l + 1;
    Rcpp::Vector<RTYPE> res(n);

    for (int i = 0; i < n; i++) {
      res(i) = x(l + i);
    }

    return res;
  }
}
