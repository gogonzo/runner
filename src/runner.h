namespace apply {
  template <typename otype, int OTYPE>
  otype apply(Rcpp::Vector<OTYPE> const& x,
              Rcpp::IntegerVector const& idx,
              Rcpp::Function f) {
    Rcpp::Vector<OTYPE> window(idx(1) - idx(0) + 1);
    std::copy(x.begin() + idx(0), x.begin() + idx(1) + 1, window.begin());

    return Rcpp::as<otype>(f(window));
  }


  template <int OTYPE>
  Rcpp::Vector<OTYPE> subset_window(Rcpp::Vector<OTYPE>const& x,
                                    Rcpp::IntegerVector const& idx) {
    Rcpp::Vector<OTYPE> window(idx(1) - idx(0) + 1);
    std::copy(x.begin() + idx(0), x.begin() + idx(1) + 1, window.begin());

    return window;
  }



}
