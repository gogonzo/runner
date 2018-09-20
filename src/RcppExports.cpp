// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// fill_run
SEXP fill_run(SEXP x, bool run_for_first, bool only_within);
RcppExport SEXP _runner_fill_run(SEXP xSEXP, SEXP run_for_firstSEXP, SEXP only_withinSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type run_for_first(run_for_firstSEXP);
    Rcpp::traits::input_parameter< bool >::type only_within(only_withinSEXP);
    rcpp_result_gen = Rcpp::wrap(fill_run(x, run_for_first, only_within));
    return rcpp_result_gen;
END_RCPP
}
// lag_run
SEXP lag_run(SEXP x, IntegerVector k, IntegerVector idx);
RcppExport SEXP _runner_lag_run(SEXP xSEXP, SEXP kSEXP, SEXP idxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type k(kSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type idx(idxSEXP);
    rcpp_result_gen = Rcpp::wrap(lag_run(x, k, idx));
    return rcpp_result_gen;
END_RCPP
}
// length_run
IntegerVector length_run(IntegerVector k, IntegerVector idx);
RcppExport SEXP _runner_length_run(SEXP kSEXP, SEXP idxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type k(kSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type idx(idxSEXP);
    rcpp_result_gen = Rcpp::wrap(length_run(k, idx));
    return rcpp_result_gen;
END_RCPP
}
// max_run
NumericVector max_run(NumericVector x, IntegerVector k, bool na_rm, bool na_pad, IntegerVector idx);
RcppExport SEXP _runner_max_run(SEXP xSEXP, SEXP kSEXP, SEXP na_rmSEXP, SEXP na_padSEXP, SEXP idxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type k(kSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    Rcpp::traits::input_parameter< bool >::type na_pad(na_padSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type idx(idxSEXP);
    rcpp_result_gen = Rcpp::wrap(max_run(x, k, na_rm, na_pad, idx));
    return rcpp_result_gen;
END_RCPP
}
// min_run
NumericVector min_run(NumericVector x, IntegerVector k, bool na_rm, bool na_pad, IntegerVector idx);
RcppExport SEXP _runner_min_run(SEXP xSEXP, SEXP kSEXP, SEXP na_rmSEXP, SEXP na_padSEXP, SEXP idxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type k(kSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    Rcpp::traits::input_parameter< bool >::type na_pad(na_padSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type idx(idxSEXP);
    rcpp_result_gen = Rcpp::wrap(min_run(x, k, na_rm, na_pad, idx));
    return rcpp_result_gen;
END_RCPP
}
// streak_run
IntegerVector streak_run(SEXP x, IntegerVector k, bool na_rm, bool na_pad, IntegerVector indexes);
RcppExport SEXP _runner_streak_run(SEXP xSEXP, SEXP kSEXP, SEXP na_rmSEXP, SEXP na_padSEXP, SEXP indexesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type k(kSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    Rcpp::traits::input_parameter< bool >::type na_pad(na_padSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type indexes(indexesSEXP);
    rcpp_result_gen = Rcpp::wrap(streak_run(x, k, na_rm, na_pad, indexes));
    return rcpp_result_gen;
END_RCPP
}
// mean_run
NumericVector mean_run(NumericVector x, IntegerVector k, bool na_rm, bool na_pad, IntegerVector idx);
RcppExport SEXP _runner_mean_run(SEXP xSEXP, SEXP kSEXP, SEXP na_rmSEXP, SEXP na_padSEXP, SEXP idxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type k(kSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    Rcpp::traits::input_parameter< bool >::type na_pad(na_padSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type idx(idxSEXP);
    rcpp_result_gen = Rcpp::wrap(mean_run(x, k, na_rm, na_pad, idx));
    return rcpp_result_gen;
END_RCPP
}
// sum_run
NumericVector sum_run(NumericVector x, IntegerVector k, bool na_rm, bool na_pad, IntegerVector idx);
RcppExport SEXP _runner_sum_run(SEXP xSEXP, SEXP kSEXP, SEXP na_rmSEXP, SEXP na_padSEXP, SEXP idxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type k(kSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    Rcpp::traits::input_parameter< bool >::type na_pad(na_padSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type idx(idxSEXP);
    rcpp_result_gen = Rcpp::wrap(sum_run(x, k, na_rm, na_pad, idx));
    return rcpp_result_gen;
END_RCPP
}
// unique_run
SEXP unique_run(SEXP x, IntegerVector k, IntegerVector idx);
RcppExport SEXP _runner_unique_run(SEXP xSEXP, SEXP kSEXP, SEXP idxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type k(kSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type idx(idxSEXP);
    rcpp_result_gen = Rcpp::wrap(unique_run(x, k, idx));
    return rcpp_result_gen;
END_RCPP
}
// whicht_run
IntegerVector whicht_run(LogicalVector x, IntegerVector k, std::string which, bool na_rm, bool na_pad, IntegerVector indexes);
RcppExport SEXP _runner_whicht_run(SEXP xSEXP, SEXP kSEXP, SEXP whichSEXP, SEXP na_rmSEXP, SEXP na_padSEXP, SEXP indexesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< LogicalVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type k(kSEXP);
    Rcpp::traits::input_parameter< std::string >::type which(whichSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    Rcpp::traits::input_parameter< bool >::type na_pad(na_padSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type indexes(indexesSEXP);
    rcpp_result_gen = Rcpp::wrap(whicht_run(x, k, which, na_rm, na_pad, indexes));
    return rcpp_result_gen;
END_RCPP
}
// whichd_run
IntegerVector whichd_run(SEXP x, IntegerVector k, bool na_pad);
RcppExport SEXP _runner_whichd_run(SEXP xSEXP, SEXP kSEXP, SEXP na_padSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type k(kSEXP);
    Rcpp::traits::input_parameter< bool >::type na_pad(na_padSEXP);
    rcpp_result_gen = Rcpp::wrap(whichd_run(x, k, na_pad));
    return rcpp_result_gen;
END_RCPP
}
// window_run
SEXP window_run(SEXP x, IntegerVector k, IntegerVector idx);
RcppExport SEXP _runner_window_run(SEXP xSEXP, SEXP kSEXP, SEXP idxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type k(kSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type idx(idxSEXP);
    rcpp_result_gen = Rcpp::wrap(window_run(x, k, idx));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_runner_fill_run", (DL_FUNC) &_runner_fill_run, 3},
    {"_runner_lag_run", (DL_FUNC) &_runner_lag_run, 3},
    {"_runner_length_run", (DL_FUNC) &_runner_length_run, 2},
    {"_runner_max_run", (DL_FUNC) &_runner_max_run, 5},
    {"_runner_min_run", (DL_FUNC) &_runner_min_run, 5},
    {"_runner_streak_run", (DL_FUNC) &_runner_streak_run, 5},
    {"_runner_mean_run", (DL_FUNC) &_runner_mean_run, 5},
    {"_runner_sum_run", (DL_FUNC) &_runner_sum_run, 5},
    {"_runner_unique_run", (DL_FUNC) &_runner_unique_run, 3},
    {"_runner_whicht_run", (DL_FUNC) &_runner_whicht_run, 6},
    {"_runner_whichd_run", (DL_FUNC) &_runner_whichd_run, 3},
    {"_runner_window_run", (DL_FUNC) &_runner_window_run, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_runner(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
