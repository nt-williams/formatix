// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// DataFrameToList
List DataFrameToList(DataFrame df);
RcppExport SEXP _formatix_DataFrameToList(SEXP dfSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type df(dfSEXP);
    rcpp_result_gen = Rcpp::wrap(DataFrameToList(df));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_formatix_DataFrameToList", (DL_FUNC) &_formatix_DataFrameToList, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_formatix(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
