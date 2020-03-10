#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List DataFrameToList(DataFrame df) {

  int n = df.ncol();
  List var_list(n);
  CharacterVector list_names = df.names();

  for (int i=0; i<n; i++) {
    var_list[i] = df(i);
  }

  var_list.attr("names") = list_names;

  return var_list;
}

/*** R
df <- data.frame(a = c(1, 2, 3),
                 b = c("x", "y", "z"))
DataFrameToList(df)
*/
