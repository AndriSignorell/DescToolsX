
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
LogicalVector between_num(NumericVector x,
                          NumericVector from,
                          NumericVector to,
                          bool left_closed = true,
                          bool right_closed = true) {
  
  int n = x.size();
  LogicalVector res(n);
  
  bool scalar = (from.size() == 1);
  
  for (int i = 0; i < n; ++i) {
    double lo = scalar ? from[0] : from[i];
    double hi = scalar ? to[0]   : to[i];
    
    bool ok = true;
    ok &= left_closed  ? x[i] >= lo : x[i] > lo;
    ok &= right_closed ? x[i] <= hi : x[i] < hi;
    
    res[i] = ok;
  }
  return res;
}

