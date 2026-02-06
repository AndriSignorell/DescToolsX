

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double rskew_cpp(NumericVector x, double mean) {
  
  int n = x.size();
  double sum2 = 0.0, sum3 = 0.0, d;
  
  for (int i = 0; i < n; ++i) {
    d = x[i] - mean;
    sum2 += d * d;
    sum3 += d * d * d;
  }
  
  return ( (sum3 / n) / std::pow(sum2 / n, 1.5) );
}


// [[Rcpp::export]]
double rskeww_cpp(NumericVector x, double mean, NumericVector w) {
  
  int n = x.size();
  double sum2 = 0.0, sum3 = 0.0, wsum = 0.0, d;
  
  for (int i = 0; i < n; ++i) {
    d = x[i] - mean;
    wsum += w[i];
    sum2 += d * d * w[i];
    sum3 += d * d * d * w[i];
  }
  
  return ( (sum3 / wsum) / std::pow(sum2 / wsum, 1.5) );
}


// [[Rcpp::export]]
double rkurt_cpp(NumericVector x, double mean) {
  
  int n = x.size();
  double sum2 = 0.0, sum4 = 0.0, d;
  
  for (int i = 0; i < n; ++i) {
    d = x[i] - mean;
    sum2 += d * d;
    sum4 += d * d * d * d;
  }
  
  return ( (sum4 / n) / std::pow(sum2 / n, 2.0) ) - 3.0;
}


// [[Rcpp::export]]
double rkurtw_cpp(NumericVector x, double mean, NumericVector w) {
  
  int n = x.size();
  double sum2 = 0.0, sum4 = 0.0, wsum = 0.0, d;
  
  for (int i = 0; i < n; ++i) {
    d = x[i] - mean;
    wsum += w[i];
    sum2 += d * d * w[i];
    sum4 += d * d * d * d * w[i];
  }
  
  return ( (sum4 / wsum) / std::pow(sum2 / wsum, 2.0) ) - 3.0;
}
