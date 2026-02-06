

#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
List n_pow_sum(NumericVector x) {
  
  // x must be a nonempty numeric vector with NAs omitted
  
  std::map<double, int> counts;
  typedef std::map<double, int>::iterator it_type;
  
  // int n = x.size();
  for (int i = 0; i < x.size(); i++) {
    counts[x[i]]++;
  }
  
  // double mean = 0;
  // typedef std::map<double, int>::iterator it_type;
  // for(it_type iterator = counts.begin(); iterator != counts.end(); iterator++) {
    // 
      //   mean += iterator->second * iterator->first;
      // 
        //   // iterator->first = key
        //   // iterator->second = value
        // }
  // mean = mean / n;
  
  double mean =  std::reduce(x.begin(), x.end(), 0.0, std::plus<double>()) / x.size();
  
  // int un = counts.size();
  int zn = 0;
  int un = 0;
  double d = 0;
  double d2 = 0;
  
  double sum1 = 0;
  double sum2 = 0;
  double sum3 = 0;
  double sum4 = 0;
  
  un = counts.size();
  
  for(it_type iterator = counts.begin(); iterator != counts.end(); iterator++) {
    
    d = iterator->first - mean;
    d2 = iterator->second * d * d;
    
    sum1 += abs(iterator->second * d);  // sum of absolute difference
    sum2 += d2;      // sum of squares
    sum3 += d2*d;    // sum of 3th powers
    sum4 += d2*d*d;  // sum of 4th powers
    
    if(iterator->first == 0) zn = iterator->second;   // number of zero values
  }
  
  unsigned int ldim = 5;    // dimension of small/large values vectors
  if(counts.size() < ldim) { ldim = counts.size(); }
  
  NumericVector small_val(ldim);    // the 5 smallest values
  NumericVector small_freq(ldim);   // the frequency of the 5 smallest values
  NumericVector large_val(ldim);    // the 5 smallest values
  NumericVector large_freq(ldim);   // the frequency of the 5 smallest values
  
  unsigned int i=0;
  for(it_type iterator = counts.begin(); iterator != counts.end(); iterator++, i++) {
    small_val[i] = iterator->first;
    small_freq[i] = iterator->second;
    if(i == ldim -1){
      break;
    }
  }
  
  i=0;
  typedef std::map<double, int>::reverse_iterator it_rev_type;
  for(it_rev_type iterator = counts.rbegin(); iterator != counts.rend(); iterator++, i++) {
    large_val[i] = iterator->first;
    large_freq[i] = iterator->second;
    if(i == ldim -1){
      break;
    }
  }
  
  return Rcpp::List::create(
    Rcpp::Named("mean", mean),
    Rcpp::Named("sum1", sum1),
    Rcpp::Named("sum2", sum2),
    Rcpp::Named("sum3", sum3),
    Rcpp::Named("sum4", sum4),
    Rcpp::Named("zero", zn),
    Rcpp::Named("unique", un),
    Rcpp::Named("small_val", small_val),
    Rcpp::Named("small_freq", small_freq),
    Rcpp::Named("large_val", large_val),
    Rcpp::Named("large_freq", large_freq)
  );
  
  
}


