
#include <Rcpp.h>
using namespace Rcpp;


// --------------------------------
// least common multiple

// [[Rcpp::export]]
long long compute_LCM(long long int a, long long int b){
  return
  std::lcm(a,b);
}

// --------------------------------
// greatest common divisor

// [[Rcpp::export]]
long long compute_GCD(long long int a, long long int b){
  return
  std::gcd(a,b);
}


// --------------------------------
// Divisors

//[[Rcpp::export]]
IntegerVector divs( int x ){
  IntegerVector d = seq_len( x / 2 );
  IntegerVector out(0);
  for( int i = 0 ; i < d.size(); i++){
    if( x % d[i] == 0 )
      out.push_back( d[i] );
  }
  return out;
}

