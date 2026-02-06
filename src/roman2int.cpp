

#include <Rcpp.h>
using namespace Rcpp;

/* -----------------------------------------------------------
  helper: convert single Roman character to value
----------------------------------------------------------- */
  inline int roman_value(char c)
{
  switch(c) {
    case 'I': return 1;
    case 'V': return 5;
    case 'X': return 10;
    case 'L': return 50;
    case 'C': return 100;
    case 'D': return 500;
    case 'M': return 1000;
    default:
      stop("Invalid roman numeral '%c'", c);
  }
  return NA_INTEGER; // never reached
  }

  
/* -----------------------------------------------------------
  convert one roman string to int
----------------------------------------------------------- */
  int roman2int_one(const std::string& s)
{
  int n = s.size();
  if(n < 1)
    return NA_INTEGER;
  
  int total = 0;
  
  for(int i = 0; i < n - 1; i++) {
    int v1 = roman_value(s[i]);
    int v2 = roman_value(s[i + 1]);
    
    if(v1 < v2)
      total -= v1;
      else
        total += v1;
  }
  
  total += roman_value(s[n - 1]);
  return total;
  }

  
/* -----------------------------------------------------------
  Rcpp export (vectorised)
----------------------------------------------------------- */

// [[Rcpp::export]]
IntegerVector roman2int_cpp(CharacterVector x)
{
  int n = x.size();
  IntegerVector res(n);
  
  for(int i = 0; i < n; i++) {
    if(x[i] == NA_STRING) {
      res[i] = NA_INTEGER;
    } else {
      std::string s = as<std::string>(x[i]);
      res[i] = roman2int_one(s);
    }
  }
  
  return res;
}
  