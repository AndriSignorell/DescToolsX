
#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

/* 
 AnDarl.c
 
 $Revision: 1.2 $  $Date: 2018/03/29 08:07:08 $
 
 Original C code by G. and J. Marsaglia
 
 R interface by Adrian Baddeley
 
 Rcpp port Andri Signorell  2026/02/05
 
 */



/* ============================================================
  Complementary normal distribution function
============================================================ */
  
  double cPhi(double x) {
    static const long double v[] = {
      1.25331413731550025L,  .421369229288054473L,  .236652382913560671L,
      .162377660896867462L,  .123131963257932296L,  .0990285964717319214L,
      .0827662865013691773L, .0710695805388521071L, .0622586659950261958L
    };
    
    double h, a, b, z, t, s, pwr;
    int i, j;
    
    j = (std::fabs(x) + 1.0) / 2.0;
    a = v[j];
    z = 2.0 * j;
    h = std::fabs(x) - z;
    
    b = z * a - 1.0;
    pwr = 1.0;
    s = a + h * b;
    
    for (i = 2; i < 100; i += 2) {
      a = (a + z * b) / i;
      b = (b + z * a) / (i + 1);
      pwr *= h * h;
      t = s;
      s += pwr * (a + h * b);
      if (s == t) {
        s *= std::exp(-0.5 * x * x - 0.91893853320467274178);
        return (x > 0) ? s : 1.0 - s;
      }
    }
    return (x > 0) ? s : 1.0 - s;
  }

/* ============================================================
  ADinf helpers
============================================================ */
  
  double ADf(double z, int j) {
    double t, f, fnew, a, b, c, r;
    int i;
    
    t = (4 * j + 1) * (4 * j + 1) * 1.23370055013617 / z;
    if (t > 150.0) return 0.0;
    
    a = 2.22144146907918 * std::exp(-t) / std::sqrt(t);
    b = 3.93740248643060 * 2.0 * cPhi(std::sqrt(2.0 * t));
    
    r = z * 0.125;
    f = a + b * r;
    
    for (i = 1; i < 200; i++) {
      c = ((i - 0.5 - t) * b + t * a) / i;
      a = b;
      b = c;
      r *= z / (8.0 * i + 8.0);
      if (std::fabs(r) < 1e-40 || std::fabs(c) < 1e-40) return f;
      fnew = f + c * r;
      if (f == fnew) return f;
      f = fnew;
    }
    return f;
  }

double ADinf(double z) {
  if (z < 0.01) return 0.0;
  
  double ad, adnew, r;
  r = 1.0 / z;
  ad = r * ADf(z, 0);
  
  for (int j = 1; j < 100; j++) {
    r *= (0.5 - j) / j;
    adnew = ad + (4 * j + 1) * r * ADf(z, j);
    if (ad == adnew) return ad;
    ad = adnew;
  }
  return ad;
}

/* ============================================================
  Asymptotic approximation
============================================================ */
  
  double adinf(double z) {
    if (z < 2.0) {
      return std::exp(-1.2337141 / z) / std::sqrt(z) *
        (2.00012 +
           (0.247105 -
              (0.0649821 -
                 (0.0347962 -
                    (0.011672 - 0.00168691 * z) * z) * z) * z) * z);
    }
    
    return std::exp(-std::exp(
      1.0776 -
        (2.30695 -
           (0.43424 -
              (0.082433 -
                 (0.008056 - 0.0003146 * z) * z) * z) * z) * z));
  }

/* ============================================================
  Error correction
============================================================ */
  
  double errfix(int n, double x) {
    double c, t;
    
    if (x > 0.8) {
      return (-130.2137 +
                (745.2337 -
                   (1705.091 -
                      (1950.646 -
                         (1116.360 - 255.7844 * x) * x) * x) * x) * x) / n;
    }
    
    c = 0.01265 + 0.1757 / n;
    if (x < c) {
      t = x / c;
      t = std::sqrt(t) * (1.0 - t) * (49.0 * t - 102.0);
      return t * (0.0037 / (n * n) + 0.00078 / n + 0.00006) / n;
    }
    
    t = (x - c) / (0.8 - c);
    t = -0.00022633 +
      (6.54034 -
         (14.6538 -
            (14.458 -
               (8.259 - 1.91864 * t) * t) * t) * t) * t;
    
    return t * (0.04213 + 0.01365 / n) / n;
  }

/* ============================================================
  Finite-n distribution
============================================================ */
  
  double AD(int n, double z) {
    double x = adinf(z);
    return x + errfix(n, x);
  }

/* ============================================================
  Test statistic & p-value
============================================================ */
  
  // [[Rcpp::export]]
double ADstat(NumericVector x) {
  int n = x.size();
  double z = 0.0;
  
  for (int i = 0; i < n; i++) {
    double t = x[i] * (1.0 - x[n - 1 - i]);
    z -= (2 * i + 1) * std::log(t);
  }
  return -n + z / n;
}

// [[Rcpp::export]]
double ADtest(NumericVector x) {
  int n = x.size();
  double a = ADstat(x);
  double p = AD(n, a);
  return 1.0 - p;
}

// [[Rcpp::export]]
NumericVector ADprobExactInf(NumericVector a) {
  int m = a.size();
  NumericVector prob(m);
  for (int i = 0; i < m; i++) prob[i] = ADinf(a[i]);
  return prob;
}

// [[Rcpp::export]]
NumericVector ADprobApproxInf(NumericVector a) {
  int m = a.size();
  NumericVector prob(m);
  for (int i = 0; i < m; i++) prob[i] = adinf(a[i]);
  return prob;
}

// [[Rcpp::export]]
NumericVector ADprobN(NumericVector a, int n) {
  int m = a.size();
  NumericVector prob(m);
  for (int i = 0; i < m; i++) prob[i] = AD(n, a[i]);
  return prob;
}

  
// [[Rcpp::export]]
Rcpp::List ADtestR(const Rcpp::NumericVector& x) {
  const int n = x.size();
  
  double a = ADstat(x);      // Teststatistik
  double p = AD(n, a);       // Verteilungsfunktion
  
  return Rcpp::List::create(
    Rcpp::Named("adstat") = a,
    Rcpp::Named("pvalue") = 1.0 - p
  );
}
  
  