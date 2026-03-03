
// [[Rcpp::depends(RcppParallel)]]

// https://chatgpt.com/c/69a4c6cd-9b20-8395-8e03-3f90a97d4401


#include <Rcpp.h>
#include <RcppParallel.h>
#include <vector>
#include <random>
#include <cmath>

using namespace Rcpp;
using namespace RcppParallel;

//
  // -------------------------------------------------------------
  // 1) Punkt-Schätzer
// -------------------------------------------------------------
  //
  
  // [[Rcpp::export]]
double contcoef_table_cpp(IntegerMatrix tab, bool correct = false)
{
  int r = tab.nrow();
  int c = tab.ncol();
  
  double n = 0.0;
  
  for(int i = 0; i < r; ++i)
    for(int j = 0; j < c; ++j)
      n += tab(i,j);
  
  if(n == 0.0)
    return NA_REAL;
  
  std::vector<double> rs(r, 0.0), cs(c, 0.0);
  
  for(int i = 0; i < r; ++i)
    for(int j = 0; j < c; ++j) {
      double v = tab(i,j);
      rs[i] += v;
      cs[j] += v;
    }
  
  double chisq = 0.0;
  
  for(int i = 0; i < r; ++i)
    for(int j = 0; j < c; ++j) {
      
      double expected = rs[i] * cs[j] / n;
      
      if(expected > 0.0) {
        double diff = tab(i,j) - expected;
        chisq += diff * diff / expected;
      }
    }
  
  double cc = std::sqrt(chisq / (chisq + n));
  
  if(correct) {
    int k = std::min(r, c);
    if(k > 1)
      cc /= std::sqrt((k - 1.0) / k);
  }
  
  return cc;
}



//
  // -------------------------------------------------------------
  // 2) Bootstrap Worker (Multinomial)
// -------------------------------------------------------------
  //
  
  struct BootWorkerContCoef : public Worker {
    
    const double* p;        // cell probabilities
    int r;
    int c;
    int n;
    int R;
    unsigned int seed;
    bool correct;
    
    RVector<double> out;
    
    BootWorkerContCoef(const double* p_,
                       int r_,
                       int c_,
                       int n_,
                       int R_,
                       unsigned int seed_,
                       bool correct_,
                       NumericVector out_)
    : p(p_), r(r_), c(c_), n(n_), R(R_),
    seed(seed_), correct(correct_),
    out(out_) {}
    
    void operator()(std::size_t begin,
                    std::size_t end)
    {
      for(std::size_t rr = begin; rr < end; ++rr) {
        
        std::mt19937 rng(seed + rr);
        std::discrete_distribution<int> dist(p, p + r*c);
        
        std::vector<double> tab_star(r*c, 0.0);
        
        for(int i = 0; i < n; ++i) {
          int cell = dist(rng);
          tab_star[cell] += 1.0;
        }
        
        // --- compute statistic on bootstrap table ---
          
          std::vector<double> rs(r,0.0), cs(c,0.0);
        
        for(int i=0;i<r;i++)
          for(int j=0;j<c;j++) {
            double v = tab_star[i*c+j];
            rs[i]+=v;
            cs[j]+=v;
          }
        
        double chisq=0.0;
        
        for(int i=0;i<r;i++)
          for(int j=0;j<c;j++) {
            double e = rs[i]*cs[j]/n;
            if(e>0){
              double d = tab_star[i*c+j]-e;
              chisq += d*d/e;
            }
          }
        
        double cc = std::sqrt(chisq/(chisq+n));
        
        if(correct) {
          int k = std::min(r,c);
          if(k>1)
            cc /= std::sqrt((k-1.0)/k);
        }
        
        out[rr] = cc;
      }
    }
  };



//
  // -------------------------------------------------------------
  // 3) Bootstrap Entry Function
// -------------------------------------------------------------
  //
  
  // [[Rcpp::export]]
NumericVector bootstrap_contcoef_table_cpp(
  IntegerMatrix tab,
  int R = 5000,
  unsigned int seed = 0,
  bool correct = false)
{
  int r = tab.nrow();
  int c = tab.ncol();
  
  int n = 0;
  
  for(int i = 0; i < r; ++i)
    for(int j = 0; j < c; ++j)
      n += tab(i,j);
  
  if(n == 0)
    return NumericVector::create(NA_REAL);
  
  int K = r * c;
  
  std::vector<double> p(K);
  
  for(int i=0;i<r;i++)
    for(int j=0;j<c;j++)
      p[i*c+j] = (double)tab(i,j) / n;
  
  NumericVector out(R);
  
  BootWorkerContCoef worker(
    p.data(),
    r,
    c,
    n,
    R,
    seed,
    correct,
    out);
  
  parallelFor(0, R, worker);
  
  return out;
}