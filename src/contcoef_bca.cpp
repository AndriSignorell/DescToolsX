

// [[Rcpp::depends(RcppParallel)]]

#include <Rcpp.h>
#include <RcppParallel.h>
#include <vector>
#include <random>
#include <cmath>
#include <algorithm>

using namespace Rcpp;
using namespace RcppParallel;

inline double compute_cc(
  const std::vector<double>& tab,
  int r, int c, double n,
  bool correct)
{
  std::vector<double> rs(r,0.0), cs(c,0.0);
  
  for(int i=0;i<r;i++)
    for(int j=0;j<c;j++){
      double v = tab[i*c+j];
      rs[i]+=v;
      cs[j]+=v;
    }
  
  double chisq=0.0;
  
  for(int i=0;i<r;i++)
    for(int j=0;j<c;j++){
      double e = rs[i]*cs[j]/n;
      if(e>0){
        double d = tab[i*c+j]-e;
        chisq+=d*d/e;
      }
    }
  
  double cc = std::sqrt(chisq/(chisq+n));
  
  if(correct){
    int k = std::min(r,c);
    if(k>1)
      cc/=std::sqrt((k-1.0)/k);
  }
  
  return cc;
}


// [[Rcpp::export]]
List contcoef_table_boot_bca_cpp(
  IntegerMatrix tab,
  int R,
  unsigned int seed,
  bool correct,
  double conf_level)
{
  int r = tab.nrow();
  int c = tab.ncol();
  
  int n = 0;
  for(int i=0;i<r;i++)
    for(int j=0;j<c;j++)
      n += tab(i,j);
  
  int K = r*c;
  
  std::vector<double> p(K);
  std::vector<double> tab0(K);
  
  for(int i=0;i<r;i++)
    for(int j=0;j<c;j++){
      double v = tab(i,j);
      tab0[i*c+j]=v;
      p[i*c+j]=v/n;
    }
  
  double theta_hat = compute_cc(tab0,r,c,n,correct);
  
  // -----------------------------
    // Bootstrap
  // -----------------------------
    NumericVector boot(R);
  
  for(int rr=0; rr<R; rr++){
    
    std::mt19937 rng(seed+rr);
    std::discrete_distribution<int> dist(p.begin(), p.end());
    
    std::vector<double> tab_star(K,0.0);
    
    for(int i=0;i<n;i++){
      int cell = dist(rng);
      tab_star[cell]+=1.0;
    }
    
    boot[rr] = compute_cc(tab_star,r,c,n,correct);
  }
  
  // -----------------------------
    // z0
  // -----------------------------
    int count=0;
  for(int i=0;i<R;i++)
    if(boot[i]<theta_hat) count++;
  
  double z0 = R::qnorm((double)count/R,0,1,1,0);
  
  // -----------------------------
    // Jackknife acceleration
  // -----------------------------
    std::vector<double> jack(n);
  
  int idx=0;
  
  for(int i=0;i<r;i++)
    for(int j=0;j<c;j++){
      
      int k = tab(i,j);
      
      for(int m=0;m<k;m++){
        
        tab0[i*c+j] -= 1.0;
        
        jack[idx++] =
          compute_cc(tab0,r,c,n-1,correct);
        
        tab0[i*c+j] += 1.0;
      }
    }
  
  double mean_j=0.0;
  for(int i=0;i<n;i++)
    mean_j+=jack[i];
  mean_j/=n;
  
  double num=0.0, den=0.0;
  
  for(int i=0;i<n;i++){
    double d = mean_j-jack[i];
    num+=d*d*d;
    den+=d*d;
  }
  
  double a = num/(6.0*std::pow(den,1.5));
  
  // -----------------------------
    // Adjusted quantiles
  // -----------------------------
    double alpha = 1.0-conf_level;
  
  double zL = R::qnorm(alpha/2,0,1,1,0);
  double zU = R::qnorm(1-alpha/2,0,1,1,0);
  
  double adjL =
    R::pnorm(z0+(z0+zL)/(1-a*(z0+zL)),0,1,1,0);
  
  double adjU =
    R::pnorm(z0+(z0+zU)/(1-a*(z0+zU)),0,1,1,0);
  
  std::sort(boot.begin(), boot.end());
  
  int lo = std::floor(adjL*R);
  int hi = std::floor(adjU*R);
  
  return List::create(
    Named("estimate")=theta_hat,
    Named("conf.low")=boot[lo],
    Named("conf.high")=boot[hi]
  );
}

