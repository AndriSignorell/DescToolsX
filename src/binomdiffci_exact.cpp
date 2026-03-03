#include <Rcpp.h>
using namespace Rcpp;

// restricted log-likelihood
double loglik(int x1, int n1,
              int x2, int n2,
              double p2, double delta) {
  
  double p1 = p2 + delta;
  if (p1 < 0.0 || p1 > 1.0) return R_NegInf;
  
  return R::dbinom(x1,n1,p1,true) +
    R::dbinom(x2,n2,p2,true);
}


// find restricted MLE of p2 under delta
double restricted_mle(int x1, int n1,
                      int x2, int n2,
                      double delta) {
  
  double best_p2 = 0.0;
  double best_ll = R_NegInf;
  
  for (int g=0; g<400; ++g) {
    
    double p2 = (double)g/399.0;
    double ll = loglik(x1,n1,x2,n2,p2,delta);
    
    if (ll > best_ll) {
      best_ll = ll;
      best_p2 = p2;
    }
  }
  
  return best_p2;
}


// score statistic
double score_stat(int x1, int n1,
                  int x2, int n2,
                  double delta,
                  double p2_hat) {
  
  double p1_hat = p2_hat + delta;
  
  double num =
    (double)x1/n1 - (double)x2/n2 - delta;
  
  double var =
    p1_hat*(1.0-p1_hat)/n1 +
    p2_hat*(1.0-p2_hat)/n2;
  
  return num / sqrt(var);
}


// p-value for given delta
double pvalue_delta_sas(double delta,
                        int x1, int n1,
                        int x2, int n2) {
  
  double max_p = 0.0;
  
  for (int g=0; g<200; ++g) {
    
    double p2 = (double)g/199.0;
    double p1 = p2 + delta;
    
    if (p1 < 0.0 || p1 > 1.0) continue;
    
    double p2_hat = restricted_mle(x1,n1,x2,n2,delta);
    double Z_obs = fabs(score_stat(x1,n1,x2,n2,
                                   delta,p2_hat));
    
    double tail = 0.0;
    
    for (int i=0; i<=n1; ++i) {
      for (int j=0; j<=n2; ++j) {
        
        double Z_ij =
          fabs(score_stat(i,n1,j,n2,
                          delta,p2_hat));
        
        if (Z_ij >= Z_obs) {
          
          tail +=
            R::dbinom(i,n1,p1,false) *
            R::dbinom(j,n2,p2,false);
        }
      }
    }
    
    if (tail > max_p)
      max_p = tail;
  }
  
  return max_p;
}


// [[Rcpp::export]]
NumericVector bdci_exact_sas_rcpp(int x1, int n1,
                                  int x2, int n2,
                                  double alpha=0.05) {
  
  double delta_hat =
    (double)x1/n1 - (double)x2/n2;
  
  double lower=-1.0, upper=1.0;
  
  // lower bound
  double a=-1.0, b=delta_hat;
  for (int iter=0; iter<40; ++iter) {
    
    double mid = 0.5*(a+b);
    double p =
      pvalue_delta_sas(mid,x1,n1,x2,n2);
    
    if (p >= alpha)
      b = mid;
    else
      a = mid;
  }
  lower = 0.5*(a+b);
  
  // upper bound
  a=delta_hat; b=1.0;
  for (int iter=0; iter<40; ++iter) {
    
    double mid = 0.5*(a+b);
    double p =
      pvalue_delta_sas(mid,x1,n1,x2,n2);
    
    if (p >= alpha)
      a = mid;
    else
      b = mid;
  }
  upper = 0.5*(a+b);
  
  return NumericVector::create(
    _["estimate"]=delta_hat,
    _["lwr.ci"]=std::max(-1.0,lower),
    _["upr.ci"]=std::min(1.0,upper)
  );
}

