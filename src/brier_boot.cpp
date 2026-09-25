
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::vec brier_boot_cpp(const arma::vec& resp,
                         const arma::vec& pred,
                         int R,
                         bool scaled) {
  
  int n = resp.n_elem;
  arma::vec out(R);
  
  for(int r = 0; r < R; ++r) {
    
    arma::uvec idx = arma::randi<arma::uvec>(n, arma::distr_param(0, n - 1));
    
    arma::vec y = resp.elem(idx);
    arma::vec p = pred.elem(idx);
    
    arma::vec term = y % arma::square(1 - p) +
      (1 - y) % arma::square(p);
    
    double bs = arma::mean(term);
    
    if(scaled) {
      double mean_y = arma::mean(y);
      double Bmax = mean_y * pow(1 - mean_y, 2) +
        (1 - mean_y) * pow(mean_y, 2);
      // A resample with a constant response has Bmax == 0 and no scaled
      // score; -Inf or NaN made quantile() on the R side fail. NA is
      // dropped there with a warning.
      bs = (Bmax > 0) ? 1.0 - bs / Bmax : NA_REAL;
    }
    
    out[r] = bs;
  }
  
  return out;
}
