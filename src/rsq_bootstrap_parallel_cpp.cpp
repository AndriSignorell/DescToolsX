

//   Rewritten by Andri Signorell in C++


#include <RcppArmadillo.h>
#include <RcppParallel.h>

// [[Rcpp::depends(RcppArmadillo, RcppParallel)]]

using namespace Rcpp;
using namespace RcppParallel;
using namespace arma;

struct RSqWorker : public Worker {
  
  const RMatrix<double> X;
  const RVector<double> y;
  
  std::size_t n;
  std::size_t p;
  bool adjusted;
  unsigned int base_seed;
  
  RVector<double> stats;
  
  RSqWorker(NumericMatrix X,
            NumericVector y,
            NumericVector stats,
            bool adjusted,
            unsigned int base_seed)
  : X(X), y(y), n(X.nrow()), p(X.ncol() - 1),
  adjusted(adjusted), base_seed(base_seed), stats(stats) {}
  
  
  void operator()(std::size_t begin, std::size_t end) {
    
    std::vector<int> idx(n);
    
    for (size_t b = begin; b < end; b++) {
      
      std::mt19937 rng(base_seed + b);
      std::uniform_int_distribution<int> dist(0, n - 1);
      
      // bootstrap indices
      for (std::size_t i = 0; i < n; i++) {
        idx[i] = dist(rng);
      }
      
      // build bootstrap sample
      std::vector<double> yb(n);
      std::vector<double> fit(n);
      
      for (std::size_t i = 0; i < n; i++) {
        yb[i] = y[idx[i]];
      }
      
      // --- compute mean ---
        double mean_y = 0.0;
      for (std::size_t i = 0; i < n; i++) mean_y += yb[i];
      mean_y /= n;
      
      // --- simple OLS via normal equations ---
        // (X'X)^{-1} X'y
            arma::mat Xb(n, X.ncol());
            arma::vec ybv(n);
            
            for (std::size_t i = 0; i < n; i++) {
              for (std::size_t j = 0; j < X.ncol(); j++){
                Xb(i,j) = X(idx[i], j);
              }
              ybv[i] = y[idx[i]];
            }
            
            arma::vec beta;

            try {
              beta = arma::solve(Xb, ybv);
            } catch (...) {
              stats[b] = NA_REAL;
              continue;
            }
            
            arma::vec yhat = Xb * beta;
            
            double ss_res = 0.0;
            double ss_tot = 0.0;
            
            for (std::size_t i = 0; i < n; i++) {
              double r = ybv[i] - yhat[i];
              ss_res += r * r;
              
              double d = ybv[i] - mean_y;
              ss_tot += d * d;
            }
            
            if (ss_tot == 0.0) {
              stats[b] = NA_REAL;
              continue;
            }
            
            double r2 = 1.0 - ss_res / ss_tot;
            
            if (adjusted) {
              r2 = 1.0 - (1.0 - r2) * (n - 1.0) / (n - p - 1.0);
            }
            
            stats[b] = r2;
    }
  }
};


// [[Rcpp::export]]
NumericVector rsq_bootstrap_parallel_cpp(NumericMatrix X,
                                         NumericVector y,
                                         int B = 1000,
                                         double alpha = 0.05,
                                         bool adjusted = true,
                                         int seed = -1) {
  
  unsigned int base_seed = (seed < 0)
  ? std::random_device{}()
  : (unsigned int)seed;
  
  NumericVector stats(B);
  
  RSqWorker worker(X, y, stats, adjusted, base_seed);
  parallelFor(0, B, worker);
  
  // remove NA
  LogicalVector ok = !is_na(stats);
  NumericVector s = stats[ok];
  
  if (s.size() == 0) {
    stop("All bootstrap samples failed.");
  }
  
  std::sort(s.begin(), s.end());
  
  int n2 = s.size();
  
  int lo = (int)(alpha / 2.0 * n2);
  int hi = (int)((1 - alpha / 2.0) * n2);
  
  lo = std::max(0, std::min(lo, n2 - 1));
  hi = std::max(0, std::min(hi, n2 - 1));
  
  // original R²
  arma::mat Xa = as<arma::mat>(X);
  arma::vec ya = as<arma::vec>(y);
  
  arma::vec beta = arma::solve(Xa, ya);
  arma::vec yhat = Xa * beta;
  
  double mean_y = arma::mean(ya);
  
  double ss_res = arma::accu(arma::square(ya - yhat));
  double ss_tot = arma::accu(arma::square(ya - mean_y));
  
  double r2 = 1.0 - ss_res / ss_tot;
  
  int p = static_cast<int>(X.ncol()) - 1;
  int n = X.nrow();
  
  if (adjusted) {
    r2 = 1.0 - (1.0 - r2) * (n - 1.0) / (n - p - 1.0);
  }
  
  NumericVector out(3);
  out[0] = r2;
  out[1] = s[lo];
  out[2] = s[hi];
  
  out.attr("names") = CharacterVector::create("est","lci","uci");
  
  return out;
}