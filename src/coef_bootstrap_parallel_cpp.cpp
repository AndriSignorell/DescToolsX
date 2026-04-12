

#include <RcppArmadillo.h>
#include <RcppParallel.h>
#include <random>

// [[Rcpp::depends(RcppArmadillo, RcppParallel)]]

using namespace Rcpp;
using namespace RcppParallel;
using namespace arma;

struct CoefWorker : public Worker {
  
  const RMatrix<double> X;
  const RVector<double> y;
  
  std::size_t n;
  std::size_t p;
  unsigned int base_seed;
  
  RMatrix<double> stats;
  
  CoefWorker(NumericMatrix X,
             NumericVector y,
             NumericMatrix stats,
             unsigned int base_seed)
  : X(X), y(y),
  n(X.nrow()), p(X.ncol()),
  base_seed(base_seed),
  stats(stats) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    
    std::vector<int> idx(n);
    
    for (std::size_t b = begin; b < end; b++) {
      
      std::mt19937 rng(base_seed + b);
      std::uniform_int_distribution<int> dist(0, n - 1);
      
      for (std::size_t i = 0; i < n; i++) {
        idx[i] = dist(rng);
      }
      
      arma::mat Xb(n, p);
      arma::vec yb(n);
      
      for (std::size_t i = 0; i < n; i++) {
        for (std::size_t j = 0; j < p; j++) {
          Xb(i,j) = X(idx[i], j);
        }
        yb[i] = y[idx[i]];
      }
      
      try {
        arma::vec beta = arma::solve(Xb, yb);
        
        for (std::size_t j = 0; j < p; j++) {
          stats(b, j) = beta[j];
        }
        
      } catch (...) {
        for (std::size_t j = 0; j < p; j++) {
          stats(b, j) = NA_REAL;
        }
      }
    }
  }
};


// [[Rcpp::export]]
NumericMatrix coef_bootstrap_parallel_cpp(NumericMatrix X,
                                          NumericVector y,
                                          int B = 2000,
                                          double alpha = 0.05,
                                          int seed = -1) {
  
  unsigned int base_seed = (seed < 0)
  ? std::random_device{}()
  : (unsigned int)seed;
  
  std::size_t p = X.ncol();
  
  NumericMatrix stats(B, p);
  
  CoefWorker worker(X, y, stats, base_seed);
  parallelFor(0, B, worker);
  
  // --- original estimate ---
    arma::mat Xa = as<arma::mat>(X);
    arma::vec ya = as<arma::vec>(y);
    
    arma::vec beta0 = arma::solve(Xa, ya);
    
    NumericMatrix out(p, 3);
    colnames(out) = CharacterVector::create("est", "lci", "uci");
    
    for (std::size_t j = 0; j < p; j++) {
      
      NumericVector s = stats(_, j);
      s = s[!is_na(s)];
      
      if (s.size() == 0) {
        out(j, 0) = beta0[j];
        out(j, 1) = NA_REAL;
        out(j, 2) = NA_REAL;
        continue;
      }
      
      std::sort(s.begin(), s.end());
      
      int n2 = s.size();
      
      int lo = std::max(0, std::min((int)(alpha/2 * n2), n2 - 1));
      int hi = std::max(0, std::min((int)((1 - alpha/2) * n2), n2 - 1));
      
      out(j, 0) = beta0[j];
      out(j, 1) = s[lo];
      out(j, 2) = s[hi];
    }
    
    return out;
}

