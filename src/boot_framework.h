
#pragma once

// ============================================================
// boot_framework.h
//
// Generic parallel bootstrap framework using RcppParallel.
//
// To add a new bootstrapped statistic:
//
//   1. Create a new .cpp file
//   2. #include "boot_framework.h"
//   3. Define a StatFn struct with:
//        double compute(const arma::mat& X, const arma::vec& y) const
//      or for vector-valued statistics:
//        arma::vec compute(const arma::mat& X, const arma::vec& y) const
//   4. Call run_boot() or run_boot_matrix()
//
// Supported CI methods: "perc", "bca"
//
// ============================================================

#include <RcppArmadillo.h>
#include <RcppParallel.h>
#include <random>
#include <cmath>

// [[Rcpp::depends(RcppArmadillo, RcppParallel)]]

using namespace Rcpp;
using namespace RcppParallel;
using namespace arma;


// ============================================================
// scalar bootstrap worker
//
// StatFn must implement:
//   double compute(const arma::mat& X, const arma::vec& y) const
//
// ============================================================

template <typename StatFn>
struct ScalarBootWorker : public Worker {
  
  const RMatrix<double> X;
  const RVector<double> y;
  const std::size_t     n;
  const std::size_t     p;
  unsigned int          base_seed;
  RVector<double>       stats;
  StatFn                fn;
  
  ScalarBootWorker(NumericMatrix X,
                   NumericVector y,
                   NumericVector stats,
                   unsigned int  base_seed,
                   StatFn        fn)
    : X(X), y(y),
      n(X.nrow()),
      p(X.ncol()),
      base_seed(base_seed),
      stats(stats),
      fn(fn) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    
    std::vector<std::size_t> idx(n);
    
    for (std::size_t b = begin; b < end; b++) {
      
      // thread-local RNG seeded per iteration
      std::mt19937 rng(base_seed + static_cast<unsigned>(b));
      std::uniform_int_distribution<std::size_t> dist(0, n - 1);
      
      for (std::size_t i = 0; i < n; i++)
        idx[i] = dist(rng);
      
      arma::mat Xb(n, p);
      arma::vec yb(n);
      
      for (std::size_t i = 0; i < n; i++) {
        for (std::size_t j = 0; j < p; j++)
          Xb(i, j) = X(idx[i], j);
        yb[i] = y[idx[i]];
      }
      
      try {
        stats[b] = fn.compute(Xb, yb);
      } catch (...) {
        stats[b] = NA_REAL;
      }
    }
  }
};


// ============================================================
// matrix bootstrap worker
//
// StatFn must implement:
//   arma::vec compute(const arma::mat& X, const arma::vec& y) const
//
// Used for vector-valued statistics (e.g. regression coefficients).
//
// ============================================================

template <typename StatFn>
struct MatrixBootWorker : public Worker {
  
  const RMatrix<double> X;
  const RVector<double> y;
  const std::size_t     n;
  const std::size_t     p;
  const std::size_t     k;     // number of output statistics
  unsigned int          base_seed;
  RMatrix<double>       stats; // R x k
  StatFn                fn;
  
  MatrixBootWorker(NumericMatrix X,
                   NumericVector y,
                   NumericMatrix stats,
                   unsigned int  base_seed,
                   StatFn        fn)
    : X(X), y(y),
      n(X.nrow()),
      p(X.ncol()),
      k(stats.ncol()),
      base_seed(base_seed),
      stats(stats),
      fn(fn) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    
    std::vector<std::size_t> idx(n);
    
    for (std::size_t b = begin; b < end; b++) {
      
      std::mt19937 rng(base_seed + static_cast<unsigned>(b));
      std::uniform_int_distribution<std::size_t> dist(0, n - 1);
      
      for (std::size_t i = 0; i < n; i++)
        idx[i] = dist(rng);
      
      arma::mat Xb(n, p);
      arma::vec yb(n);
      
      for (std::size_t i = 0; i < n; i++) {
        for (std::size_t j = 0; j < p; j++)
          Xb(i, j) = X(idx[i], j);
        yb[i] = y[idx[i]];
      }
      
      try {
        arma::vec result = fn.compute(Xb, yb);
        for (std::size_t j = 0; j < k; j++)
          stats(b, j) = result[j];
      } catch (...) {
        for (std::size_t j = 0; j < k; j++)
          stats(b, j) = NA_REAL;
      }
    }
  }
};


// ============================================================
// quantile type 7 (same as R default)
// ============================================================

inline double quantile_type7(const NumericVector& x, double p) {
  
  int n = x.size();
  
  if (n == 1)
    return x[0];
  
  double h    = (n - 1) * p;
  int    i    = (int)std::floor(h);
  double frac = h - i;
  
  if (i >= n - 1)
    return x[n - 1];
  
  return x[i] + frac * (x[i + 1] - x[i]);
}


// ============================================================
// percentile CI from sorted bootstrap distribution
// ============================================================

inline NumericVector boot_percentile_ci(NumericVector stats,
                                        double        est,
                                        double        alpha) {
  
  LogicalVector ok = !is_na(stats);
  NumericVector s  = stats[ok];
  
  if (s.size() == 0)
    Rcpp::stop("All bootstrap samples failed.");
  
  std::sort(s.begin(), s.end());
  
  NumericVector out(3);
  out[0] = est;
  out[1] = quantile_type7(s, alpha / 2.0);
  out[2] = quantile_type7(s, 1.0 - alpha / 2.0);
  out.attr("names") = CharacterVector::create("est", "lci", "uci");
  
  return out;
}


// ============================================================
// BCa helpers
// ============================================================

// bias correction z0 from bootstrap distribution
inline double bca_z0(const NumericVector& s, double est) {
  
  int count_less = 0;
  int B          = s.size();
  
  for (int i = 0; i < B; i++){
    if (s[i] < est) count_less++;
  }
    
    double prop = static_cast<double>(count_less) / B;
    prop = std::max(1e-10, std::min(1.0 - 1e-10, prop));
    
    return R::qnorm(prop, 0.0, 1.0, 1, 0);
}

// jackknife acceleration constant from leave-one-out estimates
inline double bca_acceleration(const NumericVector& jack) {
  
  LogicalVector okj = !is_na(jack);
  NumericVector jv  = jack[okj];
  
  if ((int)jv.size() < 2)
    Rcpp::stop("Jackknife failed: too few valid leave-one-out estimates.");
  
  double jmean = mean(jv);
  double num   = 0.0;
  double den   = 0.0;
  
  for (int i = 0; i < (int)jv.size(); i++) {
    double d  = jmean - jv[i];
    num += d * d * d;
    den += d * d;
  }
  
  if (den == 0.0) return 0.0;
  
  return num / (6.0 * std::pow(den, 1.5));
}

// adjusted quantile levels from z0 and acceleration
inline std::pair<double, double> bca_quantile_levels(double z0,
                                                     double acc,
                                                     double alpha) {
  
  double zal = R::qnorm(alpha / 2.0,       0.0, 1.0, 1, 0);
  double zau = R::qnorm(1.0 - alpha / 2.0, 0.0, 1.0, 1, 0);
  
  double adjl = R::pnorm(
    z0 + (z0 + zal) / (1.0 - acc * (z0 + zal)),
    0.0, 1.0, 1, 0
  );
  
  double adju = R::pnorm(
    z0 + (z0 + zau) / (1.0 - acc * (z0 + zau)),
    0.0, 1.0, 1, 0
  );
  
  return { adjl, adju };
}

// jackknife leave-one-out estimates for scalar StatFn
template <typename StatFn>
inline NumericVector jackknife_scalar(NumericMatrix X,
                                      NumericVector y,
                                      StatFn        fn) {
  
  int n = X.nrow();
  int p = X.ncol();
  
  NumericVector jack(n);
  
  for (int i = 0; i < n; i++) {
    
    arma::mat Xj(n - 1, p);
    arma::vec yj(n - 1);
    int row = 0;
    
    for (int r = 0; r < n; r++) {
      if (r == i) continue;
      for (int c = 0; c < p; c++)
        Xj(row, c) = X(r, c);
      yj[row] = y[r];
      row++;
    }
    
    try {
      jack[i] = fn.compute(Xj, yj);
    } catch (...) {
      jack[i] = NA_REAL;
    }
  }
  
  return jack;
}

// jackknife leave-one-out estimates for vector StatFn (n x k matrix)
template <typename StatFn>
inline NumericMatrix jackknife_matrix(NumericMatrix X,
                                      NumericVector y,
                                      int           k,
                                      StatFn        fn) {
  
  int n = X.nrow();
  int p = X.ncol();
  
  NumericMatrix jack(n, k);
  
  for (int i = 0; i < n; i++) {
    
    arma::mat Xj(n - 1, p);
    arma::vec yj(n - 1);
    int row = 0;
    
    for (int r = 0; r < n; r++) {
      if (r == i) continue;
      for (int c = 0; c < p; c++)
        Xj(row, c) = X(r, c);
      yj[row] = y[r];
      row++;
    }
    
    try {
      arma::vec jval = fn.compute(Xj, yj);
      for (int j = 0; j < k; j++)
        jack(i, j) = jval[j];
    } catch (...) {
      for (int j = 0; j < k; j++)
        jack(i, j) = NA_REAL;
    }
  }
  
  return jack;
}


// ============================================================
// BCa CI: scalar statistic
// ============================================================

template <typename StatFn>
inline NumericVector boot_bca_ci(NumericVector stats,
                                 NumericMatrix X,
                                 NumericVector y,
                                 double        est,
                                 double        alpha,
                                 StatFn        fn) {
  
  LogicalVector ok = !is_na(stats);
  NumericVector s  = stats[ok];
  
  if (s.size() == 0)
    Rcpp::stop("All bootstrap samples failed.");
  
  std::sort(s.begin(), s.end());
  
  double z0  = bca_z0(s, est);
  double acc = bca_acceleration(jackknife_scalar(X, y, fn));
  
  auto levels = bca_quantile_levels(z0, acc, alpha);
  
  NumericVector out(3);
  out[0] = est;
  out[1] = quantile_type7(s, levels.first);
  out[2] = quantile_type7(s, levels.second);
  out.attr("names") = CharacterVector::create("est", "lci", "uci");
  
  return out;
}


// ============================================================
// BCa CI: vector statistic (one BCa interval per parameter)
// ============================================================

template <typename StatFn>
inline NumericMatrix boot_bca_ci_matrix(NumericMatrix stats,
                                        NumericMatrix X,
                                        NumericVector y,
                                        arma::vec     est,
                                        double        alpha,
                                        StatFn        fn) {
  
  int k = stats.ncol();
  
  // compute jackknife once for all k parameters
  NumericMatrix jack = jackknife_matrix(X, y, k, fn);
  
  NumericMatrix out(k, 3);
  colnames(out) = CharacterVector::create("est", "lci", "uci");
  
  for (int j = 0; j < k; j++) {
    
    NumericVector s  = stats(_, j);
    LogicalVector ok = !is_na(s);
    NumericVector sv = s[ok];
    
    out(j, 0) = est[j];
    
    if ((int)sv.size() == 0) {
      out(j, 1) = NA_REAL;
      out(j, 2) = NA_REAL;
      continue;
    }
    
    std::sort(sv.begin(), sv.end());
    
    double z0      = bca_z0(sv, est[j]);
    double acc     = bca_acceleration(jack(_, j));
    auto   levels  = bca_quantile_levels(z0, acc, alpha);
    
    out(j, 1) = quantile_type7(sv, levels.first);
    out(j, 2) = quantile_type7(sv, levels.second);
  }
  
  return out;
}


// ============================================================
// run_boot: scalar statistic -> named vector (est, lci, uci)
//
// method: "perc" or "bca"
// ============================================================

template <typename StatFn>
NumericVector run_boot(NumericMatrix     X,
                       NumericVector     y,
                       int               R,
                       double            alpha,
                       int               seed,
                       StatFn            fn,
                       std::string       method = "perc") {
  
  unsigned int base_seed = (seed < 0)
  ? std::random_device{}()
    : static_cast<unsigned int>(seed);
  
  NumericVector stats(R);
  
  ScalarBootWorker<StatFn> worker(X, y, stats, base_seed, fn);
  parallelFor(0, R, worker);
  
  arma::mat Xa  = Rcpp::as<arma::mat>(X);
  arma::vec ya  = Rcpp::as<arma::vec>(y);
  double    est = fn.compute(Xa, ya);
  
  if (method == "perc") {
    return boot_percentile_ci(stats, est, alpha);
    
  } else if (method == "bca") {
    return boot_bca_ci(stats, X, y, est, alpha, fn);
    
  } else {
    Rcpp::stop("Unknown bootstrap method '%s'. Use 'perc' or 'bca'.",
               method.c_str());
  }
}


// ============================================================
// run_boot_matrix: vector statistic -> matrix (k rows x 3 cols)
//
// Each row: one parameter, columns: est / lci / uci
// method: "perc" or "bca"
// ============================================================

template <typename StatFn>
NumericMatrix run_boot_matrix(NumericMatrix     X,
                              NumericVector     y,
                              int               R,
                              double            alpha,
                              int               seed,
                              StatFn            fn,
                              std::string       method = "perc") {
  
  unsigned int base_seed = (seed < 0)
  ? std::random_device{}()
    : static_cast<unsigned int>(seed);
  
  arma::mat Xa  = Rcpp::as<arma::mat>(X);
  arma::vec ya  = Rcpp::as<arma::vec>(y);
  arma::vec est = fn.compute(Xa, ya);
  int       k   = est.n_elem;
  
  NumericMatrix stats(R, k);
  
  MatrixBootWorker<StatFn> worker(X, y, stats, base_seed, fn);
  parallelFor(0, R, worker);
  
  if (method == "perc") {
    
    NumericMatrix out(k, 3);
    colnames(out) = CharacterVector::create("est", "lci", "uci");
    
    for (int j = 0; j < k; j++) {
      
      NumericVector s  = stats(_, j);
      LogicalVector ok = !is_na(s);
      NumericVector sv = s[ok];
      
      out(j, 0) = est[j];
      
      if ((int)sv.size() == 0) {
        out(j, 1) = NA_REAL;
        out(j, 2) = NA_REAL;
        continue;
      }
      
      std::sort(sv.begin(), sv.end());
      out(j, 1) = quantile_type7(sv, alpha / 2.0);
      out(j, 2) = quantile_type7(sv, 1.0 - alpha / 2.0);
    }
    
    return out;
    
  } else if (method == "bca") {
    
    return boot_bca_ci_matrix(stats, X, y, est, alpha, fn);
    
  } else {
    Rcpp::stop("Unknown bootstrap method '%s'. Use 'perc' or 'bca'.",
               method.c_str());
  }
}


// ============================================================
// convenience helpers
//
// vec_to_matrix: wrap a numeric vector into a 1-column matrix
//   so scalar-only StatFn structs fit the X/y interface.
//
// dummy_vec: zero vector of length n (placeholder for y when
//   the statistic only depends on X)
//
// Usage:
//   run_boot(vec_to_matrix(x), dummy_vec(x.size()), ...)
//
// ============================================================

inline NumericMatrix vec_to_matrix(NumericVector x) {
  NumericMatrix m(x.size(), 1);
  m(_, 0) = x;
  return m;
}

inline NumericVector dummy_vec(int n) {
  return NumericVector(n, 0.0);
}

