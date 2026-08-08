
#pragma once

// ============================================================
// boot_framework.h
//
// Generic parallel bootstrap framework using RcppParallel.
//
// ACHTUNG: Diese Datei liegt in ZWEI Paketen (lumen und DescToolsX) und
// muss dort BYTEGLEICH sein. Sie war es nicht - lumen hatte einen
// aelteren Stand ohne die Schutzabfragen. Vor jeder Aenderung:
//   tools::md5sum(c("../lumen/src/boot_framework.h",
//                   "../DescToolsX/src/boot_framework.h"))
// Dasselbe gilt fuer bca_helpers.h.
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
// Requirements on StatFn (the workers call it from several threads
// at once, on the SAME object):
//   - compute() must be const and must not mutate any shared state
//   - compute() must NOT touch the R API (no Rcpp::stop, no R::*,
//     no allocation of SEXPs) - only plain C++/Armadillo
//   - compute() may throw; the worker catches everything and records
//     NA_REAL for that replicate
//
// Reproducibility: every replicate is seeded individually with
// base_seed + b, so the result does not depend on the number of
// threads, and raising R keeps the first replicates unchanged.
//
// Supported CI methods: "perc", "bca"
//
// alpha is the TOTAL error probability: the interval runs from the
// alpha/2 to the 1-alpha/2 quantile.
//
// ============================================================

#include <RcppArmadillo.h>
#include <RcppParallel.h>

#include "bca_helpers.h"   // bca_z0, bca_acceleration, bca_quantile_levels

#include <random>
#include <cmath>
#include <vector>
#include <algorithm>
#include <numeric>
#include <utility>

// [[Rcpp::depends(RcppArmadillo, RcppParallel)]]

using namespace Rcpp;
using namespace RcppParallel;
using namespace arma;


// ============================================================
// argument checks, shared by run_boot() and run_boot_matrix()
// ============================================================

inline void check_boot_args(int R, double alpha) {

  if (R < 2)
    Rcpp::stop("'R' must be at least 2.");

  // alpha outside (0, 1) would push the quantile level out of [0, 1]
  // and index the sorted bootstrap distribution out of bounds
  if (!(alpha > 0.0) || !(alpha < 1.0))
    Rcpp::stop("'alpha' must lie in (0, 1).");
}


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
    
    // allocated once per thread chunk instead of once per replicate
    arma::mat Xb(n, p);
    arma::vec yb(n);
    
    for (std::size_t b = begin; b < end; b++) {
      
      // thread-local RNG seeded per iteration
      std::mt19937 rng(base_seed + static_cast<unsigned>(b));

      std::uniform_int_distribution<std::size_t> dist(0, n - 1);
      
      for (std::size_t i = 0; i < n; i++)
        idx[i] = dist(rng);
      
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
    
    arma::mat Xb(n, p);
    arma::vec yb(n);
    
    for (std::size_t b = begin; b < end; b++) {
      
      std::mt19937 rng(base_seed + static_cast<unsigned>(b));
      std::uniform_int_distribution<std::size_t> dist(0, n - 1);
      
      for (std::size_t i = 0; i < n; i++)
        idx[i] = dist(rng);
      
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
// drop NA/NaN replicates and sort
//
// std::sort() requires a strict weak ordering; a NaN among the
// bootstrap replicates would make the comparisons inconsistent, which
// is undefined behaviour (not merely a NaN in the result). ISNAN()
// covers NA_real_ as well as NaN, whatever a failing StatFn returns.
// ============================================================

inline NumericVector boot_valid_sorted(const NumericVector& stats) {
  
  std::vector<double> v;
  v.reserve(stats.size());
  
  for (R_xlen_t i = 0; i < stats.size(); i++)
    if (!ISNAN(stats[i]))
      v.push_back(stats[i]);
  
  std::sort(v.begin(), v.end());
  
  return NumericVector(v.begin(), v.end());
}


// ============================================================
// quantile type 7 (same as R default)
//
// x must be sorted and non-empty, p must lie in [0, 1]
// ============================================================

inline double quantile_type7(const NumericVector& x, double p) {
  
  int n = x.size();
  
  if (n == 0)
    return NA_REAL;
  
  if (n == 1)
    return x[0];
  
  if (ISNAN(p))
    return NA_REAL;
  
  // (int)std::floor() of a negative or NaN argument would index out of
  // bounds; clamp instead of trusting the caller
  if (p <= 0.0) return x[0];
  if (p >= 1.0) return x[n - 1];
  
  double h    = (n - 1) * p;
  int    i    = (int)std::floor(h);
  double frac = h - i;
  
  if (i >= n - 1)
    return x[n - 1];
  
  return x[i] + frac * (x[i + 1] - x[i]);
}


// ============================================================
// percentile CI from bootstrap distribution
// ============================================================

inline NumericVector boot_percentile_ci(NumericVector stats,
                                        double        est,
                                        double        alpha) {
  
  NumericVector s = boot_valid_sorted(stats);
  
  if (s.size() == 0)
    Rcpp::stop("All bootstrap samples failed.");
  
  NumericVector out(3);
  out[0] = est;
  out[1] = quantile_type7(s, alpha / 2.0);
  out[2] = quantile_type7(s, 1.0 - alpha / 2.0);
  out.attr("names") = CharacterVector::create("est", "lci", "uci");
  
  return out;
}


// jackknife leave-one-out estimates for scalar StatFn
template <typename StatFn>
inline NumericVector jackknife_scalar(NumericMatrix X,
                                      NumericVector y,
                                      StatFn        fn) {
  
  int n = X.nrow();
  int p = X.ncol();
  
  NumericVector jack(n);
  
  arma::mat Xj(n - 1, p);
  arma::vec yj(n - 1);
  
  for (int i = 0; i < n; i++) {
    
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
  
  arma::mat Xj(n - 1, p);
  arma::vec yj(n - 1);
  
  for (int i = 0; i < n; i++) {
    
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
  
  NumericVector s = boot_valid_sorted(stats);
  
  if (s.size() == 0)
    Rcpp::stop("All bootstrap samples failed.");
  
  if (X.nrow() < 2)
    Rcpp::stop("BCa needs at least 2 observations; use method = \"perc\".");
  
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
  
  if (X.nrow() < 2)
    Rcpp::stop("BCa needs at least 2 observations; use method = \"perc\".");
  
  // compute jackknife once for all k parameters
  NumericMatrix jack = jackknife_matrix(X, y, k, fn);
  
  NumericMatrix out(k, 3);
  colnames(out) = CharacterVector::create("est", "lci", "uci");
  
  for (int j = 0; j < k; j++) {
    
    NumericVector sv = boot_valid_sorted(stats(_, j));
    
    out(j, 0) = est[j];
    
    if ((int)sv.size() == 0) {
      out(j, 1) = NA_REAL;
      out(j, 2) = NA_REAL;
      continue;
    }
    
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
// est is the statistic of the ORIGINAL sample, not the bootstrap mean.
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
  
  check_boot_args(R, alpha);
  
  if (method != "perc" && method != "bca")
    Rcpp::stop("Unknown bootstrap method '%s'. Use 'perc' or 'bca'.",
               method.c_str());
  
  // seed < 0 zieht aus R's RNG, nicht aus std::random_device: sonst
  // waere dieser Pfad die einzige Stelle, an der set.seed() nachweislich
  // wirkungslos bleibt. Laeuft auf dem Hauptthread, Rcpp haelt den
  // RNG-Zustand.
  unsigned int base_seed =
    (seed < 0) ? static_cast<unsigned int>(R::unif_rand() * 4294967295.0)
               : static_cast<unsigned int>(seed);
  
  NumericVector stats(R);
  
  ScalarBootWorker<StatFn> worker(X, y, stats, base_seed, fn);
  parallelFor(0, R, worker);
  
  arma::mat Xa  = Rcpp::as<arma::mat>(X);
  arma::vec ya  = Rcpp::as<arma::vec>(y);
  double    est = fn.compute(Xa, ya);
  
  if (method == "perc")
    return boot_percentile_ci(stats, est, alpha);
  
  return boot_bca_ci(stats, X, y, est, alpha, fn);
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
  
  check_boot_args(R, alpha);
  
  if (method != "perc" && method != "bca")
    Rcpp::stop("Unknown bootstrap method '%s'. Use 'perc' or 'bca'.",
               method.c_str());
  
  // seed < 0 zieht aus R's RNG, nicht aus std::random_device: sonst
  // waere dieser Pfad die einzige Stelle, an der set.seed() nachweislich
  // wirkungslos bleibt. Laeuft auf dem Hauptthread, Rcpp haelt den
  // RNG-Zustand.
  unsigned int base_seed =
    (seed < 0) ? static_cast<unsigned int>(R::unif_rand() * 4294967295.0)
               : static_cast<unsigned int>(seed);
  
  arma::mat Xa  = Rcpp::as<arma::mat>(X);
  arma::vec ya  = Rcpp::as<arma::vec>(y);
  arma::vec est = fn.compute(Xa, ya);
  int       k   = est.n_elem;
  
  NumericMatrix stats(R, k);
  
  MatrixBootWorker<StatFn> worker(X, y, stats, base_seed, fn);
  parallelFor(0, R, worker);
  
  if (method == "bca")
    return boot_bca_ci_matrix(stats, X, y, est, alpha, fn);
  
  NumericMatrix out(k, 3);
  colnames(out) = CharacterVector::create("est", "lci", "uci");
  
  for (int j = 0; j < k; j++) {
    
    NumericVector sv = boot_valid_sorted(stats(_, j));
    
    out(j, 0) = est[j];
    
    if ((int)sv.size() == 0) {
      out(j, 1) = NA_REAL;
      out(j, 2) = NA_REAL;
      continue;
    }
    
    out(j, 1) = quantile_type7(sv, alpha / 2.0);
    out(j, 2) = quantile_type7(sv, 1.0 - alpha / 2.0);
  }
  
  return out;
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

inline NumericVector dummy_vec(R_xlen_t n) {
  return NumericVector(n, 0.0);
}
