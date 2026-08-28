
//   Rewritten by Andri Signorell in C++

#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>
#include <random>

using namespace Rcpp;
using namespace std;



// 1D Binary Indexed Tree (Fenwick tree) over compressed y ranks.
struct BIT1D {
  int n;
  vector<int> tree;

  explicit BIT1D(int n_) : n(n_), tree(n_ + 1, 0) {}

  void add(int i) {
    for (++i; i <= n; i += i & -i)
      ++tree[i];
  }

  // Number of inserted points with rank <= i. For i = -1 this is zero.
  int sum(int i) const {
    int res = 0;
    for (++i; i > 0; i -= i & -i)
      res += tree[i];
    return res;
  }
};



struct ConcordanceResult {
  double C;
  double D;
  double Ties_X;   // tied in x only
  double Ties_Y;   // tied in y only
  double Ties_XY;  // tied in both
};



// =============================================================
// Pure indexed concordance.
// This version also accepts repeated indices from parallel bootstraps.
// =============================================================

ConcordanceResult conDisPairsXY_indexed(const vector<double>& x,
                                        const vector<double>& y,
                                        const vector<int>& idx) {

  const int n = (int)idx.size();

  vector<pair<double,double>> valid_pairs;
  valid_pairs.reserve(n);

  for (int i = 0; i < n; ++i) {
    const double xi = x[idx[i]];
    const double yi = y[idx[i]];

    if (!std::isnan(xi) && !std::isnan(yi))
      valid_pairs.emplace_back(xi, yi);
  }

  // Coordinate compression using contiguous vectors instead of trees and
  // hash maps. Only the ranks are needed by the sweep below.
  vector<double> xs, ys;
  xs.reserve(valid_pairs.size());
  ys.reserve(valid_pairs.size());

  for (auto& p : valid_pairs) {
    xs.push_back(p.first);
    ys.push_back(p.second);
  }

  sort(xs.begin(), xs.end());
  xs.erase(unique(xs.begin(), xs.end()), xs.end());

  sort(ys.begin(), ys.end());
  ys.erase(unique(ys.begin(), ys.end()), ys.end());

  const int y_size = (int)ys.size();

  vector<pair<int,int>> points;
  points.reserve(valid_pairs.size());

  vector<long long> count_y(y_size, 0);

  for (auto& p : valid_pairs) {
    const int xi = (int)(lower_bound(xs.begin(), xs.end(), p.first) -
                         xs.begin());
    const int yi = (int)(lower_bound(ys.begin(), ys.end(), p.second) -
                         ys.begin());

    points.emplace_back(xi, yi);
    ++count_y[yi];
  }

  sort(points.begin(), points.end());

  // Sweep in x order. Equal x values are queried as one block and inserted
  // only afterwards, so every point already in the tree has strictly smaller
  // x. A smaller y is concordant and a larger y is discordant.
  BIT1D bit(y_size);

  long long C = 0, D = 0;
  size_t i = 0;

  while (i < points.size()) {
    size_t j = i;
    while (j < points.size() && points[j].first == points[i].first)
      ++j;

    const long long inserted = (long long)bit.sum(y_size - 1);

    for (size_t k = i; k < j; ++k) {
      const int yi = points[k].second;
      C += (long long)bit.sum(yi - 1);
      D += inserted - (long long)bit.sum(yi);
    }

    for (size_t k = i; k < j; ++k)
      bit.add(points[k].second);

    i = j;
  }

  // Marginal tie counts are inclusive. Subtracting the joint ties makes
  // Ties_X and Ties_Y exclusive, as required by the five-part partition:
  //
  //   C + D + Ties_X + Ties_Y + Ties_XY = n(n-1)/2

  long long ties_x_inclusive = 0;
  long long ties_y_inclusive = 0;
  long long ties_xy = 0;

  i = 0;
  while (i < points.size()) {
    size_t j = i;
    while (j < points.size() && points[j].first == points[i].first)
      ++j;

    const long long nx = (long long)(j - i);
    ties_x_inclusive += nx * (nx - 1) / 2;

    size_t k = i;
    while (k < j) {
      size_t l = k;
      while (l < j && points[l].second == points[k].second)
        ++l;

      const long long nxy = (long long)(l - k);
      ties_xy += nxy * (nxy - 1) / 2;
      k = l;
    }

    i = j;
  }

  for (long long ny : count_y)
    ties_y_inclusive += ny * (ny - 1) / 2;

  ConcordanceResult res;
  res.C = (double)C;
  res.D = (double)D;
  res.Ties_X = (double)(ties_x_inclusive - ties_xy);
  res.Ties_Y = (double)(ties_y_inclusive - ties_xy);
  res.Ties_XY = (double)ties_xy;

  return res;
}



// [[Rcpp::export]]
NumericVector condis_pairs_xy_cpp(NumericVector xR,
                                     NumericVector yR) {
  
  std::vector<double> x = Rcpp::as<std::vector<double>>(xR);
  std::vector<double> y = Rcpp::as<std::vector<double>>(yR);
  
  int n = x.size();
  
  std::vector<int> idx(n);
  for (int i = 0; i < n; i++) idx[i] = i;
  
  auto z = conDisPairsXY_indexed(x, y, idx);
  
  NumericVector out(5);
  out[0] = z.C;
  out[1] = z.D;
  out[2] = z.Ties_X;
  out[3] = z.Ties_Y;
  out[4] = z.Ties_XY;
  
  out.attr("names") =
    CharacterVector::create("C","D","Ties_X","Ties_Y","Ties_XY");
  
  return out;
}




// ============================================================
//   parallel processing for speed up bootstraps CIs
// ============================================================


#include <RcppParallel.h>
using namespace RcppParallel;

struct CStatWorker : public Worker {
  
  const std::vector<double>& x;
  const std::vector<double>& y;
  int n;
  int B;
  unsigned int base_seed;  
  
  RVector<double> stats;
  
  CStatWorker(const std::vector<double>& x,
              const std::vector<double>& y,
              NumericVector stats,
              unsigned int base_seed)  
    : x(x), y(y), n(x.size()), base_seed(base_seed), stats(stats) {}
  
  
  void operator()(std::size_t begin, std::size_t end) {
    
    std::vector<int> idx(n);
    
    for (size_t b = begin; b < end; b++) {
      
      // own RNG per bootstrap
      // std::mt19937 rng(123 + b);
      std::mt19937 rng(base_seed + b);
      std::uniform_int_distribution<int> dist(0, n - 1);
      
      bool has0 = false, has1 = false;
      
      for (int i = 0; i < n; i++) {
        idx[i] = dist(rng);
        
        if (y[idx[i]] == 0) has0 = true;
        if (y[idx[i]] == 1) has1 = true;
      }
      
      if (!(has0 && has1)) {
        stats[b] = NA_REAL;
        continue;
      }
      
      auto z = conDisPairsXY_indexed(y, x, idx);
      
      double denom = z.C + z.D + z.Ties_Y;
      
      if (denom == 0.0) {
        stats[b] = NA_REAL;
      } else {
        stats[b] = (z.C + 0.5 * z.Ties_Y) / denom;
      }
    }
  }
  
};


// [[Rcpp::export]]
NumericVector cstat_boot_cpp(NumericVector yR,
                                           NumericVector xR,
                                           int B = 1000,
                                           double alpha = 0.05,
                                           int seed = -1) {  // -1 = random
                                             
  unsigned int base_seed = (seed < 0) 
    ? std::random_device{}() 
      : (unsigned int)seed;
  
  std::vector<double> x = Rcpp::as<std::vector<double>>(xR);
  std::vector<double> y = Rcpp::as<std::vector<double>>(yR);
  
  NumericVector stats(B);
  
  // --- parallel ---
  CStatWorker worker(x, y, stats, base_seed);
  parallelFor(0, B, worker);
  
  // --- omit NAs ---
  LogicalVector ok = !is_na(stats);
  NumericVector s = stats[ok];
  
  if (s.size() == 0) {
    stop("All bootstrap samples were degenerate.");
  }
  
  std::sort(s.begin(), s.end());
  
  int n2 = s.size();
  
  int lo = (int)(alpha / 2.0 * n2);
  int hi = (int)((1 - alpha / 2.0) * n2);
  
  lo = std::max(0, std::min(lo, n2 - 1));
  hi = std::max(0, std::min(hi, n2 - 1));
  
  // --- original ---
  std::vector<int> idx_full(x.size());
  for (int i = 0; i < (int)x.size(); i++) idx_full[i] = i;
  
  auto z0 = conDisPairsXY_indexed(y, x, idx_full);
  
  double est = (z0.C + 0.5 * z0.Ties_Y) /
    (z0.C + z0.D + z0.Ties_Y);
  
  NumericVector out(3);
  out[0] = est;
  out[1] = s[lo];
  out[2] = s[hi];
  
  out.attr("names") = CharacterVector::create("est","lci","uci");
  
  return out;
}


// 
// // =========================================================
// //   base R reproduced version - stable and reference
// //   !!! do not delete - keep for comparison
// // =========================================================
// 
// ConcordanceResult conDisPairsXY_cpp(const vector<double>& x,
//                                     const vector<double>& y) {
//   
//   int n = x.size();
//   
//   vector<pair<double, double>> valid_pairs;
//   valid_pairs.reserve(n);
//   
//   for (int i = 0; i < n; ++i) {
//     if (!std::isnan(x[i]) && !std::isnan(y[i])) {
//       valid_pairs.emplace_back(x[i], y[i]);
//     }
//   }
//   
//   // coordinate compression
//   set<double> x_set, y_set;
//   for (auto& p : valid_pairs) {
//     x_set.insert(p.first);
//     y_set.insert(p.second);
//   }
//   
//   unordered_map<double, int> x_map, y_map;
//   int idx = 0;
//   for (double v : x_set) x_map[v] = idx++;
//   int x_size = idx;
//   
//   idx = 0;
//   for (double v : y_set) y_map[v] = idx++;
//   int y_size = idx;
//   
//   vector<pair<int, int>> points;
//   points.reserve(valid_pairs.size());
//   
//   unordered_map<int, int> count_x;
//   unordered_map<int, int> count_y;
//   unordered_map<long long, int> count_xy;
//   
//   for (auto& p : valid_pairs) {
//     int xi = x_map[p.first];
//     int yi = y_map[p.second];
//     points.emplace_back(xi, yi);
//     
//     count_x[xi]++;
//     count_y[yi]++;
//     long long key = ((long long)xi << 32) | yi;
//     count_xy[key]++;
//   }
//   
//   sort(points.begin(), points.end());
//   
//   BIT2D bit(x_size, y_size);
//   
//   long long C = 0, D = 0;
//   
//   for (size_t i = 0; i < points.size(); ++i) {
//     int xi = points[i].first;
//     int yi = points[i].second;
//     
//     int concordant =
//       bit.query_range(0, 0, xi - 1, yi - 1) +
//       bit.query_range(xi + 1, yi + 1, x_size - 1, y_size - 1);
//     
//     int discordant =
//       bit.query_range(0, yi + 1, xi - 1, y_size - 1) +
//       bit.query_range(xi + 1, 0, x_size - 1, yi - 1);
//     
//     C += concordant;
//     D += discordant;
//     
//     bit.update(xi, yi, 1);
//   }
//   
//   long long Ties_X = 0, Ties_Y = 0;
//   
//   for (auto& kv : count_x) {
//     long long g = kv.second;
//     Ties_X += g * (g - 1) / 2;
//   }
//   
//   for (auto& kv : count_y) {
//     long long g = kv.second;
//     Ties_Y += g * (g - 1) / 2;
//   }
//   
//   for (auto& kv : count_xy) {
//     long long g = kv.second;
//     long long corr = g * (g - 1) / 2;
//     Ties_X -= corr;
//     Ties_Y -= corr;
//   }
//   
//   ConcordanceResult res;
//   res.C = C;
//   res.D = D;
//   res.Ties_X = Ties_X;
//   res.Ties_Y = Ties_Y;
//   
//   return res;
// }
// 
// 
// // [[Rcpp::export]]
// Rcpp::List conDisPairsXY(NumericVector x, NumericVector y) {
//   
//   std::vector<double> xv = Rcpp::as<std::vector<double>>(x);
//   std::vector<double> yv = Rcpp::as<std::vector<double>>(y);
//   
//   auto res = conDisPairsXY_cpp(xv, yv);
//   
//   return Rcpp::List::create(
//     Rcpp::Named("C") = res.C,
//     Rcpp::Named("D") = res.D,
//     Rcpp::Named("Ties_X") = res.Ties_X,
//     Rcpp::Named("Ties_Y") = res.Ties_Y
//   );
//   
// }
// 
// 
// 
// // working safe version
// // 
// // // [[Rcpp::export]]
// // NumericVector cstat_bootstrap_safe_cpp(NumericVector x,
// //                                        NumericVector y,
// //                                        int B = 1000,
// //                                        double alpha = 0.05) {
// //   
// //   int n = x.size();
// //   NumericVector stats(B);
// //   
// //   // --- bootstrap ---
// //   for (int b = 0; b < B; b++) {
// //     
// //     IntegerVector idx = Rcpp::sample(n, n, true);
// //     
// //     NumericVector xb(n), yb(n);
// //     
// //     for (int i = 0; i < n; i++) {
// //       xb[i] = x[idx[i] - 1];
// //       yb[i] = y[idx[i] - 1];
// //     }
// //     
// //     // --- check: beide Klassen vorhanden ---
// //     bool has0 = false, has1 = false;
// //     for (int i = 0; i < n; i++) {
// //       if (yb[i] == 0) has0 = true;
// //       if (yb[i] == 1) has1 = true;
// //     }
// //     
// //     if (!(has0 && has1)) {
// //       stats[b] = NA_REAL;
// //       continue;
// //     }
// //     
// //     List z = conDisPairsXY(xb, yb);
// //     
// //     double C = z["C"];
// //     double D = z["D"];
// //     double T = z["Ties_Y"];
// //     
// //     double denom = C + D + T;
// //     
// //     if (denom == 0.0) {
// //       stats[b] = NA_REAL;
// //     } else {
// //       stats[b] = (C + 0.5 * T) / denom;
// //     }
// //   }
// //   
// //   // --- NA entfernen ---
// //   LogicalVector ok = !is_na(stats);
// //   NumericVector s = stats[ok];
// //   
// //   if (s.size() == 0) {
// //     stop("All bootstrap samples were degenerate.");
// //   }
// //   
// //   std::sort(s.begin(), s.end());
// //   
// //   int n2 = s.size();
// //   
// //   int lo = (int)(alpha / 2.0 * n2);
// //   int hi = (int)((1 - alpha / 2.0) * n2);
// //   
// //   lo = std::max(0, std::min(lo, n2 - 1));
// //   hi = std::max(0, std::min(hi, n2 - 1));
// //   
// //   
// //   // --- original estimate ---
// //   List z0 = conDisPairsXY(x, y);
// //   
// //   double C0 = z0["C"];
// //   double D0 = z0["D"];
// //   double T0 = z0["Ties_Y"];
// //   
// //   double est = (C0 + 0.5 * T0) / (C0 + D0 + T0);
// //   
// //   // --- Rückgabe ---
// //   NumericVector out(3);
// //   out[0] = est;
// //   out[1] = s[lo];
// //   out[2] = s[hi];
// //   
// //   out.attr("names") = CharacterVector::create("est", "lwr", "upr");
// //   
// //   return out;
// // }
// 
// 
// // 
// // // ===============================
// // // FAST BOOTSTRAP -- not parallel - not faster than pure R
// // // ===============================
// // 
// // // [[Rcpp::export]]
// // NumericVector cstat_bootstrap_fast_cpp(NumericVector xR,
// //                                        NumericVector yR,
// //                                        int B = 1000,
// //                                        double alpha = 0.05) {
// //   
// //   vector<double> x = as<vector<double>>(xR);
// //   vector<double> y = as<vector<double>>(yR);
// //   
// //   int n = x.size();
// //   NumericVector stats(B);
// //   
// //   std::mt19937 rng(123);
// //   std::uniform_int_distribution<int> dist(0, n - 1);
// //   
// //   vector<int> idx(n);
// //   
// //   for (int b = 0; b < B; b++) {
// //     
// //     bool has0 = false, has1 = false;
// //     
// //     for (int i = 0; i < n; i++) {
// //       idx[i] = dist(rng);
// //       
// //       if (y[idx[i]] == 0) has0 = true;
// //       if (y[idx[i]] == 1) has1 = true;
// //     }
// //     
// //     if (!(has0 && has1)) {
// //       stats[b] = NA_REAL;
// //       continue;
// //     }
// //     
// //     auto z = conDisPairsXY_indexed(x, y, idx);
// //     
// //     double denom = z.C + z.D + z.Ties_Y;
// //     
// //     if (denom == 0.0) {
// //       stats[b] = NA_REAL;
// //     } else {
// //       stats[b] = (z.C + 0.5 * z.Ties_Y) / denom;
// //     }
// //   }
// //   
// //   // remove NA
// //   LogicalVector ok = !is_na(stats);
// //   NumericVector s = stats[ok];
// //   
// //   if (s.size() == 0) {
// //     stop("All bootstrap samples were degenerate.");
// //   }
// //   
// //   std::sort(s.begin(), s.end());
// //   
// //   int n2 = s.size();
// //   
// //   int lo = (int)(alpha / 2.0 * n2);
// //   int hi = (int)((1 - alpha / 2.0) * n2);
// //   
// //   lo = std::max(0, std::min(lo, n2 - 1));
// //   hi = std::max(0, std::min(hi, n2 - 1));
// //   
// //   // original estimate
// //   vector<int> idx_full(n);
// //   for (int i = 0; i < n; i++) idx_full[i] = i;
// //   
// //   auto z0 = conDisPairsXY_indexed(x, y, idx_full);
// //   
// //   double est = (z0.C + 0.5 * z0.Ties_Y) /
// //     (z0.C + z0.D + z0.Ties_Y);
// //   
// //   NumericVector out(3);
// //   out[0] = est;
// //   out[1] = s[lo];
// //   out[2] = s[hi];
// //   
// //   out.attr("names") = CharacterVector::create("est", "lwr", "upr");
// //   
// //   return out;
// // }
// // 
// // 
