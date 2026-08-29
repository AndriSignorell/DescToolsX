
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

  explicit BIT1D(int n_ = 0) : n(n_), tree(n_ + 1, 0) {}

  // Reused across bootstrap replicates: clearing costs O(n), which is
  // dominated by the O(n log n) sweep that follows.
  void reset(int n_) {
    n = n_;
    tree.assign(n_ + 1, 0);
  }

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
// Compression, sweep
// =============================================================

// Maps a vector of doubles to 0-based ranks, NaN to -1. Returns the number
// of distinct non-missing values, which is the size the Fenwick tree needs.
//
// Contiguous vectors rather than set<double> plus unordered_map<double,int>:
// no node allocation, no hashing of doubles. Sorting treats -0.0 and 0.0 as
// equivalent and unique() collapses them, which is what the pair counts mean
// by a tie.
static int compressRanks(const vector<double>& v, vector<int>& rank_out) {

  const size_t n = v.size();
  rank_out.assign(n, -1);

  vector<double> vals;
  vals.reserve(n);

  for (size_t i = 0; i < n; ++i)
    if (!std::isnan(v[i])) vals.push_back(v[i]);

  sort(vals.begin(), vals.end());
  vals.erase(unique(vals.begin(), vals.end()), vals.end());

  for (size_t i = 0; i < n; ++i)
    if (!std::isnan(v[i]))
      rank_out[i] = (int)(lower_bound(vals.begin(), vals.end(), v[i]) -
                          vals.begin());

  return (int)vals.size();
}


// The whole count, given the points as (x key, y rank).
//
// Only the y coordinate has to be a rank, because it indexes the tree. The x
// coordinate is merely sorted and compared for equality, so it can stay a raw
// double for a direct call and be a precomputed rank in the bootstrap - hence
// the template. Compressing x as well would cost one extra sort and one
// binary search per observation for nothing.
//
// 'points' is sorted in place. 'bit' and 'count_y' are buffers the caller
// owns, so a bootstrap allocates them once per thread instead of once per
// replicate.
template <class XKey>
static ConcordanceResult sweepPairs(vector<pair<XKey,int> >& points,
                                    int y_size,
                                    BIT1D& bit,
                                    vector<long long>& count_y) {

  bit.reset(y_size);
  count_y.assign(y_size, 0);

  for (size_t k = 0; k < points.size(); ++k)
    ++count_y[points[k].second];

  sort(points.begin(), points.end());

  // Sweep in x order. Equal x values are queried as one block and inserted
  // only afterwards, so every point already in the tree has strictly smaller
  // x. A smaller y is concordant and a larger y is discordant.
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

  for (size_t k = 0; k < count_y.size(); ++k) {
    const long long ny = count_y[k];
    ties_y_inclusive += ny * (ny - 1) / 2;
  }

  ConcordanceResult res;
  res.C = (double)C;
  res.D = (double)D;
  res.Ties_X = (double)(ties_x_inclusive - ties_xy);
  res.Ties_Y = (double)(ties_y_inclusive - ties_xy);
  res.Ties_XY = (double)ties_xy;

  return res;
}



// =============================================================
// Pure indexed concordance.
// This version also accepts repeated indices from parallel bootstraps.
// =============================================================

ConcordanceResult conDisPairsXY_indexed(const vector<double>& x,
                                        const vector<double>& y,
                                        const vector<int>& idx) {

  vector<int> y_rank;
  const int y_size = compressRanks(y, y_rank);

  vector<pair<double,int> > points;
  points.reserve(idx.size());

  for (size_t i = 0; i < idx.size(); ++i) {
    const int k = idx[i];
    if (!std::isnan(x[k]) && y_rank[k] >= 0)
      points.push_back(make_pair(x[k], y_rank[k]));
  }

  BIT1D bit;
  vector<long long> count_y;

  return sweepPairs(points, y_size, bit, count_y);
}



// [[Rcpp::export]]
NumericVector condis_pairs_xy_cpp(NumericVector xR,
                                     NumericVector yR) {

  std::vector<double> x = Rcpp::as<std::vector<double> >(xR);
  std::vector<double> y = Rcpp::as<std::vector<double> >(yR);

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

// The resampled values never change over the B replicates, only which of
// them are drawn - so both variables are compressed once, before the loop,
// and a replicate sorts integers instead of redoing the compression. The
// Fenwick tree and the count buffer are allocated once per thread and
// cleared per replicate.
//
// Ranks are taken over the full data, so a replicate may leave gaps in them.
// That is harmless: the counts depend on the order and the equalities of the
// values drawn, and both survive. Only the tree is then wider than strictly
// needed.
struct CStatWorker : public Worker {

  const std::vector<double>& y;      // outcome, for the degeneracy check
  const std::vector<int>& out_rank;  // ranks of the outcome
  const std::vector<int>& pred_rank; // ranks of the predictor
  int n;
  int pred_size;
  unsigned int base_seed;

  RVector<double> stats;

  CStatWorker(const std::vector<double>& y,
              const std::vector<int>& out_rank,
              const std::vector<int>& pred_rank,
              int pred_size,
              NumericVector stats,
              unsigned int base_seed)
    : y(y), out_rank(out_rank), pred_rank(pred_rank),
      n((int)out_rank.size()), pred_size(pred_size),
      base_seed(base_seed), stats(stats) {}


  void operator()(std::size_t begin, std::size_t end) {

    std::vector<int> idx(n);

    // buffers, reused across the replicates of this thread
    BIT1D bit;
    std::vector<long long> count_y;
    std::vector<std::pair<int,int> > points;
    points.reserve(n);

    for (size_t b = begin; b < end; b++) {

      // own RNG per bootstrap
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

      points.clear();

      for (int i = 0; i < n; i++) {
        const int k = idx[i];
        if (out_rank[k] >= 0 && pred_rank[k] >= 0)
          points.push_back(std::make_pair(out_rank[k], pred_rank[k]));
      }

      // outcome in x position, predictor in y position - Ties_Y are
      // therefore the ties of the predictor, as the c statistic needs
      auto z = sweepPairs(points, pred_size, bit, count_y);

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

  std::vector<double> x = Rcpp::as<std::vector<double> >(xR);
  std::vector<double> y = Rcpp::as<std::vector<double> >(yR);

  NumericVector stats(B);

  // --- compress once, not once per replicate ---
  std::vector<int> out_rank, pred_rank;
  compressRanks(y, out_rank);
  const int pred_size = compressRanks(x, pred_rank);

  // --- parallel ---
  CStatWorker worker(y, out_rank, pred_rank, pred_size, stats, base_seed);
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

