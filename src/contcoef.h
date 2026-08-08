#ifndef DESCTOOLSX_CONTCOEF_H
#define DESCTOOLSX_CONTCOEF_H

// One implementation of Pearson's contingency coefficient, used by the
// point estimate, by every bootstrap replicate and by the jackknife.
// Before, the same formula existed three times (point estimator, worker,
// compute_cc) - and the three had already started to drift apart in their
// guards.
//
// LAYOUT: all buffers here are ROW-major (cell (i,j) at i*c + j), which is
// NOT R's layout. Entry points copy the R matrix once into a row-major
// buffer. That copy is deliberate: the bootstrap draws a cell index from a
// discrete_distribution over this buffer, so changing the ordering would
// change every interval the suite has ever reported for a given seed.

#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>
#include <cstddef>

namespace desctoolsx {

// Copy an R matrix (column-major, integer or double) into the row-major
// buffer everything below works on, and total it.
//
// NumericMatrix, not IntegerMatrix: an IntegerMatrix parameter silently
// coerces a double matrix by TRUNCATING towards zero, so a weighted table
// with 2.7 in a cell used to be counted as 2 without a word.
inline std::vector<double> tableToRowMajor(const Rcpp::NumericMatrix& tab,
                                           int& r, int& c, double& n)
{
  r = tab.nrow();
  c = tab.ncol();

  if (r < 1 || c < 1)
    Rcpp::stop("'tab' must have at least one row and one column");

  std::vector<double> out(static_cast<std::size_t>(r) * c);
  n = 0.0;

  for (int i = 0; i < r; ++i)
    for (int j = 0; j < c; ++j) {
      const double v = tab(i, j);
      if (!R_finite(v) || v < 0.0)
        Rcpp::stop("'tab' must contain finite, non-negative counts");
      out[static_cast<std::size_t>(i) * c + j] = v;
      n += v;
    }

  return out;
}

// Sakoda's scale factor sqrt((k-1)/k), k = min(r, c); 0 for a degenerate
// table, where the corrected coefficient is not defined
inline double contcoefMax(int r, int c) {
  const int k = std::min(r, c);
  return (k > 1) ? std::sqrt((k - 1.0) / k) : 0.0;
}

// tab: row-major, length r*c. n: table total (the jackknife passes n-1).
// rs/cs: scratch of length r and c, supplied by the caller so that the
// bootstrap loop allocates nothing.
inline double contcoefImpl(const double* tab, int r, int c, double n,
                           bool correct,
                           std::vector<double>& rs,
                           std::vector<double>& cs)
{
  if (!(n > 0.0))
    return NA_REAL;

  std::fill(rs.begin(), rs.end(), 0.0);
  std::fill(cs.begin(), cs.end(), 0.0);

  for (int i = 0; i < r; ++i) {
    const double* row = tab + static_cast<std::size_t>(i) * c;
    for (int j = 0; j < c; ++j) {
      const double v = row[j];
      rs[i] += v;
      cs[j] += v;
    }
  }

  // empty rows and columns contribute nothing and would divide by zero;
  // skipping them by margin is the same cell set as the old test
  // expected > 0, one comparison per cell cheaper
  double chisq = 0.0;

  for (int i = 0; i < r; ++i) {
    if (rs[i] <= 0.0) continue;
    const double* row = tab + static_cast<std::size_t>(i) * c;
    for (int j = 0; j < c; ++j) {
      if (cs[j] <= 0.0) continue;
      const double e = rs[i] * cs[j] / n;
      const double d = row[j] - e;
      chisq += d * d / e;
    }
  }

  double cc = std::sqrt(chisq / (chisq + n));

  // k is taken from the ORIGINAL dimensions, also for a resampled table
  // whose margins happen to be empty - otherwise the replicates would be
  // measured on a different scale than the estimate they surround
  if (correct) {
    const double cmax = contcoefMax(r, c);
    if (cmax > 0.0) cc /= cmax;
  }

  return cc;
}

// convenience overload for the callers that are not in a hot loop
inline double contcoefImpl(const std::vector<double>& tab, int r, int c,
                           double n, bool correct)
{
  std::vector<double> rs(r), cs(c);
  return contcoefImpl(tab.data(), r, c, n, correct, rs, cs);
}

}  // namespace desctoolsx

#endif  // DESCTOOLSX_CONTCOEF_H
