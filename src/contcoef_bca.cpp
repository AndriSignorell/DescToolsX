#include <Rcpp.h>
#include <vector>
#include <cmath>

#include "contcoef.h"

using namespace Rcpp;

// ---------------------------------------------------------------------
// Jackknife acceleration for the BCa interval
// ---------------------------------------------------------------------
// What used to live here besides this: a second bootstrap loop (identical
// to the one in contcoef_table.cpp), a third copy of the statistic, and
// the quantile extraction. The interval is now assembled in R from the
// replicates the other file returns, so this is the only piece that has to
// be compiled.
//
// The deleted quantile code read boot[floor(adj*R)] without clamping the
// index, which reached past the end of the vector whenever the upper
// adjusted probability saturated at 1, and produced an arbitrary index
// whenever the acceleration came out NaN.

// [[Rcpp::export]]
double contcoef_jackknife_a_cpp(NumericMatrix tab, bool correct = false)
{
  int r, c;
  double n;
  std::vector<double> t = desctoolsx::tableToRowMajor(tab, r, c, n);

  // with one observation the leave-one-out table is empty
  if (!(n > 1.0))
    return NA_REAL;

  const std::size_t K = static_cast<std::size_t>(r) * c;

  std::vector<double> rs(r), cs(c);

  // Observations inside one cell are exchangeable, so all tab[k] of them
  // give the SAME leave-one-out value. The old loop recomputed it once per
  // observation - O(n * r * c) work and an n-long vector, which for a
  // table of a million observations was 8 MB and a minute of nothing.
  // Computing it once per cell and weighting by the count is exact, not an
  // approximation.
  std::vector<double> jack(K, 0.0);

  double origin = NA_REAL;
  bool haveOrigin = false;

  for (std::size_t k = 0; k < K; ++k) {
    if (t[k] < 1.0) continue;

    t[k] -= 1.0;
    jack[k] = desctoolsx::contcoefImpl(t.data(), r, c, n - 1.0,
                                       correct, rs, cs);
    t[k] += 1.0;

    if (!haveOrigin) { origin = jack[k]; haveOrigin = true; }
  }

  if (!haveOrigin || !R_finite(origin))
    return NA_REAL;

  // The deviations are of order 1/n while the values themselves are of
  // order 0.1 to 1, so mean and value cancel to almost nothing. Summing
  // them RELATIVE to one of the values keeps that cancellation exact:
  // with all jackknife values equal - a symmetric table, say - every
  // deviation is then a hard zero.
  //
  // Without the shift a single ulp of difference between the mean and the
  // (identical) values survives: on matrix(c(5,5,5,5), 2) it produced
  // den = 1e-33 and an acceleration of -0.037, which is not small, not
  // zero, and pure rounding noise.
  double meanU = 0.0;
  for (std::size_t k = 0; k < K; ++k)
    if (t[k] >= 1.0) meanU += t[k] * (jack[k] - origin);
  meanU /= n;

  double num = 0.0, den = 0.0;

  for (std::size_t k = 0; k < K; ++k) {
    if (t[k] < 1.0) continue;
    const double d = meanU - (jack[k] - origin);
    num += t[k] * d * d * d;
    den += t[k] * d * d;
  }

  // A jackknife that does not move means the acceleration is genuinely
  // zero and BCa reduces to the bias-corrected interval. The old code
  // computed 0/0 here and let the NaN travel into an array index.
  // The threshold is on the jackknife SD, not on den itself: C lives in
  // [0, 1], so anything below 1e-12 is noise rather than signal.
  if (!(std::sqrt(den / n) > 1e-12))
    return 0.0;

  return num / (6.0 * std::pow(den, 1.5));
}
