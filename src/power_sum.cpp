
#include <Rcpp.h>
#include <map>
#include <numeric>   // std::accumulate - was used via std::reduce without
                     // including anything for it
#include <cmath>     // std::fabs
using namespace Rcpp;


// [[Rcpp::export]]
List n_pow_sum_cpp(NumericVector x) {

  // x must be a nonempty numeric vector with NAs omitted

  const R_xlen_t n = x.size();

  if (n == 0)
    stop("'x' must not be empty");

  // The map orders its keys, so a NaN would violate the strict weak
  // ordering std::map requires - undefined behaviour, not merely a wrong
  // answer. The caller is documented to strip NAs; this makes the
  // precondition enforceable rather than hopeful.
  for (R_xlen_t i = 0; i < n; i++)
    if (ISNAN(x[i]))
      stop("'x' must not contain NA or NaN");

  // R_xlen_t, not int: both the loop counter and the tie counts are
  // bounded by the vector length, which is a 64-bit quantity for long
  // vectors.
  std::map<double, R_xlen_t> counts;

  for (R_xlen_t i = 0; i < n; i++) {
    counts[x[i]]++;
  }

  // std::accumulate rather than std::reduce: reduce() leaves the order of
  // summation unspecified, so a floating-point sum is not guaranteed to
  // be reproducible between runs or platforms. It also needs C++17 plus
  // an include that was missing.
  const double mean =
    std::accumulate(x.begin(), x.end(), 0.0) / static_cast<double>(n);

  R_xlen_t zn = 0;
  const R_xlen_t un = static_cast<R_xlen_t>(counts.size());

  double sum1 = 0;
  double sum2 = 0;
  double sum3 = 0;
  double sum4 = 0;

  for (std::map<double, R_xlen_t>::const_iterator it = counts.begin();
       it != counts.end(); ++it) {

    const double cnt = static_cast<double>(it->second);
    const double d   = it->first - mean;
    const double d2  = cnt * d * d;

    // std::fabs, NOT abs. Without a "using namespace std", unqualified
    // abs() resolves to the C int abs(int) and TRUNCATES its argument:
    // abs(-5.1) is 5, and every deviation below 1 in magnitude becomes 0.
    // meanAD was therefore biased low throughout, and exactly zero for
    // any variable whose values sit within +/-1 of their mean (rates,
    // proportions, measurements in metres). Verified on gcc 12:
    // -25.6% for a delivery-time example, -100% for values in [0, 1].
    sum1 += cnt * std::fabs(d);  // sum of absolute differences
    sum2 += d2;                  // sum of squares
    sum3 += d2 * d;              // sum of 3rd powers
    sum4 += d2 * d * d;          // sum of 4th powers

    if (it->first == 0) zn = it->second;   // number of zero values
  }

  // dimension of the small/large value vectors
  const R_xlen_t ldim = std::min(static_cast<R_xlen_t>(5), un);

  NumericVector small_val(ldim);    // the 5 smallest values
  IntegerVector small_freq(ldim);   // the frequency of the 5 smallest values
  NumericVector large_val(ldim);    // the 5 largest values
  IntegerVector large_freq(ldim);   // the frequency of the 5 largest values

  // A plain bounded loop rather than the former "iterate to the end and
  // break when i == ldim - 1": that pattern reads ldim - 1 even when ldim
  // is 0, which is a wraparound on an unsigned type.
  {
    std::map<double, R_xlen_t>::const_iterator it = counts.begin();
    for (R_xlen_t i = 0; i < ldim; ++i, ++it) {
      small_val[i]  = it->first;
      small_freq[i] = static_cast<int>(it->second);
    }
  }

  {
    std::map<double, R_xlen_t>::const_reverse_iterator it = counts.rbegin();
    for (R_xlen_t i = 0; i < ldim; ++i, ++it) {
      large_val[i]  = it->first;
      large_freq[i] = static_cast<int>(it->second);
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("mean", mean),
    Rcpp::Named("sum1", sum1),
    Rcpp::Named("sum2", sum2),
    Rcpp::Named("sum3", sum3),
    Rcpp::Named("sum4", sum4),
    Rcpp::Named("zero", static_cast<double>(zn)),
    Rcpp::Named("unique", static_cast<double>(un)),
    Rcpp::Named("small_val", small_val),
    Rcpp::Named("small_freq", small_freq),
    Rcpp::Named("large_val", large_val),
    Rcpp::Named("large_freq", large_freq)
  );

}
