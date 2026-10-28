
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
using namespace Rcpp;


// Standardised central moment m_k / m_2^(k/2), k = 3 (skewness g_1) or
// k = 4 (g_2 + 3), optionally weighted (w == nullptr: unweighted).
//
// The ratio is invariant to shifting and scaling x and to scaling w. The
// steps below use exactly that, without giving away precision:
//
//  1. x is scaled by a POWER OF TWO, 2^-e with 2^e > max|x_i|. That is
//     exact (only results already below ~1e-308 relative to max|x| can be
//     rounded, and they are negligible against it), and it makes the later
//     subtraction safe: for x = c(-1e308, 1.7e308, 1.7e308) centring on the
//     original scale overflows (-1.8e308). Dividing by max|x_i| itself, as
//     the previous version did, rounded every value: with a large common
//     offset (1e16 + c(0, 2, 4, 6)) the small distances were destroyed and
//     skewness came out as 0.65 instead of 0.
//  2. The data are shifted by a reference (a rough mean) BEFORE the mean is
//     refined. Values close to the reference are subtracted exactly
//     (Sterbenz), the offset is gone, and the refined mean and the
//     deviations are formed on the small scale. Refining the mean on the
//     original scale does not work: adding the correction term back to a
//     number of size 1e16 rounds it away again.
//  3. The deviations are scaled by a power of two once more, so that the
//     powers lie in [-1, 1] and cannot overflow. Individual small
//     standardised terms can still underflow when the deviations span more
//     than ~77 orders of magnitude; their contribution is then below double
//     precision anyway.
//
// w is divided by max(w_i), so the weight sum cannot overflow. Observations
// with weight 0 are skipped everywhere, including in the scale factors, so
// they are absent in the C++ entry points as well.
//
// Empty, constant or non-finite input gives NaN; deciding what that means
// (NA plus a warning) is the R side's job, which also removes NAs.
static double stdMoment(const NumericVector& x, const double* w, int k) {

  const R_xlen_t n = x.size();

  double a = 0.0, wmax = 0.0;
  for (R_xlen_t i = 0; i < n; ++i) {
    const double wi = w ? w[i] : 1.0;
    if (wi == 0.0) continue;
    a    = std::max(a, std::fabs(x[i]));
    wmax = std::max(wmax, wi);
  }

  if (!(a > 0.0) || !std::isfinite(a) || !(wmax > 0.0) || !std::isfinite(wmax))
    return R_NaN;

  // 1. exact scaling: |x_i * 2^-ex| < 1
  int ex;
  std::frexp(a, &ex);

  // 2a. reference: rough weighted mean on that scale
  double vsum = 0.0, ref = 0.0;
  for (R_xlen_t i = 0; i < n; ++i) {
    const double vi = w ? w[i] / wmax : 1.0;
    if (vi == 0.0) continue;
    vsum += vi;
    ref  += vi * std::ldexp(x[i], -ex);
  }
  ref /= vsum;

  // 2b. mean of the shifted values, |d_i| < 2, with correction term
  double m = 0.0;
  for (R_xlen_t i = 0; i < n; ++i) {
    const double vi = w ? w[i] / wmax : 1.0;
    if (vi == 0.0) continue;
    m += vi * (std::ldexp(x[i], -ex) - ref);
  }
  m /= vsum;

  double corr = 0.0, s = 0.0;
  for (R_xlen_t i = 0; i < n; ++i) {
    const double vi = w ? w[i] / wmax : 1.0;
    if (vi == 0.0) continue;
    corr += vi * ((std::ldexp(x[i], -ex) - ref) - m);
  }
  m += corr / vsum;

  for (R_xlen_t i = 0; i < n; ++i) {
    const double vi = w ? w[i] / wmax : 1.0;
    if (vi == 0.0) continue;
    s = std::max(s, std::fabs((std::ldexp(x[i], -ex) - ref) - m));
  }

  if (!(s > 0.0))
    return R_NaN;

  // 3. exact scaling of the deviations: |z_i| < 1
  int es;
  std::frexp(s, &es);

  double m2 = 0.0, mk = 0.0;
  for (R_xlen_t i = 0; i < n; ++i) {
    const double vi = w ? w[i] / wmax : 1.0;
    if (vi == 0.0) continue;
    const double z  = std::ldexp((std::ldexp(x[i], -ex) - ref) - m, -es);
    const double z2 = z * z;
    m2 += vi * z2;
    mk += vi * (k == 3 ? z2 * z : z2 * z2);
  }

  m2 /= vsum;
  mk /= vsum;

  return mk / std::pow(m2, 0.5 * k);
}


static const double* weightPtr(const NumericVector& x, const NumericVector& w) {
  if (w.size() != x.size())
    stop("'x' and 'w' must have the same length");
  return w.begin();
}


// [[Rcpp::export]]
double skew_cpp(NumericVector x) {
  return stdMoment(x, nullptr, 3);
}

// [[Rcpp::export]]
double skew_weighted_cpp(NumericVector x, NumericVector w) {
  return stdMoment(x, weightPtr(x, w), 3);
}

// [[Rcpp::export]]
double kurt_cpp(NumericVector x) {
  return stdMoment(x, nullptr, 4) - 3.0;
}

// [[Rcpp::export]]
double kurt_weighted_cpp(NumericVector x, NumericVector w) {
  return stdMoment(x, weightPtr(x, w), 4) - 3.0;
}
