//
// Port of Monahan's algorithm for the Hodges-Lehmann estimator
// (one- and two-sample case)
//
// https://dl.acm.org/doi/10.1145/1271.319414
// https://www4.stat.ncsu.edu/~monahan/jul10/
//
// by Cyril Flurin Moser
//
// 2023-11-29


#define STRICT_R_HEADERS
#include <Rcpp.h>

#include <algorithm>
#include <cfloat>
#include <cmath>
#include <cstdint>
#include <vector>

using namespace Rcpp;

namespace {

// Pivot selection only needs to be arbitrary, not reproducible: the
// Hodges-Lehmann estimate is an exact order statistic of the Walsh
// averages (or pairwise differences), so every pivot path reaches the
// same answer. The original drew from R's generator via R::runif(); the
// RNGScope in the generated wrapper kept that consistent, but the point
// estimate then consumed R's stream - calling hodgesLehmann() shifted
// every later random draw. A local xorshift keeps the estimate
// independent of R's RNG and of set.seed(). It holds no shared state;
// that says nothing about the R API calls elsewhere in this file.
class Xorshift {
public:
  explicit Xorshift(uint64_t seed) : s_(seed ? seed : 0x9E3779B97F4A7C15ULL) {}

  // draw in [0, max]; the modulo bias is irrelevant for a pivot choice
  long long next(long long max) {
    s_ ^= s_ << 13;
    s_ ^= s_ >> 7;
    s_ ^= s_ << 17;
    if (max <= 0) return 0;
    return static_cast<long long>(s_ % static_cast<uint64_t>(max + 1));
  }

private:
  uint64_t s_;
};


// Every iteration strictly shrinks the candidate set or switches to the
// midrange step, so the expected count is O(log n). The cap only turns a
// would-be endless loop into an error.
const int kMaxIter = 10000;


// Pair sums (one-sample), differences (two-sample) and the final
// (amn + amx) can reach 4 * max|x|. Scaling by a power of two is exact
// (barring subnormals), so data above DBL_MAX / 8 are scaled by 1/8 and
// the result scaled back. Only finite values decide; infinities stay
// infinite under scaling.
double scaleFactor(const std::vector<double>& a,
                   const std::vector<double>& b = std::vector<double>()) {
  double mx = 0.0;
  for (double v : a) if (std::isfinite(v)) mx = std::max(mx, std::fabs(v));
  for (double v : b) if (std::isfinite(v)) mx = std::max(mx, std::fabs(v));
  return (mx > DBL_MAX / 8.0) ? 0.125 : 1.0;
}

void scale(std::vector<double>& a, double f) {
  if (f != 1.0) for (double& v : a) v *= f;
}

bool anyNaN(const std::vector<double>& a) {
  for (double v : a) if (std::isnan(v)) return true;
  return false;
}

} // namespace


// Mean of the k1-th and k2-th smallest Walsh average (1-based,
// k2 in {k1, k1 + 1}). For the median, k1 = (nn+1)/2, k2 = (nn+2)/2;
// other k are needed when infinite order statistics are split off.
// x: sorted, finite, n >= 1, scaled so that 4 * max|x| is finite.
static double hlqest_core(const std::vector<double>& x,
                          long long k1, long long k2) {

  const long long n = static_cast<long long>(x.size());

  if (n == 1) return x[0];
  if (n == 2) {
    const double w[3] = {x[0], (x[0] + x[1]) / 2.0, x[1]};
    return (w[k1 - 1] + w[k2 - 1]) / 2.0;
  }

  std::vector<long long> lb(n), rb(n), q(n);

  const long long nn = n * (n + 1) / 2;  // total number of pairs

  for (long long i = 0; i < n; i++) {
    lb[i] = i + 1;
    rb[i] = n;
  }

  long long sm = nn;  // number in set s at step m
  long long l  = 0;   // number of pairs less than those in set s at step m

  const long long mdll = (n + 1) / 2;
  const long long mdlu = (n + 2) / 2;

  double am = x[mdll - 1] + x[mdlu - 1];
  double amx = 0.0, amn = 0.0;

  Xorshift rng(static_cast<uint64_t>(n) * 6364136223846793005ULL + 1);

  // midrange of set s (labels 30/40); true when the search is finished
  auto midrange = [&]() -> bool {
    amx = x[0] + x[0];
    amn = x[n - 1] + x[n - 1];
    for (long long i = 0; i < n; i++) {
      if (lb[i] > rb[i]) continue;   // no element of this row is in s
      amn = std::min(amn, x[lb[i] - 1] + x[i]);
      amx = std::max(amx, x[rb[i] - 1] + x[i]);
    }
    if (sm == 2) {
      // s holds ranks l+1 and l+2: either they are k1, k2 or one of
      // them is the single target (k1 == k2, not only the median case)
      am = (k1 < k2) ? (amn + amx) / 2.0 : (k1 == l + 1 ? amn : amx);
      return true;
    }
    am = (amx + amn) / 2.0;
    // be careful to cut off something, roundoff can do weird things
    if ((am <= amn) || (am > amx)) am = amx;
    return amn == amx;
  };

  for (int iter = 0; ; iter++) {  // 80

    if (iter == kMaxIter)
      Rcpp::stop("hlqest: no convergence after %d iterations", kMaxIter);

    // Partition step: q[i] = number of pairs x[i] + x[j] < am in row i.
    // j decreases monotonically across rows, which keeps this linear.
    long long j  = n;
    long long sq = 0;

    for (long long i = 1; i <= n; i++) {
      q[i - 1] = 0;
      for (; j >= i; j--) {
        if (x[i - 1] + x[j - 1] < am) {
          q[i - 1] = j - i + 1;
          sq += q[i - 1];
          break;
        }
      }
    }

    // nearly done, with the values we want on the border?
    if (sq == k2 - 1 || sq == k1) {  // 180
      amn = x[n - 1] + x[n - 1];
      amx = x[0] + x[0];
      for (long long i = 1; i <= n; i++) {
        const long long iq   = q[i - 1];
        const long long ipiq = i + iq;
        const double curiX   = x[i - 1];
        if (iq > 0)          amx = std::max(amx, curiX + x[ipiq - 2]);
        if (iq < n - i + 1)  amn = std::min(amn, curiX + x[ipiq - 1]);
      }
      if (k1 < k2)      return (amn + amx) / 4.0;
      if (sq == k1)     return amx / 2.0;
      if (sq == k1 - 1) return amn / 2.0;
      return (amn + amx) / 4.0;
    }

    // Tested only after the border test: sq == l == 0 is a legitimate
    // finish for k1 == 1, not a tie.
    if (sq == l) {  // consecutive partitions identical: ties are likely
      if (midrange()) return am / 2.0;
      continue;
    }

    if (sq < k1) {  // 140 reset left bounds
      for (long long i = 0; i < n; i++) lb[i] = i + q[i] + 1;
    } else {        // 120 reset right bounds
      for (long long i = 0; i < n; i++) rb[i] = i + q[i];
    }

    // 160
    l  = 0;
    sm = 0;
    for (long long i = 1; i <= n; i++) {
      l  += lb[i - 1] - i;
      sm += rb[i - 1] - lb[i - 1] + 1;
    }

    if (sm > 2) {  // 50 random row, its median as pivot
      // k indexes the sm candidates: 0 .. sm - 1
      long long k = rng.next(sm - 1);
      long long jj = 1;
      for (long long i = 0; i < n; i++) {
        jj = i + 1;
        if (k <= rb[i] - lb[i]) break;
        k = k - rb[i] + lb[i] - 1;
      }
      const long long mdlrow = (lb[jj - 1] + rb[jj - 1]) / 2;
      am = x[jj - 1] + x[mdlrow - 1];
      continue;  // go to 80
    }

    // at most two elements left
    if (midrange()) return am / 2.0;
  }
}


// Mean of the k1-th and k2-th smallest difference x[j] - y[i]
// (1-based, k2 in {k1, k1 + 1}).
// x, y: sorted, finite, m, n >= 1, scaled so that 4 * max is finite.
static double hl2qest_core(const std::vector<double>& x,
                           const std::vector<double>& y,
                           long long k1, long long k2) {

  const long long m = static_cast<long long>(x.size());
  const long long n = static_cast<long long>(y.size());

  const long long nn = m * n;

  if (n == 1) return (x[k1 - 1] + x[k2 - 1]) / 2.0 - y[0];
  // x[0] - y[i] DEcreases in i: the k-th smallest is x[0] - y[n - k]
  if (m == 1) return x[0] - (y[n - k1] + y[n - k2]) / 2.0;

  std::vector<long long> lb(n), rb(n), q(n);

  double am = 0.0, amx = 0.0, amn = 0.0;

  // All differences admitted at the start. The former row bounds
  //   lb[i] = max(1, m + 1 - (nn + 1 - k1) / (i + 1)),
  //   rb[i] = min(m, k2 / (n - i))
  // assume the opposite row order (x[j] - y[i] DEcreases in i for
  // ascending y) and were not matched by sm/l, which can freeze the
  // search, e.g. x = c(0,0,1,1,1), y = c(0,0,0,1,1,1). The first
  // partition trims the set anyway, so nothing is lost.
  for (long long i = 0; i < n; i++) {
    lb[i] = 1;
    rb[i] = m;
  }

  long long sm = nn;
  long long l  = 0;
  long long j  = 0;
  long long sq = 0;

  int method = 1;

  Xorshift rng(static_cast<uint64_t>(m) * 1000003ULL +
               static_cast<uint64_t>(n) + 1);

  for (int iter = 0; ; iter++) {

    if (iter == kMaxIter)
      Rcpp::stop("hl2qest: no convergence after %d iterations", kMaxIter);

    if (method == 1) {

      am = x[((m + 1) / 2) - 1] - y[((n + 1) / 2) - 1];

    } else if (method == 2) {

      amx = x[0] - y[n - 1];
      amn = x[m - 1] - y[0];
      for (long long i = 0; i < n; i++) {
        if (lb[i] <= rb[i]) {
          amn = std::min(amn, x[lb[i] - 1] - y[i]);
          amx = std::max(amx, x[rb[i] - 1] - y[i]);
        }
      }
      if (sm == 2)   // ranks l+1, l+2; see hlqest_core()
        return (k1 < k2) ? (amn + amx) / 2.0 : (k1 == l + 1 ? amn : amx);
      am = (amx + amn) / 2.0;
      if ((am <= amn) || (am > amx)) am = amx;   // roundoff
      if (amn == amx) return am;

    } else {

      long long k = rng.next(sm - 1);   // 0 .. sm - 1
      for (long long i = 0; i < n; i++) {
        j = i;
        if (k <= (rb[i] - lb[i])) break;
        k = k - rb[i] + lb[i] - 1;
      }
      const long long mdlrow = (lb[j] + rb[j]) / 2 - 1;
      am = x[mdlrow] - y[j];
    }

    // Partition step. j is carried across rows on purpose: y is sorted,
    // so the threshold moves monotonically and the scan stays linear.
    j  = 0;
    sq = 0;
    for (long long i = 1; i <= n; i++) {
      while (j < m) {
        if ((x[j] - y[i - 1]) >= am) break;
        j++;
      }
      q[i - 1] = j;
      sq += j;
    }

    // border test first: sq == l == 0 is a legitimate finish for k1 == 1
    if ((sq == (k2 - 1)) || (sq == k1)) break;

    if (sq == l) {
      method = 2;
    } else {
      if (sq > k1) {
        for (long long i = 0; i < n; i++) rb[i] = q[i];
      } else {
        for (long long i = 0; i < n; i++) lb[i] = q[i] + 1;
      }
      l  = 0;
      sm = 0;
      for (long long i = 0; i < n; i++) {
        l  += lb[i] - 1;
        sm += rb[i] - lb[i] + 1;
      }
      method = 3;
    }

    if (sm == 2) method = 2;
  }

  amn = x[m - 1] - y[0];
  amx = x[0] - y[n - 1];
  for (long long i = 0; i < n; i++) {
    const long long iq = q[i];
    if (iq > 0) amx = std::max(amx, x[iq - 1] - y[i]);
    if (iq < m) amn = std::min(amn, x[iq] - y[i]);
  }

  if (k1 < k2)      return (amn + amx) / 2.0;
  if (sq == k1)     return amx;
  if (sq == k1 - 1) return amn;

  return (amn + amx) / 2.0;
}


namespace {

// The data prepared for order-statistic queries: sorted finite parts plus
// the counts of infinite Walsh averages / differences. Infinities are
// resolved by counting, never inside the search, where (-Inf + Inf) / 2
// would poison the midrange step.
struct HLSet {
  enum Status { OK, MISSING, UNDEFINED };

  Status status = OK;
  bool two = false;        // two-sample: differences x - y
  long long nn = 0;        // number of Walsh averages / differences
  long long NL = 0;        // ... of which -Inf (the NL smallest)
  long long NU = 0;        // ... of which +Inf (the NU largest)
  std::vector<double> fx, fy;   // finite parts, sorted and scaled
  double sf = 1.0;              // scale factor, see scaleFactor()

  // Mean of the K1-th and K2-th smallest (1-based, K2 in {K1, K1 + 1}).
  // Straddling -Inf and +Inf gives NaN, as median(c(-Inf, Inf)).
  double between(long long K1, long long K2) const {

    if (status == MISSING)   return NA_REAL;
    if (status == UNDEFINED) return R_NaN;

    auto cls = [&](long long K) { return K <= NL ? -1 : (K > nn - NU ? 1 : 0); };
    const int c1 = cls(K1), c2 = cls(K2);

    if (c1 == 0 && c2 == 0) return finite(K1 - NL, K2 - NL);

    auto val = [&](int c, long long K) {
      return c < 0 ? -INFINITY : c > 0 ? INFINITY : finite(K - NL, K - NL);
    };
    return (val(c1, K1) + val(c2, K2)) / 2.0;
  }

  double median() const { return between((nn + 1) / 2, (nn + 2) / 2); }

private:
  double finite(long long k1, long long k2) const {
    return (two ? hl2qest_core(fx, fy, k1, k2) : hlqest_core(fx, k1, k2)) / sf;
  }
};


// Contract of the exported functions, also for direct calls from the
// boot statistic:
//   * empty input or any NA/NaN           -> NA
//   * an Inf - Inf pair (one-sample: -Inf and Inf in x; two-sample: the
//     same infinity in x and y)            -> NaN, the pair has no value
//   * otherwise infinities are ordinary order statistics
//   * inputs are copied, never modified

// sorted copy; number of leading -Inf and trailing +Inf
std::vector<double> sortedCopy(const NumericVector& v, bool& nan,
                               long long& nNeg, long long& nPos) {
  std::vector<double> s(v.begin(), v.end());
  nan = anyNaN(s);
  if (nan) return s;
  std::sort(s.begin(), s.end());
  nNeg = std::upper_bound(s.begin(), s.end(), -INFINITY) - s.begin();
  nPos = s.end() - std::lower_bound(s.begin(), s.end(), INFINITY);
  return s;
}

HLSet prepare1(const NumericVector& xIn) {

  HLSet h;
  long long a = 0, b = 0;
  bool nan = false;

  if (xIn.size() == 0) { h.status = HLSet::MISSING; return h; }
  std::vector<double> x = sortedCopy(xIn, nan, a, b);
  if (nan)            { h.status = HLSet::MISSING;   return h; }
  if (a > 0 && b > 0) { h.status = HLSet::UNDEFINED; return h; }

  // Walsh averages with at least one -Inf (+Inf) partner are -Inf (+Inf)
  const long long n = static_cast<long long>(x.size());
  h.nn = n * (n + 1) / 2;
  h.NL = a * (a + 1) / 2 + a * (n - a);
  h.NU = b * (b + 1) / 2 + b * (n - b);

  h.fx.assign(x.begin() + a, x.end() - b);
  h.sf = scaleFactor(h.fx);
  scale(h.fx, h.sf);
  return h;
}

HLSet prepare2(const NumericVector& xIn, const NumericVector& yIn) {

  HLSet h;
  h.two = true;
  long long a = 0, b = 0, c = 0, d = 0;   // -Inf / +Inf in x (a, b), y (c, d)
  bool nanx = false, nany = false;

  if (xIn.size() == 0 || yIn.size() == 0) { h.status = HLSet::MISSING; return h; }
  std::vector<double> x = sortedCopy(xIn, nanx, a, b);
  std::vector<double> y = sortedCopy(yIn, nany, c, d);
  if (nanx || nany) { h.status = HLSet::MISSING; return h; }
  if ((a > 0 && c > 0) || (b > 0 && d > 0)) {         // Inf - Inf
    h.status = HLSet::UNDEFINED;
    return h;
  }

  h.fx.assign(x.begin() + a, x.end() - b);
  h.fy.assign(y.begin() + c, y.end() - d);

  const long long n  = static_cast<long long>(y.size());
  const long long mf = static_cast<long long>(h.fx.size());

  // -Inf: x = -Inf (any y), or finite x - (+Inf);  +Inf: mirror image
  h.nn = static_cast<long long>(x.size()) * n;
  h.NL = a * n + d * mf;
  h.NU = b * n + c * mf;

  h.sf = scaleFactor(h.fx, h.fy);
  scale(h.fx, h.sf);
  scale(h.fy, h.sf);
  return h;
}

// k-th smallest for each k (1-based); k as double, nn can exceed INT_MAX
NumericVector orderStats(const HLSet& h, const NumericVector& k) {
  NumericVector res(k.size());
  for (R_xlen_t i = 0; i < k.size(); i++) {
    const double ki = k[i];
    if (!(ki >= 1) || ki != std::floor(ki) || (h.status == HLSet::OK && ki > h.nn))
      Rcpp::stop("k must be whole numbers in [1, %lld]", h.nn);
    const long long K = static_cast<long long>(ki);
    res[i] = h.between(K, K);
  }
  return res;
}

} // namespace


// Hodges-Lehmann estimate: median of the Walsh averages
// [[Rcpp::export]]
double hlqest_cpp(NumericVector x) {
  return prepare1(x).median();
}

// Hodges-Lehmann shift estimate: median of the differences x[j] - y[i]
// [[Rcpp::export]]
double hl2qest_cpp(NumericVector x, NumericVector y) {
  return prepare2(x, y).median();
}

// k-th smallest Walsh averages, for the rank-based confidence interval
// [[Rcpp::export]]
NumericVector hlqorder_cpp(NumericVector x, NumericVector k) {
  return orderStats(prepare1(x), k);
}

// k-th smallest differences x[j] - y[i]
// [[Rcpp::export]]
NumericVector hl2qorder_cpp(NumericVector x, NumericVector y, NumericVector k) {
  return orderStats(prepare2(x, y), k);
}


// == conditional null distributions for the exact interval under ties ======
//
// Scores are doubled midranks, i.e. non-negative integers. Both functions
// return an upper tail P(T >= s) of the permutation distribution given
// the scores, as needed to invert the signed-rank / rank-sum test.

// T = sum_i w_i U_i, U_i iid Bernoulli(1/2): the signed-rank statistic.
// Counts are at most 2^n, exact in double for n <= 53; the caller uses
// this for n < 50 only.
// [[Rcpp::export]]
double signrankUpper_cpp(IntegerVector w, double s) {

  long long tot = 0;
  for (int wi : w) tot += wi;

  std::vector<double> f(tot + 1, 0.0);   // f[t]: sign vectors with sum t
  f[0] = 1.0;
  long long cur = 0;
  for (int wi : w) {
    for (long long t = cur; t >= 0; t--) f[t + wi] += f[t];
    cur += wi;
  }

  double num = 0.0;
  for (long long t = std::max(0LL, static_cast<long long>(std::ceil(s))); t <= tot; t++)
    num += f[t];

  return std::ldexp(num, -static_cast<int>(w.size()));
}


// T = sum of a random k-subset (without replacement) of the scores w: the
// rank-sum statistic of the first sample, k = its size.
// [[Rcpp::export]]
double wilcoxUpper_cpp(IntegerVector w, int k, double s) {

  const int N = w.size();
  long long tot = 0;
  for (int wi : w) tot += wi;

  // the smaller subset is cheaper: T = tot - T', T' over N - k elements,
  // and P(T >= s) = P(T' <= tot - s)
  const bool compl_ = k > N - k;
  const int kk = compl_ ? N - k : k;

  // g[j][t]: number of j-subsets with score sum t
  std::vector<std::vector<double>> g(kk + 1, std::vector<double>(tot + 1, 0.0));
  g[0][0] = 1.0;
  long long cur = 0;
  for (int i = 0; i < N; i++) {
    const int wi = w[i];
    for (int j = std::min(i + 1, kk); j >= 1; j--)
      for (long long t = cur; t >= 0; t--)
        if (g[j - 1][t] != 0.0) g[j][t + wi] += g[j - 1][t];
    cur += wi;
  }

  double num = 0.0, den = 0.0;
  for (long long t = 0; t <= tot; t++) {
    const double c = g[kk][t];
    den += c;
    if (compl_ ? (t <= tot - s) : (t >= s)) num += c;
  }
  return num / den;
}
