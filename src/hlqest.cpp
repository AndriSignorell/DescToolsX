//
// Port of Monahans algorithm for Hodges-Lehman estimator
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
#include <cstdint>
#include <vector>

using namespace Rcpp;

// <iostream> and <cstdio> were included but unused; iostream in
// particular is discouraged in R packages (static initialisation, and
// R CMD check flags std::cout usage).
// "using namespace std;" at file scope is gone as well - std::sort and
// std::min are spelled out.

namespace {

// Pivot selection only needs to be arbitrary, not reproducible: the
// Hodges-Lehmann estimate is an exact order statistic of the Walsh
// averages, so every pivot path reaches the same answer. The original
// drew from R's generator via R::runif() without the mandatory
// GetRNGstate()/PutRNGstate() bracket, which
//   * leaves .Random.seed unsynchronised, so the draws neither respect
//     set.seed() nor advance R's stream, and
//   * is not safe to call from a forked worker, while the bootstrap in
//     hodgesLehmann() offers parallel = "multicore".
// A local xorshift removes the dependency on R's RNG altogether and
// makes the routine deterministic and thread-safe.
class Xorshift {
public:
  explicit Xorshift(uint64_t seed) : s_(seed ? seed : 0x9E3779B97F4A7C15ULL) {}

  // uniform-ish draw in [0, max]; the modulo bias is irrelevant for a
  // pivot choice
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

} // namespace


// [[Rcpp::export]]
double hlqest_cpp(NumericVector xIn) {

  const long long n = xIn.size();

  if (n == 0) return NA_REAL;

  // COPY, not a view. NumericVector is a proxy onto the caller's SEXP,
  // so sorting it in place reordered the vector the user passed in:
  //
  //   v <- c(3, 1, 2); hodgesLehmann(v); v   # 1 2 3
  //
  // Only na.omit() upstream produced a copy, i.e. the vector survived
  // exactly when NAs had to be dropped and was mangled in the ordinary
  // case. std::vector also gives the working arrays below RAII, which
  // removes the malloc()/R_Free() mismatch and the leak that any throw
  // between the three allocations used to cause.
  std::vector<double> x(xIn.begin(), xIn.end());
  std::sort(x.begin(), x.end());

  if (n == 1) return x[0];
  if (n == 2) return (x[0] + x[1]) / 2.0;

  std::vector<long long> lb(n), rb(n), q(n);

  const long long nn = n * (n + 1) / 2;  // total number of pairs
  const long long k1 = (nn + 1) / 2;     // median(s)
  const long long k2 = (nn + 2) / 2;

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

  // The "use the midrange of set s" block (labels 30/40) appeared twice,
  // character for character. Returns true when the search is finished.
  auto midrange = [&]() -> bool {
    amx = x[0] + x[0];
    amn = x[n - 1] + x[n - 1];
    for (long long i = 0; i < n; i++) {
      if (lb[i] > rb[i]) continue;   // no element of this row is in s
      amn = std::min(amn, x[lb[i] - 1] + x[i]);
      amx = std::max(amx, x[rb[i] - 1] + x[i]);
    }
    am = (amx + amn) / 2.0;
    // be careful to cut off something, roundoff can do weird things
    if ((am <= amn) || (am > amx)) am = amx;
    return (amn == amx) || (sm == 2);
  };

  for (;;) {  // 80

    // Partition step: split s into x[i] + x[j] < am and >= am.
    // q[i] = how many pairs x[i] + x[j] < am in row i. j decreases
    // monotonically across rows, which is what makes this linear.
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

    if (sq == l) {  // consecutive partitions identical: ties are likely
      if (midrange()) return am / 2.0;
      continue;
    }

    // are we nearly done, with the values we want on the border?
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

    if (sq < k1) {  // 140 reset left bounds for each row
      for (long long i = 0; i < n; i++) lb[i] = i + q[i] + 1;
    } else {        // 120 reset right bounds for each row
      for (long long i = 0; i < n; i++) rb[i] = i + q[i];
    }

    // 160
    l  = 0;   // number of pairs less than those in the new set s
    sm = 0;   // number of pairs still in the new set s
    for (long long i = 1; i <= n; i++) {
      l  += lb[i - 1] - i;
      sm += rb[i - 1] - lb[i - 1] + 1;
    }

    if (sm > 2) {  // 50 pick a random row and take its median as pivot
      // long long throughout: sm starts at n(n+1)/2, which overflows an
      // int from about n = 65536 on, and the old rng() took an int
      long long k = rng.next(sm);
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

    // only two elements left, which can only happen when k1 != k2
    if (midrange()) return am / 2.0;
  }
}


// [[Rcpp::export]]
double hl2qest_cpp(NumericVector xIn, NumericVector yIn) {

  const long long m = xIn.size();
  const long long n = yIn.size();

  // guard both: hodgesLehmann() checked only length(x), so an empty y
  // reached the "method 2" branch below and read y[n - 1] = y[-1]
  if (m == 0 || n == 0) return NA_REAL;

  // copies, for the same reason as in hlqest_cpp()
  std::vector<double> x(xIn.begin(), xIn.end());
  std::vector<double> y(yIn.begin(), yIn.end());
  std::sort(x.begin(), x.end());
  std::sort(y.begin(), y.end());

  const long long nn = m * n;
  const long long k1 = (nn + 1) / 2;
  const long long k2 = (nn + 2) / 2;

  if (n == 1) return (x[k1 - 1] + x[k2 - 1]) / 2.0 - y[0];
  if (m == 1) return x[0] - (y[k1 - 1] + y[k2 - 1]) / 2.0;

  std::vector<long long> lb(n), rb(n), q(n);

  double am = 0.0, amx = 0.0, amn = 0.0;

  for (long long i = 0; i < n; i++) {
    lb[i] = std::max(1LL, (m + 1) - (nn + 1 - k1) / (i + 1));
    rb[i] = std::min(m, k2 / (n - i));
  }

  long long sm = nn;
  long long l  = 0;
  long long j  = 0;
  long long sq = 0;

  int method = 1;

  Xorshift rng(static_cast<uint64_t>(m) * 1000003ULL +
               static_cast<uint64_t>(n) + 1);

  for (;;) {

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
      am = (amx + amn) / 2.0;
      if ((am <= amn) || (am > amx)) am = amx;   // roundoff
      if ((amn == amx) || (sm == 2)) return am;

    } else {

      long long k = rng.next(sm);
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

    if (sq == l) {
      method = 2;
    } else {
      if ((sq == (k2 - 1)) || (sq == k1)) break;
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
