
#include <Rcpp.h>
#include <vector>
#include <cstdint>
#include <cmath>
using namespace Rcpp;

// ===============================
// Fenwick Tree (Binary Indexed Tree)
// ===============================
template <typename T>
class Fenwick {
private:
  R_xlen_t n;
  std::vector<T> tree;

public:
  // R_xlen_t throughout: n came in as int, which truncates above 2^31.
  // That is out of reach for the 64-bit path (see the note on the 14081
  // threshold below) but not for the __int128 one.
  explicit Fenwick(R_xlen_t n_) : n(n_), tree(n_ + 1, 0) {}

  // add value at index i (1-based)
  inline void update(R_xlen_t i, T val) {
    for (; i <= n; i += i & -i) {
      tree[i] += val;
    }
  }

  // prefix sum [1..i]
  inline T query(R_xlen_t i) const {
    T sum = 0;
    for (; i > 0; i -= i & -i) {
      sum += tree[i];
    }
    return sum;
  }
};

// ===============================
// Check if vector is permutation 0,...,n-1
// ===============================
namespace {

bool is_valid_permutation(const std::vector<double>& perm) {
  const R_xlen_t n = static_cast<R_xlen_t>(perm.size());
  std::vector<bool> seen(n, false);

  for (R_xlen_t i = 0; i < n; ++i) {
    const double v = perm[i];
    // The input used to be taken as std::vector<unsigned long>, so a
    // negative or fractional value from R was converted before this
    // check ever saw it - and a negative double wraps to a huge unsigned
    // value rather than failing. Validate on the double and convert
    // afterwards.
    if (!R_finite(v) || v != std::floor(v) || v < 0 || v >= static_cast<double>(n))
      return false;
    const R_xlen_t k = static_cast<R_xlen_t>(v);
    if (seen[k]) return false;
    seen[k] = true;
  }
  return true;
}

// ===============================
// Core Hoeffding count (O(n log n))
// ===============================
template <typename T>
T hoeffding_count(const std::vector<R_xlen_t>& perm) {
  const R_xlen_t n = static_cast<R_xlen_t>(perm.size());

  Fenwick<T> fenw(n);
  T sum = 0;

  for (R_xlen_t i = 0; i < n; ++i) {

    const R_xlen_t y = perm[i] + 1;  // Fenwick is 1-based

    // a = number of previous points
    const T a = static_cast<T>(i);

    // b = rank of current y
    const T b = static_cast<T>(perm[i]);

    // c = number of previous y_j <= y_i
    const T c = fenw.query(y);

    // update tree AFTER query
    fenw.update(y, 1);

    const T nn = static_cast<T>(n);

    sum += (a * (a - 1) / 2) * (b * (b - 1))
      - ((a - 1) * (b - 1)) * c * (nn - 2)
      + (c * (c - 1) / 2) * (nn - 2) * (nn - 3);

    // interrupt check (fast version)
    if ((i & 16383) == 0)
      Rcpp::checkUserInterrupt();
  }

  return sum;
}

} // namespace


// [[Rcpp::export]]
double hoeffdingD_cpp(std::vector<double> permIn) {

  const R_xlen_t n = static_cast<R_xlen_t>(permIn.size());

  if (n < 5)
    stop("n must be >= 5");

  if (!is_valid_permutation(permIn))
    stop("Input must be a permutation of 0,...,n-1");

  std::vector<R_xlen_t> perm(n);
  for (R_xlen_t i = 0; i < n; ++i)
    perm[i] = static_cast<R_xlen_t>(permIn[i]);

  const double dn = static_cast<double>(n);
  const double denom = dn * (dn - 1) * (dn - 2) * (dn - 3) * (dn - 4);

  // ---- 64-bit ----
  //
  // The threshold is correct, but only just. Computed in exact
  // arithmetic for the monotone permutation - the worst case for the
  // running sum, which grows like n^5 - the peak partial sum is
  //
  //     n = 14081   9.2195e18     int64 max 9.2234e18
  //     n = 14082   9.2228e18
  //     n = 14083   overflow
  //
  // so this limit has a margin of exactly TWO observations. That is
  // fine as long as nobody edits it casually: raising it by even a
  // little silently produces wrong numbers rather than an error,
  // because signed overflow is undefined behaviour and the result just
  // looks like a plausible D. test-hoeffdingD.R asserts the bound.
  //
  // The monotone case is the worst one I could construct, not a proven
  // bound over all permutations.
  if (n <= 14081) {
    const long long count = hoeffding_count<long long>(perm);
    // x30, so the result is on the conventional scale where perfect
    // dependence is 1 - see the note in hoeffdingD()'s @return
    return static_cast<double>(count) * 2.0 * 30.0 / denom;
  }

  // Use GCC/Clang extension __int128 for large n to avoid overflow.
  // Falls back to an error if not available (e.g., MSVC).
#if defined(__SIZEOF_INT128__) && !defined(_MSC_VER)
  {
    const __int128 count = hoeffding_count<__int128>(perm);
    return static_cast<double>(count) * 2.0 * 30.0 / denom;
  }
#else
  stop("n > 14081 requires 128-bit integer support, "
       "which is not available on this platform");
#endif
}
