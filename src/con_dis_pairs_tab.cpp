
//  Rewritten by Andri Signorell in C++
//  Concordant and discordant pairs for a contingency table
//
//  Cumulative sums instead of a 2D Fenwick tree: O(r c) rather than
//  O(r c log r log c), and no helper class at namespace scope. The former
//  class BIT2D collided with the struct of the same name in assoc.cpp - two
//  different layouts under one name violate the one-definition rule, the
//  linker kept one set of inline member functions for both files, and
//  ordAssocs() segfaulted on macOS/arm64.

#include <Rcpp.h>
#include <vector>
#include <climits>
using namespace Rcpp;


// [[Rcpp::export]]
List condis_pairs_tab_cpp(IntegerMatrix x) {

  const int n = x.nrow(), m = x.ncol();

  // Callers such as .tableAssocCI() reach this without the checks in
  // conDisPairs(), so the C++ guards its own preconditions.
  long long total = 0;
  for (R_xlen_t k = 0; k < x.size(); ++k) {
    if (x[k] == NA_INTEGER || x[k] < 0)
      stop("Table counts must be non-negative and not missing.");
    total += x[k];
  }
  if (total > INT_MAX)
    stop("Table counts must not add up to more than .Machine$integer.max.");

  // P(i, j) = sum of x over rows < i and columns < j
  std::vector<long long> P((size_t)(n + 1) * (m + 1), 0);
  auto at = [&](int i, int j) -> long long& {
    return P[(size_t)i * (m + 1) + j];
  };
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < m; ++j)
      at(i + 1, j + 1) = x(i, j) + at(i, j + 1) + at(i + 1, j) - at(i, j);

  // sum over the inclusive rectangle [r1, r2] x [c1, c2], zero when empty
  auto rect = [&](int r1, int c1, int r2, int c2) -> long long {
    if (r1 > r2 || c1 > c2) return 0;
    return at(r2 + 1, c2 + 1) - at(r1, c2 + 1) - at(r2 + 1, c1) + at(r1, c1);
  };

  IntegerMatrix pi_c(n, m), pi_d(n, m);
  std::vector<long long> rowSum(n, 0), colSum(m, 0);
  long long C = 0, D = 0, Ties_XY = 0;

  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      const long long nij = x(i, j);
      const long long pc = rect(0, 0, i - 1, j - 1) + rect(i + 1, j + 1, n - 1, m - 1);
      const long long pd = rect(0, j + 1, i - 1, m - 1) + rect(i + 1, 0, n - 1, j - 1);

      // both are bounded by total <= INT_MAX
      pi_c(i, j) = (int)pc;
      pi_d(i, j) = (int)pd;

      C += pc * nij;
      D += pd * nij;
      Ties_XY += nij * (nij - 1) / 2;
      rowSum[i] += nij;
      colSum[j] += nij;
    }
  }

  long long Ties_X = 0, Ties_Y = 0;
  for (long long t : rowSum) Ties_X += t * (t - 1) / 2;
  for (long long t : colSum) Ties_Y += t * (t - 1) / 2;

  // The marginal sums count a pair tied in BOTH variables once in Ties_X
  // and once in Ties_Y. Subtracting the joint ties makes the two counts
  // exclusive - tied in x only, tied in y only - which is what
  // con_dis_pairs_xy.cpp returns and what makes the five numbers a
  // partition:
  //
  //   C + D + Ties_X + Ties_Y + Ties_XY = n(n-1)/2
  //
  // The classic tau-b formula wants the INCLUSIVE marginal counts; those
  // are Ties_X + Ties_XY and Ties_Y + Ties_XY.
  Ties_X -= Ties_XY;
  Ties_Y -= Ties_XY;

  // C and D count every pair twice, once from each of its two cells
  return List::create(
    Named("C")       = C / 2.0,
    Named("D")       = D / 2.0,
    Named("Ties_X")  = (double)Ties_X,
    Named("Ties_Y")  = (double)Ties_Y,
    Named("Ties_XY") = (double)Ties_XY,
    Named("pi.c")    = pi_c,
    Named("pi.d")    = pi_d
  );
}
