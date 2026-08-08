// [[Rcpp::depends(RcppParallel)]]

#include <Rcpp.h>
#include <RcppParallel.h>
#include <vector>
#include <random>
#include <cmath>
#include <limits>

#include "contcoef.h"

using namespace Rcpp;
using namespace RcppParallel;
using desctoolsx::contcoefImpl;

// ---------------------------------------------------------------------
// 1) Point estimate
// ---------------------------------------------------------------------

// [[Rcpp::export]]
double contcoef_table_cpp(NumericMatrix tab, bool correct = false)
{
  int r, c;
  double n;
  const std::vector<double> t = desctoolsx::tableToRowMajor(tab, r, c, n);

  return desctoolsx::contcoefImpl(t, r, c, n, correct);
}


// ---------------------------------------------------------------------
// 2) Bootstrap worker (multinomial)
// ---------------------------------------------------------------------

namespace {

struct BootWorkerContCoef : public Worker {

  const double* p;      // cell probabilities, row-major
  const int r;
  const int c;
  const int n;          // draws per replicate
  const bool correct;
  const unsigned int seed;

  RVector<double> out;

  BootWorkerContCoef(const double* p_, int r_, int c_, int n_,
                     bool correct_, unsigned int seed_,
                     NumericVector out_)
    : p(p_), r(r_), c(c_), n(n_), correct(correct_), seed(seed_),
      out(out_) {}

  void operator()(std::size_t begin, std::size_t end)
  {
    // per-thread scratch: nothing below allocates or touches the R API
    const std::size_t K = static_cast<std::size_t>(r) * c;

    std::vector<double> tabStar(K, 0.0);
    std::vector<double> rs(r), cs(c);

    // stateless, so hoisting it out of the replicate loop draws exactly
    // the same numbers as building it inside
    std::discrete_distribution<int> dist(p, p + K);

    for (std::size_t rr = begin; rr < end; ++rr) {

      // seed + rr, unchanged on purpose: F14 (02.08.2026) decided against
      // re-seeding schemes, because it would move every bootstrap bound
      // in the suite for no measurable gain
      std::mt19937 rng(seed + static_cast<unsigned int>(rr));

      std::fill(tabStar.begin(), tabStar.end(), 0.0);

      for (int i = 0; i < n; ++i)
        tabStar[dist(rng)] += 1.0;

      out[rr] = contcoefImpl(tabStar.data(), r, c,
                             static_cast<double>(n), correct, rs, cs);
    }
  }
};

}  // namespace


// ---------------------------------------------------------------------
// 3) Bootstrap entry point
// ---------------------------------------------------------------------
// Returns the replicates only. Turning them into interval bounds happens
// in R, in one place, for every interval type - so that "perc" and "bca"
// cannot end up with different quantile conventions, and so that no index
// arithmetic can reach past the end of the vector.

// [[Rcpp::export]]
NumericVector contcoef_table_boot_cpp(NumericMatrix tab,
                                      int R,
                                      unsigned int seed,
                                      bool correct = false)
{
  if (R < 1)
    stop("'R' must be at least 1");

  int r, c;
  double n;
  const std::vector<double> t = desctoolsx::tableToRowMajor(tab, r, c, n);

  NumericVector out(R, NA_REAL);

  // an empty table has no coefficient; returning a vector of the RIGHT
  // length keeps the caller's quantile() from failing on a length-1 answer
  if (!(n > 0.0))
    return out;

  // the resample draws n observations, so n has to BE a count - a weighted
  // table would have been silently truncated here
  if (n != std::floor(n) || n > static_cast<double>(std::numeric_limits<int>::max()))
    stop("the bootstrap needs whole-number counts; the total of 'tab' is not one");

  const std::size_t K = static_cast<std::size_t>(r) * c;

  std::vector<double> p(K);
  for (std::size_t k = 0; k < K; ++k)
    p[k] = t[k] / n;

  BootWorkerContCoef worker(p.data(), r, c, static_cast<int>(n),
                            correct, seed, out);

  parallelFor(0, R, worker);

  return out;
}
