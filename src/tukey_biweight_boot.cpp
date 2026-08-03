// ============================================================
// tbrm_boot.cpp
//
// Bootstrap confidence interval for Tukey's biweight mean.
//
// tbrm_cpp() is a C++ function defined elsewhere in the package.
// TbrmFn calls it directly — no R-API involved, so the
// parallel worker is fully safe.
//
// Uses the generic parallel bootstrap framework.
//
// R interface:
//   tbrm_boot_cpp(x, R = 999, alpha = 0.05,
//                 constant = 9, seed = -1, method = "perc")
//
//   NOTE: the tuning constant is called 'constant' and not 'const'
//   ('const' is a C++ keyword) - the R side has to name it accordingly.
//
// ============================================================

#include "boot_framework.h"

// forward declaration — tbrm_cpp() is defined in tbrm_cpp.cpp
double tbrm_cpp(const std::vector<double>& x, double C);


// ============================================================
// StatFn
// ============================================================

struct TbrmFn {

  double constant;

  explicit TbrmFn(double c = 9.0) : constant(c) {}

  double compute(const arma::mat& X,
                 const arma::vec& /* y */) const {

    // arma::vec → std::vector<double> for tbrm_cpp()
    const arma::vec col = X.col(0);
    std::vector<double> v(col.begin(), col.end());
    return tbrm_cpp(v, constant);
  }
};


// [[Rcpp::export]]
NumericVector tbrm_boot_cpp(NumericVector x,
                            int    R        = 999,
                            double alpha    = 0.05,
                            double constant = 9.0,
                            int    seed     = -1,
                            String method   = "perc") {

  if (R < 1)
    Rcpp::stop("'R' must be at least 1.");
  if (alpha <= 0.0 || alpha >= 1.0)
    Rcpp::stop("'alpha' must lie in (0, 1).");
  if (!(constant > 0.0))
    Rcpp::stop("'constant' must be a positive number.");
  if (x.size() < 2)
    Rcpp::stop("'x' must contain at least 2 observations.");

  return run_boot(
    vec_to_matrix(x),
    dummy_vec(x.size()),
    R,
    alpha,
    seed,
    TbrmFn(constant),
    std::string(method.get_cstring())
  );
}
