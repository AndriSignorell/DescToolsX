// ============================================================
// tbrm_boot.cpp
//
// Bootstrap confidence interval for Tukey's biweight mean.
//
// tbrm() is a C++ function defined elsewhere in the package.
// TbrmFn calls it directly — no R-API involved, so the
// parallel worker is fully safe.
//
// Uses the generic parallel bootstrap framework.
//
// R interface:
//   tbrm_boot_cpp(x, R = 999, alpha = 0.05,
//                 const = 9, seed = -1, method = "perc")
//
// ============================================================

#include "boot_framework.h"

// forward declaration — tbrm() is defined in tbrm.cpp
double tbrm(const std::vector<double>& x, double C);


// ============================================================
// StatFn
// ============================================================

struct TbrmFn {

  double constant;

  explicit TbrmFn(double c = 9.0) : constant(c) {}

  double compute(const arma::mat& X,
                 const arma::vec& /* y */) const {

    // arma::vec → std::vector<double> for tbrm()
    const arma::vec col = X.col(0);
    std::vector<double> v(col.begin(), col.end());
    return tbrm(v, constant);
  }
};


// [[Rcpp::export]]
NumericVector tbrm_boot_cpp(NumericVector x,
                            int    R        = 999,
                            double alpha    = 0.05,
                            double constant = 9.0,
                            int    seed     = -1,
                            String method   = "perc") {

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
