
#define STRICT_R_HEADERS
#include <Rcpp.h>

#include <vector>
#include <algorithm>   // nth_element, min_element
#include <numeric>     // accumulate
#include <cmath>       // fabs

using namespace std;
using namespace Rcpp;


// Tukey's Biweight Robust Mean (tbrm_cpp).'
//   There must be no NAs in 'x' - non-finite values are reported as NA.
//
//   Input:
//     - x   Vector of numbers to be summarized by tbrm_cpp (std::vector<double>)
//     - C   Parameter C which adjusts the scaling of the data (double)
//   Output: numeric value
//
//   Rewritten by Andri Signorell in C++ based on C-Code by Mikko Korpela.
//


// median of a vector, changes the order of its elements
static double median_inplace(std::vector<double>& v) {

    const std::size_t n = v.size();
    const std::size_t half = n / 2;

    std::nth_element(v.begin(), v.begin() + (n % 2 == 1 ? half : half - 1), v.end());

    if (n % 2 == 1)
        return v[half];

    // n even: v[half-1] is the lower of the two middle values, the upper one
    // is the minimum of the (unordered) right part
    const double upper = *std::min_element(v.begin() + half, v.end());
    return (v[half - 1] + upper) / 2.0;
}


// [[Rcpp::export]]
double tbrm_cpp(const std::vector<double>& x, double C) {

    const std::size_t n = x.size();
    if (n == 0)
        return NA_REAL;

    // nth_element() needs a strict weak ordering; NA/NaN would make the
    // comparisons inconsistent, which is undefined behaviour. ISNAN() is R's
    // macro and covers NA_real_ as well as NaN.
    for (std::size_t i = 0; i < n; ++i)
        if (ISNAN(x[i]))
            return NA_REAL;

    std::vector<double> x2 = x;   // Copy of the data part of argument x

    // Median of x
    const double x_med = median_inplace(x2);

    // abs(x - median(x))  -- note that x2 has been permuted above, so
    // abs_x_dev[i] belongs to x2[i], which is all we need below
    std::vector<double> abs_x_dev(n);
    for (std::size_t i = 0; i < n; ++i)
        abs_x_dev[i] = std::fabs(x2[i] - x_med);

    // Median of abs_x_dev, stored in div_const
    double div_const = median_inplace(abs_x_dev);

    // This is a normalization constant (well, constant over x2[i])
    div_const = div_const * C + 1e-6;

    // Number of values x2[i] with non-zero weights
    std::size_t my_count = 0;

    // Recycling memory, i.e. renaming the same space
    std::vector<double>& wt = abs_x_dev;
    std::vector<double>& wtx = x2;  // Have to be careful not to overwrite too soon

    // Weights (wt) and weighted data (wtx)
    for (std::size_t i = 0; i < n; ++i) {
        double this_wt = (x2[i] - x_med) / div_const;
        if (this_wt >= -1.0 && this_wt <= 1.0) {  // absolute value <= 1
            this_wt = 1.0 - this_wt * this_wt;
            this_wt *= this_wt;
            wt[my_count] = this_wt;
            wtx[my_count++] = this_wt * x2[i];    // reads x2[i] before writing it
        }
    }

    // Sum of my_count values -- ONLY the first my_count entries carry the
    // weights, everything beyond is leftover from x2 / abs_x_dev
    if (my_count == 0)
        return NA_REAL;

    const double sum_wt =
        std::accumulate(wt.begin(), wt.begin() + my_count, 0.0);
    const double sum_wtx =
        std::accumulate(wtx.begin(), wtx.begin() + my_count, 0.0);

    if (sum_wt == 0.0)
        return NA_REAL;

    return sum_wtx / sum_wt;

}
