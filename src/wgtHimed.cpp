#include <Rcpp.h>
using namespace Rcpp;

/*
 * We only need rPsort from R
 */
extern "C" {
  void rPsort(double*, int, int);
}

/*
 * ------------------------------------------------------------------
 * Embedded whimed / whimed_i template code (from robustbase)
 * ------------------------------------------------------------------
 */

/* === double weights ================================================= */

#define _d_whimed_

#define _WHIMED_        whimed
#define _WGT_TYPE_      double
#define _WGT_SUM_TYPE_  double

double _WHIMED_(double *a, _WGT_TYPE_ *w, int n,
                double* a_cand, double *a_srt, _WGT_TYPE_* w_cand)
{
  int n2, i, kcand;
  _WGT_SUM_TYPE_ wleft, wmid, wright, w_tot, wrest;
  double trial;
  
  w_tot = 0;
  for (i = 0; i < n; ++i)
    w_tot += w[i];
  wrest = 0;
  
  do {
    for (i = 0; i < n; ++i)
      a_srt[i] = a[i];
    
    n2 = n / 2;
    rPsort(a_srt, n, n2);
    trial = a_srt[n2];
    
    wleft = wmid = wright = 0;
    for (i = 0; i < n; ++i) {
      if (a[i] < trial)
        wleft += w[i];
      else if (a[i] > trial)
        wright += w[i];
      else
        wmid += w[i];
    }
    
    kcand = 0;
    if (2 * (wrest + wleft) > w_tot) {
      for (i = 0; i < n; ++i)
        if (a[i] < trial) {
          a_cand[kcand] = a[i];
          w_cand[kcand] = w[i];
          ++kcand;
        }
    }
    else if (2 * (wrest + wleft + wmid) <= w_tot) {
      for (i = 0; i < n; ++i)
        if (a[i] > trial) {
          a_cand[kcand] = a[i];
          w_cand[kcand] = w[i];
          ++kcand;
        }
      wrest += wleft + wmid;
    }
    else {
      return trial;
    }
    
    n = kcand;
    for (i = 0; i < n; ++i) {
      a[i] = a_cand[i];
      w[i] = w_cand[i];
    }
    
  } while (1);
}

#undef _WHIMED_
#undef _WGT_TYPE_
#undef _WGT_SUM_TYPE_
#undef _d_whimed_

/* === integer weights ================================================ */

#define _i_whimed_

#define _WHIMED_        whimed_i
#define _WGT_TYPE_      int
#define _WGT_SUM_TYPE_  int64_t

double _WHIMED_(double *a, _WGT_TYPE_ *w, int n,
                double* a_cand, double *a_srt, _WGT_TYPE_* w_cand)
{
  int n2, i, kcand;
  _WGT_SUM_TYPE_ wleft, wmid, wright, w_tot, wrest;
  double trial;
  
  w_tot = 0;
  for (i = 0; i < n; ++i)
    w_tot += w[i];
  wrest = 0;
  
  do {
    for (i = 0; i < n; ++i)
      a_srt[i] = a[i];
    
    n2 = n / 2;
    rPsort(a_srt, n, n2);
    trial = a_srt[n2];
    
    wleft = wmid = wright = 0;
    for (i = 0; i < n; ++i) {
      if (a[i] < trial)
        wleft += w[i];
      else if (a[i] > trial)
        wright += w[i];
      else
        wmid += w[i];
    }
    
    kcand = 0;
    if (2 * (wrest + wleft) > w_tot) {
      for (i = 0; i < n; ++i)
        if (a[i] < trial) {
          a_cand[kcand] = a[i];
          w_cand[kcand] = w[i];
          ++kcand;
        }
    }
    else if (2 * (wrest + wleft + wmid) <= w_tot) {
      for (i = 0; i < n; ++i)
        if (a[i] > trial) {
          a_cand[kcand] = a[i];
          w_cand[kcand] = w[i];
          ++kcand;
        }
      wrest += wleft + wmid;
    }
    else {
      return trial;
    }
    
    n = kcand;
    for (i = 0; i < n; ++i) {
      a[i] = a_cand[i];
      w[i] = w_cand[i];
    }
    
  } while (1);
}

#undef _WHIMED_
#undef _WGT_TYPE_
#undef _WGT_SUM_TYPE_
#undef _i_whimed_

/*
 * ------------------------------------------------------------------
 * Optional: R-callable wrappers (INTERNAL USE ONLY)
 * ------------------------------------------------------------------
 * Remove these entirely if you only call from C++.
 */

// [[Rcpp::export]]
double wgtHimed_cpp(NumericVector x, NumericVector w)
{
  int n = x.size();
  if (w.size() != n)
    stop("x and w must have same length");
  
  NumericVector a(x);
  NumericVector a_cand(n), a_srt(n), w_cand(n);
  
  return whimed(REAL(a), REAL(w), n,
                REAL(a_cand), REAL(a_srt), REAL(w_cand));
}

// [[Rcpp::export]]
double wgtHimedInt_cpp(NumericVector x, IntegerVector iw)
{
  int n = x.size();
  if (iw.size() != n)
    stop("x and iw must have same length");
  
  NumericVector a(x);
  NumericVector a_cand(n), a_srt(n);
  IntegerVector iw_cand(n);
  
  return whimed_i(REAL(a), INTEGER(iw), n,
                  REAL(a_cand), REAL(a_srt), INTEGER(iw_cand));
}
