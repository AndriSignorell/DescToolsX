
//  Association measures including confidence intervals based on fast
//  concordant discordant pairs calculation

//  Rewritten by Andri Signorell in C++

#include <Rcpp.h>
#include <vector>
#include <unordered_map>
#include <set>
#include <algorithm>
#include <cmath>
#include <limits>

  
// 2D Binary Indexed Tree (Fenwick Tree)
struct BIT2D {
  int size_x, size_y;
  std::vector<std::vector<int>> tree;
  
  BIT2D(int sx, int sy)
  : size_x(sx), size_y(sy),
  tree(sx + 2, std::vector<int>(sy + 2, 0)) {}
  
  void update(int x, int y, int val) {
    for (int i = x + 1; i <= size_x + 1; i += i & -i)
      for (int j = y + 1; j <= size_y + 1; j += j & -j)
        tree[i][j] += val;
  }
  
  int query(int x, int y) const {
    int sum = 0;
    for (int i = x + 1; i > 0; i -= i & -i){
      for (int j = y + 1; j > 0; j -= j & -j){
        sum += tree[i][j];
      }
    }  
    return sum;
  }
  
  int query_range(int x1, int y1, int x2, int y2) const {
    return query(x2, y2)
    - query(x1 - 1, y2)
    - query(x2, y1 - 1)
    + query(x1 - 1, y1 - 1);
  }
};



// [[Rcpp::export]]
Rcpp::NumericVector assoc_cpp(Rcpp::NumericVector xR,
                                 Rcpp::NumericVector yR,
                                 double conf_level = 0.95) {
  
  using namespace Rcpp;
  using std::vector;
  using std::pair;
  
  // ---- NA output helper ----
    auto naOut = []() {
      return NumericVector::create(
        _["somers"]=NA_REAL,  _["somers_l"]=NA_REAL,  _["somers_u"]=NA_REAL,
        _["gamma"]=NA_REAL,   _["gamma_l"]=NA_REAL,   _["gamma_u"]=NA_REAL,
        _["tau_a"]=NA_REAL,   _["tau_a_l"]=NA_REAL,   _["tau_a_u"]=NA_REAL,
        _["tau_b"]=NA_REAL,   _["tau_b_l"]=NA_REAL,   _["tau_b_u"]=NA_REAL,
        _["tau_c"]=NA_REAL,   _["tau_c_l"]=NA_REAL,   _["tau_c_u"]=NA_REAL,
        _["cstat"]=NA_REAL,   _["cstat_l"]=NA_REAL,   _["cstat_u"]=NA_REAL,
        _["C"]=NA_REAL,       _["D"]=NA_REAL,
        _["Tx"]=NA_REAL,      _["Ty"]=NA_REAL,        _["Txy"]=NA_REAL
      );
    };
    
    int n = xR.size();
    
    // ---- filter valid ----
      vector<pair<double,double>> valid;
    valid.reserve(n);
    for (int i = 0; i < n; i++) {
      if (!NumericVector::is_na(xR[i]) && !NumericVector::is_na(yR[i]))
        valid.emplace_back(xR[i], yR[i]);
    }
    
    int nn = (int)valid.size();
    if (nn < 2) return naOut();
    
    // ---- coordinate compression ----
      std::set<double> xset, yset;
    for (auto &p : valid) {
      xset.insert(p.first);
      yset.insert(p.second);
    }
    
    std::unordered_map<double,int> xmap, ymap;
    int idx = 0;
    for (double v : xset) xmap[v] = idx++;
    int xsize = idx;
    
    idx = 0;
    for (double v : yset) ymap[v] = idx++;
    int ysize = idx;
    
    // ---- compressed points ----
      vector<pair<int,int>> pts;
    pts.reserve(nn);
    for (auto &p : valid)
      pts.emplace_back(xmap[p.first], ymap[p.second]);
    
    std::sort(pts.begin(), pts.end());
    
    // ---- cell stats + per-observation C/D (single pair of passes) ----
      struct CellStats {
        int    count = 0;
        double C     = 0.0;
        double D     = 0.0;
      };
    std::map<std::pair<int,int>, CellStats> cells;
    
    vector<double> Ci(nn, 0.0), Di(nn, 0.0), Tyi(nn, 0.0);
    
    // forward pass
    {
      BIT2D bit(xsize, ysize);
      for (int i = 0; i < nn; i++) {
        int xi = pts[i].first;
        int yi = pts[i].second;
        double ci = 0.0, di = 0.0;
        if (xi > 0 && yi > 0)
          ci = bit.query_range(0, 0, xi-1, yi-1);
        if (xi > 0 && yi < ysize-1)
          di = bit.query_range(0, yi+1, xi-1, ysize-1);
        Ci[i] += ci;  Di[i] += di;
        cells[{xi,yi}].C     += ci;
        cells[{xi,yi}].D     += di;
        cells[{xi,yi}].count++;
        bit.update(xi, yi, 1);
      }
    }
    
    // backward pass
    {
      BIT2D bit(xsize, ysize);
      for (int i = nn-1; i >= 0; i--) {
        int xi = pts[i].first;
        int yi = pts[i].second;
        double ci = 0.0, di = 0.0;
        if (xi < xsize-1 && yi < ysize-1)
          ci = bit.query_range(xi+1, yi+1, xsize-1, ysize-1);
        if (xi < xsize-1 && yi > 0)
          di = bit.query_range(xi+1, 0, xsize-1, yi-1);
        Ci[i] += ci;  Di[i] += di;
        cells[{xi,yi}].C += ci;
        cells[{xi,yi}].D += di;
        bit.update(xi, yi, 1);
      }
    }
    
    // ---- ties in Y per observation (Somers SE) ----
      {
        std::unordered_map<int, vector<int>> groups_y;
        for (int i = 0; i < nn; i++)
          groups_y[pts[i].second].push_back(i);
        for (auto &g : groups_y) {
          int k = (int)g.second.size();
          for (int ii : g.second) Tyi[ii] = k - 1;
        }
      }
    
    // ---- global C, D ----
      double C = 0.0, D = 0.0;
    for (int i = 0; i < nn; i++) { C += Ci[i]; D += Di[i]; }
    C /= 2.0;  D /= 2.0;
    
    double S  = C - D;
    double n0 = nn * (nn - 1) / 2.0;
    
    // ---- tie counts ----
      double Tx = 0.0, Ty = 0.0, Txy = 0.0;
    std::unordered_map<int,int> count_x, count_y;
    std::unordered_map<long long,int> count_xy;
    
    for (auto &p : pts) {
      count_x[p.first]++;
      count_y[p.second]++;
      long long key = ((long long)p.first << 32) | (unsigned int)p.second;
      count_xy[key]++;
    }
    for (auto &kv : count_x)  { double t = kv.second; Tx  += t*(t-1)/2.0; }
    for (auto &kv : count_y)  { double t = kv.second; Ty  += t*(t-1)/2.0; }
    for (auto &kv : count_xy) { double t = kv.second; Txy += t*(t-1)/2.0; }
    
    // ---- CI helper (clamped to [-1, 1]) ----
      double alpha = 1.0 - conf_level;
    double zcrit = R::qnorm(1.0 - alpha/2.0, 0.0, 1.0, 1, 0);
    auto makeCI = [&](double est, double se) {
      return std::make_pair(
        std::max(est - zcrit*se, -1.0),
        std::min(est + zcrit*se,  1.0)
      );
    };
    
    // ============================================================
    // Somers D
    // ============================================================
      double T_     = n0 - Ty;
    double somers = S / T_;
    
    double seSomers = 0.0;
    {
      vector<double> psi(nn);
      for (int i = 0; i < nn; i++) {
        double Si = Ci[i] - Di[i];
        double Ti = (nn - 1) - Tyi[i];
        psi[i] = (Si - somers * Ti) / T_;
      }
      double meanPsi = 0.0;
      for (double v : psi) meanPsi += v;
      meanPsi /= nn;
      double varPsi = 0.0;
      for (double v : psi) varPsi += (v - meanPsi)*(v - meanPsi);
      seSomers = std::sqrt(varPsi);
    }
    auto ci_somers = makeCI(somers, seSomers);
    
    // ============================================================
    // Gamma  (Goodman & Kruskal)
    //
    // psi_cell = 2 * (D * C_cell - C * D_cell) / (C+D)^2
    // sigma2   = sum(count * psi^2) - (sum(count * psi))^2 / n
    // Ref: gkGamma() in DescTools
    // ============================================================
    double gamma    = NA_REAL;
    auto ci_gamma   = std::make_pair(NA_REAL, NA_REAL);
    
    if (C + D > 0.0) {
      gamma = S / (C + D);
      double cd2 = (C + D) * (C + D);
      double sp1 = 0.0, sp2 = 0.0;
      for (auto &kv : cells) {
        int    cnt = kv.second.count;
        double Cc  = kv.second.C / cnt;   // per-obs concordant
        double Dc  = kv.second.D / cnt;   // per-obs discordant
        double psi_c = 2.0 * (D * Cc - C * Dc) / cd2;
        sp1 += cnt * psi_c * psi_c;
        sp2 += cnt * psi_c;
      }
      double sigma2_gamma = sp1 - sp2 * sp2 / nn;
      if (sigma2_gamma < 0.0) sigma2_gamma = 0.0;
      ci_gamma = makeCI(gamma, std::sqrt(sigma2_gamma));
    }
    
    // ============================================================
    // tau-a  (Kendall)
    //
    // Hollander & Wolfe pp. 415/416:
    // Ci_cell = (C_cell - D_cell) per observation, non-empty cells only
    // C_      = sum(Ci_cell) / n
    // sigma2  = 2/(n*(n-1)) * ((2*(n-2))/(n*(n-1)^2)*sum((Ci-C_)^2) + 1 - tau_a^2)
    // Ref: kendallTauA() in DescTools
    // ============================================================
    double tau_a    = S / n0;
    auto   ci_tau_a = std::make_pair(NA_REAL, NA_REAL);
    {
      vector<double> CiVec;
      CiVec.reserve(cells.size());
      for (auto &kv : cells) {
        int cnt = kv.second.count;
        CiVec.push_back(kv.second.C / cnt - kv.second.D / cnt);
      }
      double C_ = 0.0;
      for (double v : CiVec) C_ += v;
      C_ /= nn;
      
      double sumSq = 0.0;
      for (double v : CiVec) sumSq += (v - C_) * (v - C_);
      
      double sigma2_a = 2.0 / ((double)nn*(nn-1))
      * ( (2.0*(nn-2)) / ((double)nn*(nn-1)*(nn-1)) * sumSq
          + 1.0 - tau_a*tau_a );
      if (sigma2_a < 0.0) sigma2_a = 0.0;
      ci_tau_a = makeCI(tau_a, std::sqrt(sigma2_a));
    }
    
    // ============================================================
    // tau-b  (Meijer asymptotic SE, matches kendallTauB())
    // ============================================================
    double denom_b  = std::sqrt((n0 - Tx) * (n0 - Ty));
    double tau_b    = S / denom_b;
    auto   ci_tau_b = std::make_pair(NA_REAL, NA_REAL);
    {
      double Pdiff = 2.0 * S / ((double)nn * nn);
      
      vector<double> rowsum_v(xsize, 0.0);
      vector<double> colsum_v(ysize, 0.0);
      for (auto &kv : cells) {
        double pi_cell = kv.second.count / (double)nn;
        rowsum_v[kv.first.first]  += pi_cell;
        colsum_v[kv.first.second] += pi_cell;
      }
      
      double sum_r2 = 0.0, sum_c2 = 0.0;
      for (double v : rowsum_v) sum_r2 += v*v;
      for (double v : colsum_v) sum_c2 += v*v;
      
      double delta1  = std::sqrt(1.0 - sum_r2);
      double delta2  = std::sqrt(1.0 - sum_c2);
      double delta12 = delta1 * delta2;
      
      double sum_pi_tp  = 0.0;
      double sum_pi_tp2 = 0.0;
      
      for (auto &kv : cells) {
        int    xi  = kv.first.first;
        int    yi  = kv.first.second;
        int    cnt = kv.second.count;
        double pi_cell    = cnt / (double)nn;
        double pdiff_cell = (kv.second.C / cnt - kv.second.D / cnt) / (double)nn;
        
        // bug:
        // double tauphi =
        //   (2.0 * pdiff_cell + Pdiff * colsum_v[yi]) * delta1 * delta2
        // + (Pdiff * rowsum_v[xi] * delta2) / delta1;
        
        double tauphi =
          2.0 * pdiff_cell * delta1 * delta2
        + Pdiff * rowsum_v[xi] * delta2 / delta1
        + Pdiff * colsum_v[yi] * delta1 / delta2;        
        
        sum_pi_tp  += pi_cell * tauphi;
        sum_pi_tp2 += pi_cell * tauphi * tauphi;
      }
      
      double sigma2 = (sum_pi_tp2 - sum_pi_tp * sum_pi_tp)
      / (delta12 * delta12 * delta12 * delta12)
      / (double)nn;
      if (sigma2 < std::numeric_limits<double>::epsilon() * 10)
        sigma2 = 0.0;
      ci_tau_b = makeCI(tau_b, std::sqrt(sigma2));
    }
    
    // ============================================================
    // tau-c  (Stuart)
    //
    // sigma2 = 4*m^2 / ((m-1)^2 * n^4)
    //          * (sum(count * (C_cell - D_cell)^2) - 4*S^2/n)
    // Ref: stuartTauC() in DescTools
    // ============================================================
    int    m       = std::min(xsize, ysize);
    double tau_c   = NA_REAL;
    auto   ci_tau_c = std::make_pair(NA_REAL, NA_REAL);
    
    if (m > 1) {
      double nn2 = (double)nn * nn;
      tau_c = 2.0 * m * S / (nn2 * (m - 1));
      
      double sumCD2 = 0.0;
      for (auto &kv : cells) {
        int    cnt = kv.second.count;
        double net = kv.second.C / cnt - kv.second.D / cnt;
        sumCD2 += cnt * net * net;
      }
      
      double sigma2_c = 4.0 * m * m
      / ((double)(m-1)*(m-1) * nn2*nn2)
      * (sumCD2 - 4.0 * S*S / nn);
      if (sigma2_c < 0.0) sigma2_c = 0.0;
      ci_tau_c = makeCI(tau_c, std::sqrt(sigma2_c));
    }
    
    // ============================================================
    // C-statistic  (= (Somers_D + 1) / 2)
    // ============================================================
    double cstat   = (somers          + 1.0) / 2.0;
    double cstat_l = (ci_somers.first + 1.0) / 2.0;
    double cstat_u = (ci_somers.second+ 1.0) / 2.0;
    
    // ============================================================
    // Output
    // ============================================================
      return NumericVector::create(
        _["somers"]   = somers,
        _["somers_l"] = ci_somers.first,
        _["somers_u"] = ci_somers.second,
        
        _["gamma"]    = gamma,
        _["gamma_l"]  = ci_gamma.first,
        _["gamma_u"]  = ci_gamma.second,
        
        _["tau_a"]    = tau_a,
        _["tau_a_l"]  = ci_tau_a.first,
        _["tau_a_u"]  = ci_tau_a.second,
        
        _["tau_b"]    = tau_b,
        _["tau_b_l"]  = ci_tau_b.first,
        _["tau_b_u"]  = ci_tau_b.second,
        
        _["tau_c"]    = tau_c,
        _["tau_c_l"]  = ci_tau_c.first,
        _["tau_c_u"]  = ci_tau_c.second,
        
        _["cstat"]    = cstat,
        _["cstat_l"]  = cstat_l,
        _["cstat_u"]  = cstat_u,
        
        _["C"]   = C,
        _["D"]   = D,
        _["Tx"]  = Tx,
        _["Ty"]  = Ty,
        _["Txy"] = Txy
      );
}

