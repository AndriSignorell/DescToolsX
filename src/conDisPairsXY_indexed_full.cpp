

//   Rewritten by Andri Signorell in C++

#include <Rcpp.h>
#include <vector>
#include <unordered_map>
#include <set>
#include <algorithm>
#include <cmath>
#include <random>

using namespace Rcpp;
using namespace std;



// 2D Binary Indexed Tree (Fenwick Tree)
struct BIT2D {
  int size_x, size_y;
  vector<vector<int>> tree;
  
  BIT2D(int sx, int sy) : size_x(sx), size_y(sy), tree(sx + 2, vector<int>(sy + 2, 0)) {}
  
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
    return query(x2, y2) - query(x1 - 1, y2)
    - query(x2, y1 - 1) + query(x1 - 1, y1 - 1);
  }
};




// [[Rcpp::export]]
Rcpp::NumericVector conDisPairsXY_indexed_full(Rcpp::NumericVector xR,
                                               Rcpp::NumericVector yR) {
  
  using namespace Rcpp;
  using std::vector;
  using std::pair;
  
  int n = xR.size();
  
  // ---- filter valid pairs ----
    vector<pair<double,double>> valid;
  valid.reserve(n);
  
  for(int i=0;i<n;i++){
    if(!NumericVector::is_na(xR[i]) && !NumericVector::is_na(yR[i]))
      valid.emplace_back(xR[i], yR[i]);
  }
  
  int nn = valid.size();
  
  // ---- consistent NA return ----
    if(nn < 2){
      return NumericVector::create(
        _["C"]=NA_REAL, _["D"]=NA_REAL,
        _["Ties_X"]=NA_REAL, _["Ties_Y"]=NA_REAL, _["Ties_XY"]=NA_REAL,
        _["S1x"]=NA_REAL, _["S2x"]=NA_REAL, _["S3x"]=NA_REAL,
        _["S1y"]=NA_REAL, _["S2y"]=NA_REAL, _["S3y"]=NA_REAL,
        _["S2xy"]=NA_REAL,
        _["n"]=nn
      );
    }
  
  // ---- coordinate compression ----
    std::set<double> xset, yset;
  for(auto &p : valid){
    xset.insert(p.first);
    yset.insert(p.second);
  }
  
  std::unordered_map<double,int> xmap, ymap;
  
  int idx=0;
  for(double v : xset) xmap[v] = idx++;
  int xsize = idx;
  
  idx=0;
  for(double v : yset) ymap[v] = idx++;
  int ysize = idx;
  
  // ---- compressed points + counts ----
    vector<pair<int,int>> pts;
  pts.reserve(nn);
  
  std::unordered_map<int,int> count_x;
  std::unordered_map<int,int> count_y;
  std::unordered_map<long long,int> count_xy;
  
  for(auto &p : valid){
    int xi = xmap[p.first];
    int yi = ymap[p.second];
    
    pts.emplace_back(xi, yi);
    
    count_x[xi]++;
    count_y[yi]++;
    
    long long key = ((long long)xi << 32) | yi;
    count_xy[key]++;
  }
  
  // ---- sort points ----
    std::sort(pts.begin(), pts.end());
  
  // ---- BIT ----
    BIT2D bit(xsize, ysize);
  
  long long C = 0, D = 0;
  
  // ============================================================
    // IMPORTANT:
    // We only count pairs with previously seen points.
  // Due to sorting by x, each pair is counted exactly once.
  // No need for upper quadrants or multiplication by 2.
  // ============================================================
    
    size_t i = 0;
  while(i < pts.size()){
    
    size_t j = i;
    while(j < pts.size() && pts[j] == pts[i]) j++;
    
    // ---- query block ----
      for(size_t k=i;k<j;k++){
        int xi = pts[k].first;
        int yi = pts[k].second;
        
        int concordant = 0;
        if(xi > 0 && yi > 0)
          concordant = bit.query_range(0,0,xi-1,yi-1);
        
        int discordant = 0;
        if(xi > 0 && yi < ysize-1)
          discordant = bit.query_range(0,yi+1,xi-1,ysize-1);
        
        C += concordant;
        D += discordant;
      }
    
    // ---- update block ----
      for(size_t k=i;k<j;k++){
        bit.update(pts[k].first, pts[k].second, 1);
      }
    
    i = j;
  }
  
  // ---- tie statistics ----
    double S1x=0,S2x=0,S3x=0;
  double S1y=0,S2y=0,S3y=0;
  double S2xy=0;
  
  double tiesX=0, tiesY=0, tiesXY=0;
  
  for(auto &kv : count_x){
    double t = kv.second;
    S1x += t*(t-1);
    S2x += t*(t-1)*(2*t+5);
    S3x += t*(t-1)*(t-2);
    tiesX += t*(t-1)/2.0;
  }
  
  for(auto &kv : count_y){
    double t = kv.second;
    S1y += t*(t-1);
    S2y += t*(t-1)*(2*t+5);
    S3y += t*(t-1)*(t-2);
    tiesY += t*(t-1)/2.0;
  }
  
  for(auto &kv : count_xy){
    double v = kv.second;
    S2xy += v*(v-1)*(2*v+5);
    tiesXY += v*(v-1)/2.0;
  }
  
  // ---- output ----
    return NumericVector::create(
      _["C"] = (double)C,
      _["D"] = (double)D,
      _["Ties_X"] = tiesX,
      _["Ties_Y"] = tiesY,
      _["Ties_XY"] = tiesXY,
      _["S1x"] = S1x,
      _["S2x"] = S2x,
      _["S3x"] = S3x,
      _["S1y"] = S1y,
      _["S2y"] = S2y,
      _["S3y"] = S3y,
      _["S2xy"] = S2xy,
      _["n"] = nn
    );
}