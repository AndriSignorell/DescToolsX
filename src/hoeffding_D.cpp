

#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

// ===============================
  // Fenwick Tree (Binary Indexed Tree)
// ===============================
  template <typename T>
  class Fenwick {
    private:
      int n;
    std::vector<T> tree;
    
    public:
      Fenwick(int n_) : n(n_), tree(n_ + 1, 0) {}
    
    // add value at index i (1-based)
    inline void update(int i, T val) {
      for (; i <= n; i += i & -i){
        tree[i] += val;
      }
    }
    
    // prefix sum [1..i]
    inline T query(int i) const {
      T sum = 0;
      for (; i > 0; i -= i & -i){
        sum += tree[i];
      }
        return sum;
    }
  };

// ===============================
  // Check if vector is permutation 0,...,n-1
// ===============================
  bool is_valid_permutation(const std::vector<unsigned long>& perm) {
    int n = perm.size();
    std::vector<bool> seen(n, false);
    
    for (auto v : perm) {
      if (v >= (unsigned long)n || seen[v]) return false;
      seen[v] = true;
    }
    return true;
  }

// ===============================
  // Core Hoeffding count (O(n log n))
// ===============================
  template <typename T>
  T hoeffding_count(const std::vector<unsigned long>& perm) {
    int n = perm.size();
    
    Fenwick<T> fenw(n);
    T sum = 0;
    
    for (int i = 0; i < n; ++i) {
      
      int y = perm[i] + 1;  // Fenwick is 1-based
      
      // a = number of previous points
      T a = (T)i;
      
      // b = rank of current y
      T b = (T)perm[i];
      
      // c = number of previous y_j <= y_i
      T c = fenw.query(y);
      
      // update tree AFTER query
      fenw.update(y, 1);
      
      // safe casting already done via T
      sum += (a * (a - 1) / 2) * (b * (b - 1))
      - ((a - 1) * (b - 1)) * c * (n - 2)
      + (c * (c - 1) / 2) * (n - 2) * (n - 3);
      
      // interrupt check (fast version)
      if ((i & 16383) == 0)
        Rcpp::checkUserInterrupt();
    }
    
    return sum;
  }


  
// [[Rcpp::export]]
double hoeffdingD_cpp(std::vector<unsigned long> perm) {
  
  size_t n = perm.size();
  
  if (n < 5)
    stop("n must be >= 5");
  
  if (!is_valid_permutation(perm))
    stop("Input must be a permutation of 0,...,n-1");
  
  double dn = static_cast<double>(n);
  double denom = dn * (dn - 1) * (dn - 2) * (dn - 3) * (dn - 4);
  
  // ---- 64-bit ----
  if (n <= 14081) {
    long long count = hoeffding_count<long long>(perm);
//    return static_cast<double>(count) * 2.0 / denom;
    return static_cast<double>(count) * 2.0 * 30.0 / denom;
  }
  
  // Use GCC/Clang extension __int128 for large n to avoid overflow.
  // Falls back to 64-bit integers if not available (e.g., MSVC).
  #if defined(__SIZEOF_INT128__) && !defined(_MSC_VER)
  {
    __int128 count = hoeffding_count<__int128>(perm);
//    return static_cast<double>(count) * 2.0 / denom;
    return static_cast<double>(count) * 2.0 * 30.0 / denom;
  }
  #endif
  
  stop("n too large: 128-bit integers not available on this platform");
}
  
  
  
  
  
  
