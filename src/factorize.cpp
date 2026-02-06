
#include <Rcpp.h>
#include <cstdint>
#include <vector>
#include <cmath>

using namespace Rcpp;

/* ============================================================
 64-bit sichere Arithmetik
 ============================================================ */

uint64_t mul_mod(uint64_t a, uint64_t b, uint64_t mod) {
  __uint128_t res = ( __uint128_t ) a * b;
  return (uint64_t)(res % mod);
}

uint64_t pow_mod(uint64_t base, uint64_t exp, uint64_t mod) {
  uint64_t res = 1;
  while (exp > 0) {
    if (exp & 1)
      res = mul_mod(res, base, mod);
    base = mul_mod(base, base, mod);
    exp >>= 1;
  }
  return res;
}

uint64_t gcd_u64(uint64_t a, uint64_t b) {
  while (b != 0) {
    uint64_t t = b;
    b = a % b;
    a = t;
  }
  return a;
}

/* ============================================================
 Deterministischer Miller–Rabin (64 Bit)
 ============================================================ */

bool miller_rabin_u64(uint64_t n) {
  if (n < 2) return false;
  for (uint64_t p : {2ULL, 3ULL, 5ULL, 7ULL, 11ULL, 13ULL, 17ULL}) {
    if (n == p) return true;
    if (n % p == 0) return n == p;
  }
  
  uint64_t d = n - 1, s = 0;
  while ((d & 1) == 0) {
    d >>= 1;
    ++s;
  }
  
  for (uint64_t a : {2ULL, 325ULL, 9375ULL, 28178ULL,
       450775ULL, 9780504ULL, 1795265022ULL}) {
    if (a % n == 0) continue;
    uint64_t x = pow_mod(a, d, n);
    if (x == 1 || x == n - 1) continue;
    
    bool composite = true;
    for (uint64_t r = 1; r < s; ++r) {
      x = mul_mod(x, x, n);
      if (x == n - 1) {
        composite = false;
        break;
      }
    }
    if (composite) return false;
  }
  return true;
}

/* ============================================================
 Pollard–Rho (64 Bit)
 ============================================================ */

uint64_t pollard_rho(uint64_t n) {
  if (n % 2 == 0) return 2;
  
  uint64_t x = 2, y = 2, c = 1, d = 1;
  
  auto f = [&](uint64_t v) {
    return (mul_mod(v, v, n) + c) % n;
  };
  
  while (d == 1) {
    x = f(x);
    y = f(f(y));
    uint64_t diff = x > y ? x - y : y - x;
    d = gcd_u64(diff, n);
  }
  
  if (d == n)
    return pollard_rho(n + 1);
  
  return d;
}

/* ============================================================
 Rekursive Faktorisierung
 ============================================================ */

void factor_rec(uint64_t n, std::vector<uint64_t>& factors) {
  if (n == 1) return;
  
  if (miller_rabin_u64(n)) {
    factors.push_back(n);
  } else {
    uint64_t d = pollard_rho(n);
    factor_rec(d, factors);
    factor_rec(n / d, factors);
  }
}

/* ============================================================
 R-Interfaces
 ============================================================ */

// [[Rcpp::export]]
bool is_prime_u64(double x) {
  if (x < 2 || x != std::floor(x) || x > 9.22e18)
    return false;
  return miller_rabin_u64((uint64_t)x);
}

// [[Rcpp::export]]
IntegerVector primes_upto(int n) {
  if (n < 2) return IntegerVector();
  
  std::vector<bool> is_prime(n + 1, true);
  is_prime[0] = is_prime[1] = false;
  
  for (int i = 2; (int64_t)i * i <= n; ++i)
    if (is_prime[i])
      for (int j = i * i; j <= n; j += i)
        is_prime[j] = false;
    
  std::vector<int> primes;
  
  for (int i = 2; i <= n; ++i)
    if (is_prime[i])
      primes.push_back(i);
      
  return wrap(primes);
  
}

// // [[Rcpp::export]]
// NumericVector factor_u64(double x) {
//   if (x < 2 || x != std::floor(x) || x > 9.22e18)
//     stop("x must be an integer in [2, 2^63)");
//   
//   uint64_t n = (uint64_t)x;
//   std::vector<uint64_t> factors;
//   factor_rec(n, factors);
//   
//   NumericVector out(factors.size());
//   for (size_t i = 0; i < factors.size(); ++i)
//     out[i] = (double)factors[i];
//   
//   return out;
// }


// [[Rcpp::export]]
NumericMatrix factor_u64(double x) {
  if (x < 2 || x != std::floor(x) || x > 9.22e18)
    stop("x must be an integer in [2, 2^63)");
  
  uint64_t n = (uint64_t)x;
  std::vector<uint64_t> f;
  factor_rec(n, f);          // liefert sortierte Faktoren
  
  // Faktoren zaehlen (linear, kein table/unique)
  std::vector<uint64_t> p, m;
  for (size_t i = 0; i < f.size();) {
    uint64_t val = f[i];
    size_t cnt = 1;
    while (++i < f.size() && f[i] == val)
      ++cnt;
    p.push_back(val);
    m.push_back(cnt);
  }
  
  NumericMatrix out(p.size(), 2);
  for (size_t i = 0; i < p.size(); ++i) {
    out(i, 0) = (double)p[i];
    out(i, 1) = (double)m[i];
  }
  
  colnames(out) = CharacterVector::create("p", "m");
  return out;
}


