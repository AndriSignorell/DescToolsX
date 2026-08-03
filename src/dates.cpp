
#include <Rcpp.h>
using namespace Rcpp;

// Rcpp::Date counts days since 1970-01-01 and breaks down via gmtime, so
// everything in this file is time-zone free. That is a property worth
// keeping: the calendar day of a POSIXct is decided on the R side (see
// .asDateInTz() in date-functions.R), and nothing here should ever reach
// for localtime().

// Everything below is file-local except the [[Rcpp::export]] entry
// points. Yearday() used to have external linkage under a very generic
// name - a second definition anywhere else in src/ would have collided
// at link time, the C++ counterpart of two .nctCI() definitions in R.
namespace {

// Helper function: Compute day of the year (1-based)
int Yearday(int year, int month, int day) {

  static const int daysBeforeMonth[2][12] = {
    { 0,  31,  59,  90, 120, 151, 181, 212, 243, 273, 304, 334 }, // Normal year
    { 0,  31,  60,  91, 121, 152, 182, 213, 244, 274, 305, 335 }  // Leap year
  };

  bool isLeap = (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);

  return daysBeforeMonth[isLeap][month - 1] + day;

}

// ISO 8601: the week of a date is the week of the Thursday in it, and
// the ISO year is the calendar year that Thursday falls in. Returning
// the Thursday lets every caller read both off the same value instead of
// re-deriving the year from the week number.
Date isoThursday(const Date& d) {

  int days_n = d.getDate();      // days since 1970-01-01

  // (days_n + 4) % 7 is negative for dates before 1970 because C++ %
  // truncates towards zero; + 7 % 7 brings it back into 0..6 so that the
  // weekday index is right by construction rather than by cancellation
  int wday = ((days_n + 4) % 7 + 7) % 7;   // 0 = Sunday ... 6 = Saturday
  int isoIdx = (wday + 6) % 7;             // 0 = Monday ... 6 = Sunday

  return d + (3 - isoIdx);
}

int isoWeekOf(const Date& thursday) {
  Date jan1(thursday.getYear(), 1, 1);
  return 1 + (thursday - jan1) / 7;
}

} // namespace


// ************************ week ***************

// [[Rcpp::export]]
IntegerVector isoWeek_cpp(DateVector x) {

  R_xlen_t n = x.size();
  IntegerVector weeks(n);

  for (R_xlen_t i = 0; i < n; i++) {

    if (R_IsNA(x[i])) {
      weeks[i] = NA_INTEGER;
      continue;
    }

    weeks[i] = isoWeekOf(isoThursday(x[i]));
  }

  return weeks;

}


// [[Rcpp::export]]
IntegerVector usWeek_cpp(DateVector x) {

  R_xlen_t n = x.size();
  IntegerVector weeks(n);

  for (R_xlen_t i = 0; i < n; i++) {

    if (R_IsNA(x[i])) {
      weeks[i] = NA_INTEGER;
      continue;
    }

    Date curr_d = x[i];

    // NOTE: this counts fixed seven-day blocks from 1 January, so week 1
    // is 1-7 January whatever weekday the year starts on. It is NOT the
    // Sunday-based %U convention the old comment claimed, and not %W
    // either - for 2019 it gives 1 where %U gives 0 for 1-5 January, and
    // 53 where %U gives 52 for 31 December. Behaviour left unchanged; see
    // the note next to week() about which definition "us" should have.
    weeks[i] = (Yearday(curr_d.getYear(), curr_d.getMonth(), curr_d.getDay()) - 1) / 7 + 1;

  }

  return weeks;
}


// ************************ year ***************


// [[Rcpp::export]]
IntegerVector isoYear_cpp(DateVector x) {

  R_xlen_t n = x.size();
  IntegerVector res(n);

  for (R_xlen_t i = 0; i < n; i++) {

    if (R_IsNA(x[i])) {
      res[i] = NA_INTEGER;
      continue;
    }

    // The Thursday's calendar year IS the ISO year, by definition. The
    // previous version derived it instead from the week number and the
    // month (iw == 1 && m == 12 -> y + 1, and so on) - equivalent for
    // every case I could construct, but it re-derived something that was
    // already sitting in z.
    res[i] = isoThursday(x[i]).getYear();
  }

  return res;

}


// ************************ yearWeek ***************


// [[Rcpp::export]]
IntegerVector isoYearweek_cpp(DateVector x) {

  R_xlen_t n = x.size();
  IntegerVector res(n);

  for (R_xlen_t i = 0; i < n; i++) {

    if (R_IsNA(x[i])) {
      res[i] = NA_INTEGER;
      continue;
    }

    Date thu = isoThursday(x[i]);
    res[i] = thu.getYear() * 100 + isoWeekOf(thu);
  }

  return res;

}


// [[Rcpp::export]]
IntegerVector usYearweek_cpp(DateVector x) {

  R_xlen_t n = x.size();
  IntegerVector res(n);

  for (R_xlen_t i = 0; i < n; i++) {

    if (R_IsNA(x[i])) {
      res[i] = NA_INTEGER;
      continue;
    }

    Date curr_d = x[i];

    int y = curr_d.getYear();
    int w = (Yearday(y, curr_d.getMonth(), curr_d.getDay()) - 1) / 7 + 1;

    res[i] = y * 100 + w;
  }

  return res;

}



// ************************ Yearmonth ***************



// [[Rcpp::export]]
IntegerVector usYearmonth_cpp(DateVector x) {

  R_xlen_t n = x.size();
  IntegerVector res(n);

  for (R_xlen_t i = 0; i < n; i++) {

    // every other routine in this file guards NA; these two did not, so
    // a missing date came out as whatever getYear() made of it
    if (R_IsNA(x[i])) {
      res[i] = NA_INTEGER;
      continue;
    }

    Date curr_d = x[i];
    res[i] = curr_d.getYear() * 100 + curr_d.getMonth();
  }

  return res;

}


// A predicate should answer TRUE/FALSE, not 1/0: these returned an
// IntegerVector, so isLeapYear() handed back integers and
// expect_true(isLeapYear(d)) failed on the type.
// [[Rcpp::export]]
LogicalVector isLeapYearDate_cpp(DateVector x) {

  R_xlen_t n = x.size();
  LogicalVector res(n);

  for (R_xlen_t i = 0; i < n; i++) {

    if (R_IsNA(x[i])) {
      res[i] = NA_LOGICAL;
      continue;
    }

    int y = Date(x[i]).getYear();
    res[i] = (y % 4 == 0 && y % 100 != 0) || (y % 400 == 0);

  }

  return res;

}


// [[Rcpp::export]]
LogicalVector isLeapYearInt_cpp(IntegerVector x) {

  R_xlen_t n = x.size();
  LogicalVector res(n);

  for (R_xlen_t i = 0; i < n; i++) {

    // NA_INTEGER is INT_MIN, and INT_MIN % 4 == 0 while INT_MIN % 100 is
    // -48, so a missing year used to come back as TRUE
    if (IntegerVector::is_na(x[i])) {
      res[i] = NA_LOGICAL;
      continue;
    }

    res[i] = (x[i] % 4 == 0 && x[i] % 100 != 0) || (x[i] % 400 == 0);
  }

  return res;

}
