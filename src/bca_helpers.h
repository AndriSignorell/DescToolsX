#ifndef DESCTOOLSX_BCA_HELPERS_H
#define DESCTOOLSX_BCA_HELPERS_H

// ============================================================
// BCa helpers
// ============================================================
//
// EINBAU: in boot_framework.h den Abschnitt "BCa helpers" - also
// bca_z0(), bca_acceleration() und bca_quantile_levels() - loeschen
// und stattdessen
//
//     #include "bca_helpers.h"
//
// setzen. Sonst aendert sich dort nichts; boot_bca_ci() und
// boot_bca_ci_matrix() rufen dieselben drei Namen.
//
// Eigener Header, weil diese drei nichts mit dem X/y-Worker zu tun
// haben: sie brauchen nur die Replikationen und die Jackknife-Werte.
// Damit sind sie auch von Routinen aus nutzbar, die einen eigenen
// Worker haben (mad_two_sample_boot.cpp), und sie sind einzeln
// testbar.
//
// Alle drei laufen auf dem Hauptthread (boot_bca_ci wird nach
// parallelFor gerufen), R::qnorm/pnorm und Rcpp::warning sind dort
// erlaubt.
// ============================================================

#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <utility>
#include <algorithm>


// ------------------------------------------------------------
// Bias-Korrektur z0
// ------------------------------------------------------------
inline double bca_z0(const Rcpp::NumericVector& s, double est) {

  const int B = s.size();

  if (B == 0)
    return NA_REAL;

  // Bindungen zaehlen halb. Bei einer diskreten Statistik - kleine
  // Stichprobe, ganzzahlige Daten, ein Mass mit wenigen erreichbaren
  // Werten - landet ein guter Teil der Replikationen exakt auf dem
  // Schaetzer. Nur das strikte "<" zu zaehlen schiebt z0 dann ohne
  // Grund nach unten.
  double lt = 0.0, eq = 0.0;

  for (int i = 0; i < B; i++) {
    if (s[i] < est)        lt += 1.0;
    else if (s[i] == est)  eq += 1.0;
  }

  double prop = (lt + 0.5 * eq) / B;

  // Eine halbe Replikation von beiden Enden weg. Die vorherige Klemmung
  // auf 1e-10 liess |z0| bis 6.4 zu - das ist kein Rueckfall, sondern
  // eine wilde Anpassung aus einer Verteilung, die nichts mehr hergibt.
  const double lo = 0.5 / B;
  prop = std::max(lo, std::min(1.0 - lo, prop));

  return R::qnorm(prop, 0.0, 1.0, 1, 0);
}


// ------------------------------------------------------------
// Jackknife-Beschleunigung
// ------------------------------------------------------------
inline double bca_acceleration(const Rcpp::NumericVector& jack) {

  std::vector<double> jv;
  jv.reserve(jack.size());

  for (R_xlen_t i = 0; i < jack.size(); i++)
    if (!ISNAN(jack[i]))
      jv.push_back(jack[i]);

  if (jv.size() < 2)
    Rcpp::stop("Jackknife failed: too few valid leave-one-out estimates.");

  // Die Abweichungen sind von der Groessenordnung 1/n, die Werte selbst
  // von der Groessenordnung der Statistik - Mittelwert und Wert loeschen
  // sich also fast vollstaendig aus. Summiert man RELATIV zu einem der
  // Werte, bleibt diese Ausloeschung exakt: sind alle Jackknife-Werte
  // gleich, ist jede Abweichung eine harte Null.
  //
  // Ohne die Verschiebung ueberlebt ein einziges ulp Unterschied
  // zwischen dem Mittelwert und den (identischen) Werten. Gemessener
  // Fall: vier identische Werte, den = 9.6e-34, Beschleunigung -0.037 -
  // Rundungsrauschen in der Groessenordnung eines echten Effekts.
  const double origin = jv[0];
  const double m      = static_cast<double>(jv.size());

  double meanU = 0.0;
  for (std::size_t i = 0; i < jv.size(); i++)
    meanU += (jv[i] - origin);
  meanU /= m;

  double num = 0.0, den = 0.0;

  for (std::size_t i = 0; i < jv.size(); i++) {
    const double d = meanU - (jv[i] - origin);
    num += d * d * d;
    den += d * d;
  }

  // Schwelle auf der Jackknife-Streuung, nicht auf den: "den == 0.0"
  // exakt ist eine Bedingung, die Fliesskomma fast nie erfuellt. Die
  // Schwelle ist relativ, weil die Statistik hier - anders als ein
  // Assoziationsmass - unbeschraenkt sein kann.
  //
  // Bewegt sich der Jackknife nicht, ist die Beschleunigung tatsaechlich
  // null und BCa faellt auf das bias-korrigierte Intervall zurueck. Die
  // alte Fassung rechnete an dieser Stelle 0/0.
  const double scale = std::max(1.0, std::fabs(origin));

  if (!(std::sqrt(den / m) > 1e-12 * scale))
    return 0.0;

  return num / (6.0 * std::pow(den, 1.5));
}


// ------------------------------------------------------------
// Angepasste Quantilniveaus
// ------------------------------------------------------------
inline std::pair<double, double> bca_quantile_levels(double z0,
                                                     double acc,
                                                     double alpha) {

  const double perc_l = alpha / 2.0;
  const double perc_u = 1.0 - alpha / 2.0;

  const double zal = R::qnorm(perc_l, 0.0, 1.0, 1, 0);
  const double zau = R::qnorm(perc_u, 0.0, 1.0, 1, 0);

  const double dl = 1.0 - acc * (z0 + zal);
  const double du = 1.0 - acc * (z0 + zau);

  // BCa ist nur definiert, wo z0 und acc endlich sind UND beide Nenner
  // positiv bleiben. Bei nicht-positivem Nenner kippt das Vorzeichen des
  // Bruchs, und das obere Niveau kann UNTER dem unteren landen - die
  // Funktion lieferte dann ein verdrehtes "Intervall", und
  // quantile_type7() meldete es klaglos weiter.
  if (!R_finite(z0) || !R_finite(acc) || dl <= 0.0 || du <= 0.0) {
    Rcpp::warning("BCa adjustment is not defined for this sample; "
                  "reporting percentile bounds instead.");
    return std::make_pair(perc_l, perc_u);
  }

  const double adjl = R::pnorm(z0 + (z0 + zal) / dl, 0.0, 1.0, 1, 0);
  const double adju = R::pnorm(z0 + (z0 + zau) / du, 0.0, 1.0, 1, 0);

  // Guertel und Hosentraeger: die Abbildung ist bei positivem Nenner
  // monoton, aber wenn doch etwas durchrutscht, soll es hier auffallen
  // und nicht als vertauschte Grenzen beim Anwender.
  if (!R_finite(adjl) || !R_finite(adju) || adjl > adju) {
    Rcpp::warning("BCa adjustment produced reversed levels; "
                  "reporting percentile bounds instead.");
    return std::make_pair(perc_l, perc_u);
  }

  return std::make_pair(adjl, adju);
}

#endif  // DESCTOOLSX_BCA_HELPERS_H
