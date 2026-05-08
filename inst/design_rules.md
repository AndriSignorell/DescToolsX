# DescToolsX — Design Rules & Architecture

Version: 0.4  
Maintainer: Andri Signorell

---

# 1. Projektphilosophie

DescToolsX ist der konzeptionelle Nachfolger von DescTools mit folgenden Leitprinzipien:

- API-konsistent und vorhersagbar
- Methodisch transparent
- Defensive-programming-orientiert
- CRAN-sauber
- Klar strukturiert (Engine / Interface / Resolver getrennt)

Übergeordnetes Stilziel: DescToolsX soll wirken wie `survival`, `boot`, `stats` — ein statistisches Framework, kein historisch gewachsener Werkzeugkasten.

**Grundsatz:**  
Konsistenz > Perfektion  
Base-R-Kompatibilität > stylistische Reinheit

---

# 2. Architektur

## 2.1 Trennung der Ebenen

Jede Methodenfamilie folgt diesem Schema:

```
Interface → Resolver → Engine → Recycle Layer
```

Beispiel für `binomCI`:

```
binomCI()
 ├── .resolveMethod()
 ├── .recycleApply()
 └── .binomCI_engine()
      ├── .binomCI.wilson()
      ├── .binomCI.jeffreys()
      └── ...
```

## 2.2 Recycling-Framework

- Vektorisierung erfolgt ausschliesslich über `.recycleApply()`
- Kein implizites Recycling in Engines
- Engines arbeiten skalare Fälle
- Das Interface garantiert konsistente Längen

### Vectorization Contract

- Interface garantiert Vektorisierung
- Engines dürfen niemals Vektoren erwarten
- Recycling erfolgt zentral

## 2.3 Keine Metaprogramming-Hacks

Verboten:

- `eval(parse())`
- unnötige String-Evaluation
- dynamisches Zusammenbauen von Funktionen

Erlaubt:

- saubere Funktions-Dispatch-Tabellen
- explizite `switch`- / Lookup-Tabellen
- klar dokumentierte Resolver

## 2.4 Imports vs. Depends

- `Depends` nur für die R-Version
- Packages via `Imports`
- Keine unnötige Namespace-Verschmutzung

---

# 3. Naming Rules

## 3.1 Funktionen und API

### 3.1.1 Allgemein

| Ebene | Stil | Beispiel |
|---|---|---|
| Exportierte Funktionen | lowerCamelCase | `fitModel()` |
| Interne Funktionen | `.lowerCamelCase` | `.computeWeights()` |
| Helper-Funktionen | `.prefix` | `.checkInput()` |
| Engines | `.familyEngine` | `.binomCI_engine()` |
| Klassen | UpperCamelCase | `LinearModel` |
| User-visible Strings | kebab-case | `"log-scale"` |

S3-Methoden erhalten niemals ein X-Suffix und bleiben base-konform:

```r
print.PercTable
plot.Desc.numeric
lines.Lc
```


### 3.1.2 Naming Across R and C++ (Rcpp Integration)

🎯 Ziel
Klare, konsistente und sofort erkennbare Trennung zwischen:
- R-Code (User/API-orientiert)
- C++-Code (algorithmische Implementierung / Performance Layer)

---

R (User-facing & intern)

Ebene                  Stil
---------------------  ------------------------
API                    lowerCamelCase
intern                 .lowerCamelCase
Dispatch               .familyDispatch
Engine (R, optional)   .familyEngine
Helper                 .prefix

---

C++ (Rcpp)

Ebene                  Stil
---------------------  ------------------------
C++ Funktionen         snake_case
Dateien                snake_case.cpp

---

R Wrapper fungieren als Bridge und sind auch snake_case
```
between_num <- function(...) {
  .Call(between_num_engine, ...)
}
```

## 3.7 C++ Funktionen

C++ Funktionen verwenden snake_case mit `_cpp`-Suffix und werden via
`// [[Rcpp::export]]` registriert. Sie sind nicht im NAMESPACE exportiert
und damit für User nicht via `::` zugänglich. Der `_cpp`-Suffix
kennzeichnet sie konventionell als interne Implementierungsdetails.

Beispiele:

  kurt_cpp
  kurt_weighted_cpp
  conDisPairsTab_cpp


## 3.2 Funktionsklassifikation und X-Suffix

Das X-Suffix gilt ausschliesslich für **statistische Kennzahlen**, deren Name mit einer Funktion in `base`, `stats`, `graphics` oder `utils` kollidiert.

**Statistische Kennzahlen (kollisionsgefährdet) → lowercase + X:**

```
meanX       medianX     sdX         varX
madX        iqrwX       rangeX      coefvarX
gmeanX      hmeanX      skewX       kurtX
maeX        mseX        rmseX       mapeX
smapeX      quantileX   percentRankX
```

Regeln:
- Keine Ausnahmen
- Keine Grossschreibung: `madX`, nicht `MADX`


**Regel: Wann genau wird X verwendet?**

X wird verwendet, wenn:

1. Eine Funktion konzeptionell eine statistische Kennzahl ist
2. UND der Name mit einer bestehenden Base-R Funktion kollidiert

Beispiele:

mean → meanX
var  → varX
rank → rankX

Nicht verwenden bei:
- Tests
- CI-Funktionen
- Transformationen ohne Kollision


**Konfidenzintervalle → `basisnameCI` (kein X sofern keine Kollision):**

```
meanCI      medianCI    varCI       sdCI
quantileCI  binomCI     poissonCI   rateCI
```

**Benannte Tests und Verfahren → lowerCamelCase, kein X:**

Tests kollidieren in der Regel nicht mit base und repräsentieren benannte statistische Verfahren. Das X-Suffix entfällt daher.

```r
# korrekt
jarqueBeraTest()
bartelsRankTest()
shapiroFranciaTest()

# falsch
ShapiroFranciaTest()
AndersonDarlingTest()
```

**Transformationen und Utilities → lowercase, kein X** (ausser bei Kollision mit base):

```
winsorize    roundTo    cutAge    cutQ
sortX        rankX      sampleX   # mit X wegen base-Kollision
```

**Plot-Funktionen → lowerCamelCase mit Plot-Prefix, kein X:**

```
plotQQ       plotECDF     plotCorr
plotPairs    plotViolin   plotBar
```

**Sonderregel: Etablierte Abkürzungen**

Bekannte statistische Abkürzungen dürfen Grossbuchstaben behalten:

- CI (confidence interval)
- QQ (quantile-quantile)
- ECDF

Beispiele:

- meanCI
- plotQQ
- plotECDF

## 3.3 Argument-Namen

**(A) Base-R-Argumente unverändert übernehmen** — diese behalten bewusst den Punkt:

```
na.rm       conf.level      xlab    ylab    xlim    ylim
```

**(B) Neue Argumente → lowerCamelCase:**

```
groupSize   numBootstrap    showLegend    maxIter
```

**(C) Keine neuen Punkt-Namen einführen** — der Punkt ist ausschliesslich für Legacy/Base-R reserviert:

```r
# falsch
group.size
num.bootstrap
```

**(D) Konsistente Begriffe im ganzen Package** — ein Konzept, ein Name:

```
data      für data.frames / matrices
x         für Vektorinput
y         zweiter Vektor
groups    (nicht: grp, grouping)
weights   (nicht: w, weightVec)
data      (nicht: df, dataset gemischt)
```

siehe auch core vocabulary!


**(E) Boolean Argumente**

Boolean Argumente beginnen mit:

- is*
- has*
- show*
- use*

Beispiele:

- showLegend
- useWeights
- isSorted

Nicht erlaubt:

- legend = TRUE  (ambiguous)
- weights = TRUE (semantic overload)


**(F) Steuerungsbegriffe**

Verwende keine numerischen Begriffe wie `tol`, `eps`, `precision`,
wenn das Argument eigentlich ein User-facing Verhalten steuert
(z. B. Formatierung, Anzeigegrenzen).

→ Bevorzuge semantische Namen wie `threshold`, `cutoff`, `limit`.

## 3.4 Rückgabewerte

Output ist Teil der API und muss strikt konsistent sein:

- immer lowerCamelCase
- keine Punkte
- sprechende Namen

```r
# korrekt
list(
  pValue        = 0.03,
  testStatistic = 2.1,
  confInt       = c(0.1, 0.5),
  nObs          = 120
)

# falsch
list(p.value = 0.03, test.statistic = 2.1)
```

**Standardisierte Statistik-Namen**

Wo möglich, werden folgende Namen verwendet:

- pValue
- testStatistic
- df (bleibt base-R)
- estimate


## 3.5 Method-Strings (User Interface)

Externe Optionen:

- lowercase
- Bindestrich-getrennt (kebab-case)
- keine Leerzeichen
- keine kryptischen Abkürzungen
- literaturnahe Bezeichnungen

```
"wald"           "wald-cc"        "wilson"
"wilson-cc"      "wilson-mod"     "jeffreys"
"clopper-pearson" "agresti-coull" "mid-p"
```

Intern:

- Unterstrich `_`
- Private Funktionen mit `.`-Prefix

```
.binomCI.wilson_mod
.binomCI.clopper_pearson
```

## 3.6 Namespace-Nutzung für Base-R-Funktionen

Funktionen aus den standard-attached Packages (`base`, `stats`, `graphics`, `grDevices`, `utils`, `methods`) werden **ohne** explizite Namespace-Qualifizierung aufgerufen:

```r
# korrekt
plot(...)
lines(...)
density(...)

# falsch
graphics::plot(...)
stats::density(...)
```

Explizite Qualifizierung (`pkg::fun`) nur bei Funktionen aus Nicht-Base-Packages oder bei Namenskonflikten.

---

# 4. Argumentreihenfolge in Funktionen

## 4.1 Statistische Funktionen

Exportierte statistische Funktionen halten folgende Reihenfolge ein:

1. **Daten** — `x`, `y`, `n`, ggf. Matrix- oder Formeleingaben
2. **Estimator-Definition** — `estimator`, `model`, `type`, `unit`, `weights`
3. **Inferenz-/CI-Steuerung** — `conf.level`, `sides`, `method`
4. **Datenbehandlung** — `na.rm`, `subset`
5. **`...`**

Grundsatz: Die CI-Konstruktion hängt vom Estimator ab, niemals umgekehrt.

Beispiel `Skew`:

```r
skewX(
  x,
  # Estimator
  estimator = 3,
  weights   = NULL,
  # Inference
  conf.level = NA,
  sides      = c("two.sided", "left", "right"),
  method     = c("classic", "boot"),
  # Data handling
  na.rm = FALSE,
  ...
)
```

**`conf.level`-Default:**

- Funktionen, die das CI zusammen mit dem Ergebnis zurückgeben (z.B. `skewX`, `ICC`): `conf.level = NA` (kein CI per default)
- Dedizierte CI-Funktionen (z.B. `meanCI`, `binomCI`): `conf.level = 0.95`

## 4.2 Plot-Funktionen

Plot-Funktionen halten folgende Reihenfolge ein:

```
DATA
LABELS
AXES
STRUCTURE
STYLE
FEATURES
FRAMEWORK
...
```

| Gruppe | Typische Argumente |
|---|---|
| DATA | `x`, ggf. `y` |
| LABELS | `main`, `xlab`, `ylab` |
| AXES | `xlim`, `ylim` |
| STRUCTURE | `cluster`, `order`, `groups`, `gap`, `items` |
| STYLE | `col`, `lwd`, `pch`, `bg`, `grid`, `box` |
| FEATURES | `legend`, `text`, `connlines`, `labels` |
| FRAMEWORK | `stamp` |

STRUCTURE-Argumente dürfen niemals grafische Style-Parameter enthalten. Colors gehören immer zu STYLE.

---

# 5. Programmierung

## 5.1 Default-Handling

- Alle zulässigen Optionen stehen im `formals()`-Default
- Erster Eintrag = Default
- Keine hartcodierten Default-Strings im Body
- Keine doppelte Definitionsquelle

## 5.2 Method-Resolver

DescToolsX verwendet `.resolveMethod()` anstelle von `match.arg()`, wo eine erweiterte Auflösung nötig ist (Alias-Mapping, Mehrfachauswahl, Hidden Options). `match.arg()` bleibt zulässig für einfache Fälle.

.resolveMethod() muss:

- Partial Matching unterstützen
- Alias Mapping unterstützen
- deterministisch sein (kein guessing bei Mehrdeutigkeit)

Muster mit `.resolveMethod()`:

```r
if (missing(method)) {
  method <- formals(sys.function())$method[[1]]
} else {
  method <- .resolveMethod(method, several.ok = TRUE)
}
```

## 5.3 Defensive Programming

Exportierte Funktionen müssen:

- Typprüfung durchführen
- Längenprüfung durchführen
- Skalare Rückgaben validieren
- Keine stillen NA-Kaskaden erzeugen
- Boundary-Handling explizit dokumentieren

## 5.4 Error Messages

- Klar und konkret
- Kein Humor
- Keine internen Begriffe
- Immer mit Argumentnamen

Beispiel:

"Argument 'x' must be numeric and non-empty."

## 5.5 Backward Compatibility

- Alte Method-Namen werden als Alias akzeptiert
- Intern sofort auf kanonische Namen gemappt
- Keine Breaking Changes ohne Alias-Schicht

## 5.6 Confidence Interval Output Convention

Alle CI-Funktionen verwenden folgende Spaltennamen:

- `estimate`
- `lci` (lower confidence interval bound)
- `uci` (upper confidence interval bound)

Diese Konvention ist verbindlich und ändert sich nicht ohne Major-Version-Bump.

---

# 6. Umgang mit `...`

## 6.1 Grafische Parameter in Plot-Funktionen

Grafische Parameter werden via `...` übergeben und durch `.applyParFromDots()` angewendet. Plot-Funktionen verarbeiten grafische Parameter aus `...` nicht manuell.

Welche Argumente explizit benannt werden und welche über `...` laufen, ist **kontextabhängig**: Die 2–3 für die jeweilige Funktion wichtigsten visuellen Parameter werden explizit benannt (damit der User den Quicktip sieht); weniger zentrale Parameter (`cex`, `cex.axis`, `las`, `mar`, `oma`) laufen über `...`.

Beispiel `plotDot`: `col` und `pch` sind explizit; `cex` und `las` gehen über `...`.

**Explizite vs implizite Grafikparameter

- Schlüsselparameter der jeweiligen Funktion dürfen explizit sein
- generische Base-Parameter laufen immer über `...`

Beispiel:

plotDot():
- explizit: col, pch
- über ...: cex, lwd, las

## 6.2 Bootstrap-Argumente

Bootstrap-Argumente (`method`, `R`, `parallel`, etc.) gehen immer über `...` und werden intern über `.extractBootArgs()` extrahiert:

```r
dots      <- list(...)
boot_args <- .extractBootArgs(dots)

boot::boot(..., R = boot_args$R, parallel = boot_args$parallel)
```

Verboten:

- Direkte Verwendung von `inDots()`
- Direkter Zugriff auf `...` im Funktionskörper
- Argument-Parsing innerhalb von `apply` / `replicate`

## 6.3 Flexible Argument-Pattern für grafische Elemente

Argumente wie `xax`, `yax`, `grid`, `legend` folgen einem einheitlichen flexiblen Pattern:

| Wert | Bedeutung |
|---|---|
| `TRUE` | Element zeichnen mit Package-Defaults |
| `FALSE` | Element unterdrücken |
| `NULL` / `NA` | Package-Option verwenden |
| `list(...)` | Element mit custom Parametern zeichnen |

Implementierung via `.callIf()`:

```r
.callIf(graphics::grid, grid, defaults = th$grid)
```

DescToolsX verwendet **keine** Legacy-Base-Graphics-String-Flags wie `xaxt = "n"`. Stattdessen: `yax = FALSE`.

---

# 7. Color-Konventionen

## 7.1 Argument-Name

Immer `col`, nie `cols` — auch für Paletten oder mehrere Farben:

```r
col = "red"
col = c("red", "blue", "green")
col = colorRampPalette(...)(20)
```

## 7.2 Verwandte Color-Argumente

Wenn verschiedene grafische Elemente unterschiedliche Farben benötigen, folgt man base R:

```r
col     # Hauptfarbe
border  # Rahmenfarbe (Polygon/Box)
bg      # Füllfarbe von Symbolen
```

---

# 8. Statistikfunktionen

## 8.1 CI-Funktionen

Dedizierte CI-Funktionen (`meanCI`, `binomCI`, etc.) haben `conf.level = 0.95` als Default. Funktionen, die CI optional mitliefern, haben `conf.level = NA`.

## 8.2 Numerisches Verhalten

- Extreme Fälle explizit behandeln
- Keine impliziten Korrekturen
- Randomisierte Verfahren dokumentieren (z.B. Witting)
- RNG-Abhängigkeit erwähnen

---

# 9. Plot-Funktionen

## 9.1 Graphics State Management

Alle Plot-Funktionen werden in `.withGraphicsState()` gewrappt:

```r
.withGraphicsState({
  .applyParFromDots(...)
  # plotting code
}, stamp = stamp)
```

Kein direkter `par()`-Aufruf ausserhalb von `.applyParFromDots()`.

## 9.2 Helper-Funktionen

Plot-Funktionen nutzen diese internen Helpers:

```
.withGraphicsState()
.applyParFromDots()
.resolveNames()
.normalizeDotData()
.adjustLeftMarginForLabels()
.callIf()
.drawAxis()
```

## 9.3 Theme-System

Plot-Funktionen nutzen `.theme()` für zentralisierte Stil-Defaults:

```r
th <- .theme(
  grid = list(col = "grey", lwd = 1, lty = "dotted")
)
```

Theme-Subsetting: Plot-Funktionen modifizieren das Theme nicht global, sondern selektieren lokal den relevanten Subset:

```r
defaults = th$grid[!startsWith(names(th$grid), "group.")]
```

Theme-Werte dürfen nur STYLE definieren, niemals STRUCTURE.

## 9.4 `stamp`

`stamp` wird via globale Option gesteuert und nur als explizites Argument exponiert, wenn der User den globalen Default überschreiben muss:

```r
.withGraphicsState(expr, stamp = .getOption("stamp", NULL))
```

---

# 10. Verbose-Konzept

> [TODO: Verbose-Konzept einfügen]

---

# 11. Dokumentation

## 11.1 Description vs. Details

| Sektion | Inhalt | Nicht enthalten |
|---|---|---|
| `@description` | Was die Methode ist; statistischer Zweck; konzeptionelle Grundlage | Vergleiche; asymptotisches Verhalten; Empfehlungen; Einschränkungen |
| `@details` | Beziehungen zu anderen Methoden; Asymptotics; Power; Annahmen; Einschränkungen; Vergleiche | Die primäre Definition der Methode |

Description bleibt konzis und in sich geschlossen. Vergleiche zwischen Methoden gehören immer in Details — wenn ein Vergleich kurz genug für Description erscheint, ist das ein Signal, ihn in Details zu verschieben.


## 11.1a. Roxygen Topic Naming (`@name`, `@rdname`)

### Purpose

Roxygen topic names define:
- the **filename of the Rd documentation**
- the **grouping of related functions**
- the **anchor structure for pkgdown and help pages**

They are **not function names** and therefore follow **string-style conventions**, not API naming rules.

---

### Naming Style

All topic names MUST use:

kebab-case (lowercase + hyphen)

#### Example

@name extreme-value-moments  
@rdname extreme-value-moments

---

### Allowed Binding Element

- Hyphen (`-`) → **mandatory**

---

### Forbidden Styles

The following MUST NOT be used:

| Style | Example | Reason |
|------|--------|--------|
| underscore | extreme_value_moments | conflicts with C++ naming |
| dot | extreme.value.moments | legacy base R style |
| camelCase | extremeValueMoments | reserved for functions |
| mixed styles | extreme-valueMoments | inconsistent |

---

### Structural Guidelines

Topic names should follow a **semantic grouping pattern**:

<domain>-<concept>[-<detail>]

#### Examples

- extreme-value-moments
- extreme-value-distribution
- extreme-value-quantiles
- robust-statistics-location
- robust-statistics-scale

---

### Grouping Across Functions

Multiple functions can share the same documentation topic:

@name extreme-value-moments  
@rdname extreme-value-moments  

gumbel <- function(...) {}

@rdname extreme-value-moments  

gev <- function(...) {}

This creates a **single unified help page**.

---

### Design Principles

- Topic names describe **conceptual domains**, not implementations
- Names must be:
  - short
  - descriptive
  - reusable across functions
- Prefer **stable vocabulary** to enable consistent grouping

---

### Summary

| Element | Rule |
|--------|------|
| Case | lowercase |
| Separator | hyphen (`-`) |
| Style | kebab-case |
| Scope | conceptual grouping (not function naming) |




## 11.2 Pflicht-Sektionen

Alle exportierten Funktionen müssen enthalten:

**`@param`** — für jedes Argument: Typ, Bedeutung, Constraints und Default-Verhalten. Präzise genug, um Fehlanwendung zu verhindern.

**`@return`** — Struktur und Typ des Rückgabeobjekts. Bei komplexen Outputs (Listen) werden die wichtigsten Komponenten beschrieben.

**`@examples`** — minimal, reproduzierbar, ohne externe Daten, primärer Use Case. Wo sinnvoll: zweites Beispiel für Non-Default oder Edge-Case.
**`@examples`** sollen deterministisch sein → set.seed() wenn nötig

## 11.3 Error Handling in der Dokumentation

Zu dokumentieren, wo relevant:

- Umgang mit fehlenden Werten (`NA`, `NaN`)
- Verhalten an Grenzwerten
- Verletzung von Annahmen
- ausgelöste Fehlermeldungen oder Warnings

Ziel: Verhalten dokumentieren, das für korrekten und vorhersagbaren Gebrauch wichtig ist — keine vollständige Fehler-Enumeration.

## 11.4 Authorship

Der Package-Maintainer ist der Default-Autor aller Funktionen und wird in Einzelfunktionen nicht explizit genannt.

| Contributor-Typ | Wo erwähnen |
|---|---|
| Package Maintainer | Nur in `DESCRIPTION` |
| Externer Contributor (signifikanter Code) | `@note` der Funktion |
| Externer Contributor (geringer Beitrag) | `DESCRIPTION` oder `NEWS`, nicht per Funktion |

**`@note`-Formulierungen (abgestuft):**

Geringer Beitrag:
```r
#' @note
#' Parts of the code contributed by [Name].
```

Adaptierter Beitrag:
```r
#' @note
#' Based on code by [Name], adapted to conform to package standards.
```

Substantieller Beitrag:
```r
#' @note
#' Substantially based on code by [Name], with major extensions
#' and improvements by the package author.
```

Im Zweifelsfall die grosszügigere Formulierung wählen. `@note` ist eine Höflichkeitsattribution, keine Rechtserklärung. Lizenzkompatibilität externer Code-Bestandteile muss geprüft sein.

## 11.5 Referenzen

Jede Funktion, die eine benannte Methode, einen Test oder einen Schätzer implementiert, zitiert die primäre methodologische Quelle in `@references`.

**Format (APA-basiert):**

```r
#' @references
#' Author, A. B., & Author, C. D. (Year). Title of article.
#'   \emph{Journal Name}, \emph{volume}(issue), pages.
#'   \doi{10.xxxx/xxxxx}
#'
#' Author, A. B. (Year). \emph{Title of Book}. Publisher.
```

**Was zitieren:**

- Paper oder Buch, das die Methode erstmals formal hergeleitet hat
- Zugänglichere Sekundärquelle, falls Primärquelle schwer zugänglich (nach der Primärquelle)
- Software-/Algorithmus-Paper, wenn die Implementierung einem spezifischen Algorithmus folgt

**Was nicht zitieren:**

- Lehrbücher als Primärquellen (ausser die Methode entstand dort)
- R-Packages oder Online-Ressourcen nicht als primäre methodologische Referenz,
  wenn eine theoretische Originalquelle existiert
- Ausnahme:
  Wenn die Implementierung explizit auf einem Package basiert
  (z. B. übernommener Algorithmus), kann dieses zusätzlich referenziert werden

Bei mehreren Referenzen steht die theoretische zuerst. Verglichene Methoden in Details werden alle referenziert.


### Abgrenzung: Implementierung vs. Anwendung

Referenzen werden **nur dann gesetzt**, wenn die Funktion selbst
eine methodologische Innovation oder konkrete Implementierung
einer publizierten Methode darstellt.

Es werden **keine Referenzen gesetzt**, wenn:

- die Funktion lediglich bestehende Base-R-Funktionalität kapselt
- eine bekannte Methode **nur angewendet**, aber nicht implementiert wird
- die mathematische Definition trivial oder allgemein bekannt ist
  (z. B. Mittelwert, Varianz, Dummy-Codierung)
- die Referenz keinen direkten Mehrwert für das Verständnis der Funktion bietet

Beispiele:

- ❌ `dummy()` → keine Referenz (nutzt `contr.*`)
- ❌ Wrapper um `mean()`, `sd()` → keine Referenz
- ✅ eigener Bootstrap-Algorithmus → Referenz
- ✅ implementierter statistischer Test → Referenz



## 11.6 Mathematische Notation

- `\eqn{}` für Inline-Mathe
- `\deqn{}` nur wenn wirklich nötig
- Parameter kursiv
- Literatur sauber referenziert

---

# 12. Family und Concepts

## 12.1 Überblick

Das Package verwendet `@family` und `@concept` zur Funktionsorganisation nach strikter Aufgabentrennung:

- `@family` definiert die primäre Klassifikation (Navigation)
- `@concept` liefert zusätzliche semantische Tags (Suche, Kontext, Cross-linking)

## 12.2 @family

**Genau eine Familie pro Funktion:**

```r
@family topic.<categoryName>
```

Naming Convention: `topic.` Prefix + camelCase Suffix.

Beispiele:

```
topic.hypothesisTests
topic.nonparametricTests
topic.distributions
topic.timeSeriesTests
topic.goodnessOfFit
topic.contingencyTests
```

**Entscheidungsregel:** *Wo würde ein User diese Funktion zuerst suchen?*

## 12.3 @concept

Jede Funktion sollte typischerweise 2–4 `@concept`-Tags haben.

Regeln:
- Concepts sind nicht gegenseitig exklusiv
- Concepts sind deskriptiv, nicht hierarchisch
- Keine Redundanz mit der Family
- lowercase kebab-case: `goodness-of-fit`, `heavy-tailed`

```r
@concept goodness-of-fit
@concept normality
@concept rank-based
@concept extreme value theory
```

## 12.4 Families vs. Concepts

| Feature | `@family` | `@concept` |
|---|---|---|
| Kardinalität | genau eine | mehrere |
| Zweck | Navigation | semantisches Tagging |
| Struktur | hierarchisch (`topic.*`) | flach |
| Stabilität | hoch | flexibel |
| Beispiel | `topic.nonparametricTests` | `rank-based, paired` |

## 12.5 Sonderfall: Distributionen

Alle Distributionsfunktionen (d/p/q/r) werden unter einer einzigen Familie gruppiert:

```r
@family topic.distributions
```

Spezifische Eigenschaften via `@concept`:

```r
@concept continuous distribution
@concept extreme value theory
@concept GEV
@concept dpqr
```

**Rationale:** Distributionen folgen einer uniformen API-Struktur (dpqr) und User suchen nach Distributionsnamen, nicht nach Kategorie.

## 12.6 Hypothesis Tests

Tests sind heterogen und verwenden deshalb mehrere Familien, entsprechend der natürlichen Suchweise der User:

```
topic.goodnessOfFit
topic.nonparametricTests
topic.contingencyTests
topic.timeSeriesTests
```

## 12.7 @seealso

`@seealso` ist reserviert für enge funktionale Verwandtschaft: Funktionen, die direkte Alternativen sind, oder Hilfsfunktionen, die typischerweise zusammen verwendet werden.


# 13 DescToolsX — Core Vocabulary

## 13.1. Datenstruktur

| Bedeutung | Name | Kommentar |
|----------|------|----------|
| Vektor (Hauptinput) | `x` | Standard |
| Zweiter Vektor | `y` | Standard |
| Data Frame / Matrix | `data` | wie lm, ggplot |
| Formel | `formula` | Base-R kompatibel |

## 13.2. Gruppierung & Struktur

| Bedeutung | Name |
|----------|------|
| Gruppenvariable | `groups` |
| Strata | `strata` |
| Cluster | `cluster` |

## 13.3. Gewichte & Subsetting

| Bedeutung | Name |
|----------|------|
| Gewichte | `weights` |
| Subset | `subset` |

## 13.4. Statistik / Inferenz

| Bedeutung | Name |
|----------|------|
| Konfidenzniveau | `conf.level` |
| p-Wert | `pValue` |
| Teststatistik | `testStatistic` |
| Freiheitsgrade | `df` |
| Schätzer | `estimate` |

## 13.5. Methodensteuerung

| Bedeutung | Name |
|----------|------|
| Methode | `method` |
| Engine intern | `engine` |
| Typ | `type` |

## 13.6. Simulation / Bootstrap

| Bedeutung | Name |
|----------|------|
| Iterationen | `R` |

## 13.7. Reihenfolge / Bereich

| Bedeutung | Name |
|----------|------|
| Sortierung | `order` |
| Wertebereich | `range` |
| decreasing | `decreasing` |

## 13.8. Missing Values

| Bedeutung | Name |
|----------|------|
| NA entfernen | `na.rm` |
| NA Position | `na.last` |

## 13.9. Plotting

| Bedeutung | Name |
|----------|------|
| Farbe | `col` |
| Symbol | `pch` |
| Linienbreite | `lwd` |
| Linientyp | `lty` |

## 13.10. Boolean Flags

| Typ | Präfix |
|-----|--------|
| Anzeige | `show*` |
| Verwendung | `use*` |
| Zustand | `is*` |
| Besitz | `has*` |

## 13.11. Intervalle

| Bedeutung | Name |
|----------|------|
| linke Grenze geschlossen | `leftClosed` |
| rechte Grenze geschlossen | `rightClosed` |

## 13.12. Output

| Bedeutung | Name |
|----------|------|
| p-Wert | `pValue` |
| Statistik | `testStatistic` |
| CI unten | `lci` |
| CI oben | `uci` |
| Konfidenzintervall | `confInt` |

## 13.13 Verbotene Namen

df (für data), dat, grp, w, level, alpha, color



