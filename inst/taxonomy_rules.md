
# Design Rules: Families and Concepts

## Purpose

The documentation system has two distinct objectives:
  
  - **Navigation** between closely related functions (`@family` → *See Also*)
- **Thematic classification** of functions (`@concept` → package index)

These two objectives should be kept separate. Families and concepts serve different purposes and should therefore be assigned independently.

---
  
  # Concepts
  
  A **concept** describes **what a function is about**.

Concepts provide thematic classification and improve discoverability through the package index.

Examples include:
  
  - `data-manipulation`
- `categorical-data`
- `normality`
- `regression`
- `visualization`
- `transformation`

### Rules

- Every function should receive **at least one concept**.
- Most functions should have **one or two concepts**.
- Three concepts should be exceptional.
- Avoid assigning more than three concepts.
- Concepts may be relatively broad.

A concept answers the question:
  
  > **What topic does this function belong to?**
  
  ---
  
  # Families
  
  A **family** groups functions that naturally belong together from the user's perspective.

Unlike concepts, families automatically generate **See Also** links. Consequently, they should remain small and highly focused.

A family answers the question:

> **After reading this help page, which functions would the user most likely want to explore next?**

### Rules

- Use a family only if it provides genuine navigational value.
- Do **not** create a family for a single function.
- A family should typically contain **2–6 functions**.
- Families larger than roughly **10 functions** should usually be split.
- Avoid large umbrella families.

Examples of overly broad families include:

- `data-manipulation`
- `plotting`
- `statistical-tests`
- `probability-distributions`

---

# Group by User Workflow

Families should follow the user's workflow rather than the implementation.

The guiding question is:
  
  > **If I arrived at this function, what other functions would I naturally want to look at next?**
  
  For example, someone using `winsorize()` is likely interested in

- other variable transformations,
- scaling,
- normalization,
- robust preprocessing,

rather than unrelated mathematical utilities.

---
  
  # Statistical Tests
  
  Group tests according to the statistical question they address rather than by implementation.

Typical families include:
  
  - Normality Tests
- Variance Tests
- Location Tests
- Trend Tests
- Homogeneity Tests
- Post-hoc Tests
- Stationarity Tests
- Autocorrelation Tests
- Agreement Tests
- ROC Analysis

These groupings are intuitive for users and produce meaningful *See Also* sections.

---
  
  # Plotting Functions
  
  Group plotting functions according to the analytical situation rather than graphical style.

Typical examples include:
  
  - One-variable plots
- Two-variable plots
- Distribution plots
- Specialized plots
- Plot utilities

Users usually think in terms of the data they have, not the plotting primitive used internally.

---
  
  # Transformations
  
  Distinguish between

- functions that **return transformed data**, and
- functions that **compute summary statistics**.

For example,

Vector transformations:
  
  - `winsorize()`
- `scaleX()`
- `boxCox()`
- `yeoJohnson()`

Summary statistics:
  
  - `madX()`
- `meanAD()`
- `medianX()`

Although both may be considered "robust methods", they serve different purposes and therefore belong in different families.

---
  
  # Large Functional Areas
  
  Very large collections of functions should generally **not** be represented as families.

Examples include

- Probability distributions
- Statistical tests
- Plot galleries

Instead, these should be organized using

- concepts,
- vignettes,
- overview articles,
- package indices.

Large families generate unhelpful *See Also* sections.

---
  
  # Semantic Consistency
  
  Functions within a family should

- solve similar problems,
- operate on similar data structures,
- produce similar outputs, and
- be plausible alternatives from the user's perspective.

A family should feel like a small, coherent API.

---

# Decision Process

For every function, apply the following questions in order:

1. **What is this function about?**
   - Assign one or more **concepts**.

2. **Which functions would a user most likely explore next?**
   - Assign a **family**, if appropriate.

3. **Would the family contain fewer than two functions?**
   - If yes, omit the family.

4. **Would the complete family produce a useful *See Also* section?**
   - If not, split the family into smaller, more coherent groups.

---

# Summary

The guiding philosophy is:

- **Concepts classify functions.**
- **Families connect functions.**

Concepts improve discoverability through the package index.

Families improve navigation through concise and meaningful *See Also* sections.

Keeping these responsibilities separate leads to a documentation system that is both easier to maintain and more useful for users.

