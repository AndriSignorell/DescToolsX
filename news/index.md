# Changelog

## DescToolsX 0.0.0.945

### New features

- Descriptive layer of the DescToolsX package suite and redesigned
  successor to DescTools: frequency and contingency tables, measures of
  location, dispersion, shape and concentration, association and
  agreement coefficients, effect sizes, and classifier metrics.
- [`desc()`](../reference/Desc.md) is a single generic dispatching on
  the type of the input, so vectors, factors, dates, variable pairs,
  tables and whole data frames are described through one entry point.
- Common interfaces [`Association()`](../reference/Association.md) and
  [`Agreement()`](../reference/Agreement.md) collect the respective
  families of coefficients behind one call.
- Performance-critical routines are implemented in C++ via Rcpp,
  RcppArmadillo and RcppParallel.

### Changes from DescTools

- Consistent lowerCamelCase naming throughout; function names, argument
  names and argument order follow the design rules of the suite.
- Base utilities, graphics and inferential procedures have moved into
  the separate packages bedrock, pharos and lumen; DescToolsX now
  depends on them instead of carrying its own copies.

### Acknowledgements

Parts of the code and documentation were reviewed with the help of large
language models (OpenAI Codex, Anthropic Claude). Every suggestion was
assessed, edited and verified by the maintainer, who remains solely
responsible for the content of this package.
