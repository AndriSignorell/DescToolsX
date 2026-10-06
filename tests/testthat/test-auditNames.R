# Naming audit, design rules section 3. The rules are checked by
# bedrock::auditNames(); what is listed here is accepted, each entry with
# its reason. An entry starting with "open:" is a name still to be
# decided - it documents a debt and is removed when the name changes.

.auditNamesExceptions <- c(
  "desc"                                       = "the main function of the package keeps its name next to dplyr::desc() (section 3.2)",
  "countWorkDays(nonWorkDays = \"Sat\")"       = "weekday abbreviations are data, not options",
  "countWorkDays(nonWorkDays = \"Sun\")"       = "weekday abbreviations are data, not options",
  "ordAssocs(which = \"tauA\")"                = "the value names an element of the result (section 3.1.1)",
  "ordAssocs(which = \"tauB\")"                = "the value names an element of the result (section 3.1.1)",
  "ordAssocs(which = \"tauC\")"                = "the value names an element of the result (section 3.1.1)"
)


test_that("exported names and arguments follow the naming rules", {

  res <- bedrock::auditNames("DescToolsX",
                             exceptions = names(.auditNamesExceptions))

  expect_identical(
    nrow(res), 0L,
    info = paste(sprintf("%s [%s] %s", res$key, res$rule, res$detail),
                 collapse = "\n"))
})


test_that("every accepted exception still matches a finding", {

  res <- bedrock::auditNames("DescToolsX",
                             exceptions = names(.auditNamesExceptions))

  expect_identical(attr(res, "unused"), character(0))
})
