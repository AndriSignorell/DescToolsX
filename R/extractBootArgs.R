
## ============================================================
## Argument handling utilities (centralized, reusable)
## ============================================================

# The five names below are exactly the ones every caller maps onto a
# component of the boot.ci() result, so they have to be validated HERE -
# see the note on "all" in the validator.
#' @noRd
.bootCITypes <- c("norm", "basic", "stud", "perc", "bca")


#' @noRd
.extractBootArgs <- function(dots) {

  bedrock::extractArgs(
    dots,
    defaults = list(
      type     = "bca",
      R        = 999,
      parallel = "no",
      ncpus    = getOption("boot.ncpus", 1L)
    ),
    validate = function(x) {

      # 'type' was the one default that carried no check at all, although
      # it is the argument every caller branches on. Two ways that hurt:
      #
      #   type = "all"  is legal for boot.ci() and returns ALL interval
      #     components. The callers test `type == "norm"` and otherwise
      #     read elements 4:5 of the first component - which for the
      #     normal interval has only three columns. The bounds come back
      #     NA, silently.
      #
      #   type = c("perc", "bca")  is also legal for boot.ci(), and then
      #     `args$type == "norm"` is a length-2 logical, so the caller's
      #     if() aborts with "the condition has length > 1".
      #
      # Restricting to a single one of the five mapped names turns both
      # into a clear message at the point where the argument was given.
      if (!is.character(x$type) || length(x$type) != 1L ||
          !x$type %in% .bootCITypes)
        stop(gettextf("'type' must be one of %s",
                      paste(dQuote(.bootCITypes, FALSE), collapse = ", ")),
             domain = NA)

      # whole number, not merely numeric: R = 999.5 used to pass and was
      # then handed to boot()
      if (!is.numeric(x$R) || length(x$R) != 1L || !is.finite(x$R) ||
          x$R <= 0 || x$R %% 1 != 0)
        stop("'R' must be a single positive whole number")

      # BCa estimates the acceleration by jackknife and reads percentiles
      # of the replicates; with few replicates boot.ci() fails with
      # "estimated adjustment 'a' is NA", which points nowhere useful
      if (x$type == "bca" && x$R < 200)
        stop("'type' = \"bca\" needs at least 200 replicates; ",
             "raise 'R' or use type = \"perc\"")

      if (!is.numeric(x$ncpus) || length(x$ncpus) != 1L ||
          !is.finite(x$ncpus) || x$ncpus < 1 || x$ncpus %% 1 != 0)
        stop("'ncpus' must be a single whole number >= 1")

      # length 1 checked before %in%: a longer vector made the `if` below
      # an error about the condition rather than about 'parallel'
      if (!is.character(x$parallel) || length(x$parallel) != 1L ||
          !x$parallel %in% c("no", "multicore", "snow"))
        stop("'parallel' must be 'no', 'multicore', or 'snow'")
    }
  )
}


# The component of a boot.ci() result that belongs to 'type', and the two
# columns holding its bounds. Every caller had its own copy of this
# mapping - some by name, some as the positional ci[[4]] - which is how
# the "all" gap above went unnoticed.
#' @noRd
.bootCIBounds <- function(ci, type) {

  comp <- switch(type,
                 norm  = "normal",
                 basic = "basic",
                 stud  = "student",
                 perc  = "percent",
                 bca   = "bca")

  m <- ci[[comp]]

  # the normal interval carries level, lower, upper; the others add the
  # two order statistics in front
  if (type == "norm") unname(m[2:3]) else unname(m[4:5])
}
