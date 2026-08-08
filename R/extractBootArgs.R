
## ============================================================
## Argument handling utilities (centralized, reusable)
## ============================================================
##
## ACHTUNG: Diese Datei liegt in ZWEI Paketen (lumen und DescToolsX) und
## muss dort BYTEGLEICH sein - genau wie src/boot_framework.h und
## src/bca_helpers.h. Vor jeder Aenderung und nach jedem Einbau:
##   tools::md5sum(c("../lumen/R/extractBootArgs.R",
##                   "../DescToolsX/R/extractBootArgs.R"))
## ============================================================



#' @noRd
.checkConfLevel <- function(conf.level) {
  if (length(conf.level) != 1L ||
      !(is.numeric(conf.level) || is.logical(conf.level)) ||
      is.nan(conf.level) ||
      (!is.na(conf.level) && (conf.level <= 0 || conf.level >= 1)))
    stop("'conf.level' must be a single number in (0, 1), or NA",
         call. = FALSE)
  invisible(conf.level)
}

#' @noRd
.checkFlag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x))
    stop(gettextf("'%s' must be a single non-missing logical value", name),
         call. = FALSE, domain = NA)
  invisible(x)
}


# The five names below are exactly the ones a caller can map onto a
# component of the boot.ci() result. A caller whose interval comes from
# compiled code rather than from boot() passes a NARROWER set via
# 'types' - see contCoef(), which can only deliver perc and bca.
#' @noRd
.bootCITypes <- c("norm", "basic", "stud", "perc", "bca")


# Everything a caller may pull out of \dots for the bootstrap. Callers
# need this to tell the bootstrap arguments apart from the ones that
# belong to the data or the statistic.
#
# Do NOT compute the complement with setdiff(names(dots), .bootArgNames):
# that drops every UNNAMED element and turns a mixture of named and
# unnamed \dots into an NA entry. Subset with a logical instead:
#
#   nms <- names(dots); if (is.null(nms)) nms <- rep("", length(dots))
#   rest <- dots[!nms %in% .bootArgNames]
#' @noRd
.bootArgNames <- c("type", "R", "parallel", "ncpus")


#' @noRd
.extractBootArgs <- function(dots,
                             types    = .bootCITypes,
                             default  = "bca",
                             parallel = TRUE) {

  # internal contract, not user input: a caller that offers a default it
  # does not accept is a bug in the caller
  if (length(default) != 1L || !default %in% types)
    stop("internal: 'default' must be one of the offered 'types'")

  defaults <- list(type = default, R = 999)

  if (parallel) {

    defaults <- c(defaults,
                  list(parallel = "no",
                       ncpus    = getOption("boot.ncpus", 1L)))

  } else {

    # A caller whose intervals come from compiled code does not go
    # through boot(), so these two would be accepted and then do
    # nothing - the single most common defect this review has turned up.
    # Saying so is better than a silent no-op.
    given <- intersect(names(dots), c("parallel", "ncpus"))

    if (length(given))
      stop(gettextf(
        "%s cannot be used here: this interval is computed by a compiled bootstrap, not by boot()",
        paste(sQuote(given, FALSE), collapse = " and ")), domain = NA)
  }

  out <- bedrock::extractArgs(
    dots,
    defaults = defaults,
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
      # Restricting to a single one of the names the CALLER can deliver
      # turns both into a clear message at the point where the argument
      # was given.
      if (!is.character(x$type) || length(x$type) != 1L ||
          !x$type %in% types)
        stop(gettextf("'type' must be one of %s",
                      paste(dQuote(types, FALSE), collapse = ", ")),
             domain = NA)

      # whole number, not merely numeric: R = 999.5 used to pass and was
      # then handed to boot(). The upper bound is not cosmetic either -
      # the compiled bootstraps take R as a C int, and anything above
      # .Machine$integer.max arrives there as NA.
      if (!is.numeric(x$R) || length(x$R) != 1L || !is.finite(x$R) ||
          x$R <= 0 || x$R %% 1 != 0 || x$R > .Machine$integer.max)
        stop("'R' must be a single positive whole number")

      # BCa estimates the acceleration by jackknife and reads percentiles
      # of the replicates; with too few replicates boot.ci() fails with
      # "estimated adjustment 'a' is NA", which points nowhere useful.
      #
      # The floor is on R + 1, not on R: R = 199 is the usual choice
      # because R + 1 = 200 makes the order statistics come out even, and
      # a limit on R alone would penalise exactly that value.
      #
      # Only the hard stop lives in the validator. The soft warning is
      # raised after extractArgs() returns - see below.
      if (x$type == "bca" && x$R + 1 < 50)
        stop(gettextf("'type' = \"bca\" needs at least 49 replicates, not %d",
                      x$R), domain = NA)

      # only present when parallel = TRUE; is.null() first, or the check
      # fires on a caller that never offered the argument
      if (!is.null(x$ncpus)) {
        if (!is.numeric(x$ncpus) || length(x$ncpus) != 1L ||
            !is.finite(x$ncpus) || x$ncpus < 1 || x$ncpus %% 1 != 0)
          stop("'ncpus' must be a single whole number >= 1")
      }

      # length 1 checked before %in%: a longer vector made the `if` below
      # an error about the condition rather than about 'parallel'
      if (!is.null(x$parallel)) {
        if (!is.character(x$parallel) || length(x$parallel) != 1L ||
            !x$parallel %in% c("no", "multicore", "snow"))
          stop("'parallel' must be 'no', 'multicore', or 'snow'")
      }
    }
  )

  # Deliberately here and not in the validator. A stop() from the
  # callback always arrives, because it aborts the evaluation; a
  # warning() travels upwards as a condition and can be caught on the
  # way - which is what happened: type = "bca" with R = 100 produced no
  # warning at all, while the same validator did stop at R = 20. A
  # warning belongs to the function the user called, not to a callback
  # whose host may or may not pass it on.
  if (out$type == "bca" && out$R + 1 < 200)
    warning(gettextf(
      "'type' = \"bca\" with only %d replicates gives unstable tails; 199 or more is usual",
      out$R), call. = FALSE)

  out
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
                 bca   = "bca",
                 stop(gettextf("no boot.ci() component is mapped for type %s",
                               dQuote(type, FALSE)), domain = NA))

  m <- ci[[comp]]

  # boot.ci() drops a component it could not compute - 'stud' without
  # variance estimates is the usual case - and NULL[4:5] is NULL, which
  # used to travel on as a pair of missing bounds
  if (is.null(m))
    stop(gettextf("boot.ci() returned no %s interval", dQuote(type, FALSE)),
         domain = NA)

  # the normal interval carries level, lower, upper; the others add the
  # two order statistics in front
  if (type == "norm") unname(m[2:3]) else unname(m[4:5])
}
