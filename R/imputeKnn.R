
#' K-Nearest Neighbors Imputation
#'
#' Imputes missing values from the `k` nearest complete observations.
#' Numeric and factor variables are both supported.
#'
#' @details
#' Distance is Gower-style: numeric variables contribute their squared
#' differences and factor variables a 0/1 mismatch penalty, summed and
#' square-rooted. Numeric variables are standardized beforehand when
#' `scale = TRUE`, so that no variable dominates through its unit
#' alone. The standardization affects distances only; imputed values are
#' always taken from the original data.
#'
#' Continuous and categorical contributions are combined without further
#' weighting, so a factor mismatch counts as much as a one-standard-deviation
#' numeric gap. Whether that balance suits the data is for the caller to
#' judge.
#'
#' Each incomplete observation is compared with the complete ones on the
#' variables it does observe, so rows missing different variables use
#' different distances. Observations are therefore grouped by their pattern
#' of missingness and each pattern is solved in one pass.
#'
#' With `method = "weighted"` the neighbours are weighted by
#' \eqn{\exp(-d)}. Because that decays on the scale of the distances
#' themselves, it is meaningful only when those are of moderate size, which
#' is another reason to keep `scale = TRUE`. Weights are normalized
#' against their maximum before exponentiation, so that widely separated
#' neighbours cannot all underflow to zero.
#'
#' When \pkg{dbscan} is installed it is used to find the neighbours via a
#' kd-tree, which is markedly faster on large data. The result is the same
#' either way, apart from the resolution of exact ties.
#'
#' @param x a data frame with missing values.
#' @param k positive integer, the number of nearest neighbours to draw on.
#' @param scale logical; if `TRUE` (default), numeric variables are
#'   standardized before distances are computed. This affects the distances
#'   only, not the imputed values.
#' @param method the aggregation applied to the neighbours' values, either
#'   `"weighted"` for a distance-weighted mean (numeric) or weighted
#'   mode (factor), or `"median"` for a median (numeric) or mode
#'   (factor).
#' @param distData optional data frame supplying the pool of potential
#'   neighbours. If given, neighbours are drawn from `distData` only,
#'   while `x` alone is imputed. It must have the same variables as
#'   `x`.
#'
#' @return a data frame of the same shape as `x`, with missing values
#'   replaced.
#'
#' @examples
#' set.seed(123)
#' dat <- data.frame(
#'   x = c(1, 2, 3, 4, 5, 6),
#'   y = c(1, 2, 3, 4, 5, 6),
#'   z = factor(c("a", "b", "a", "b", "a", "b"))
#' )
#'
#' dat[c(1, 3), "x"] <- NA
#' dat[c(2, 5), "y"] <- NA
#'
#' imputeKnn(dat, k = 2)
#'
#' # neighbours drawn from a separate reference set
#' ref <- data.frame(
#'   x = c(1.5, 2.5, 3.5, 4.5),
#'   y = c(1.5, 2.5, 3.5, 4.5),
#'   z = factor(c("a", "b", "a", "b"), levels = c("a", "b"))
#' )
#'
#' imputeKnn(dat, k = 2, distData = ref)
#'
#' @family impute
#' @concept imputation
#' @concept missing-value
#'
#' @export
imputeKnn <- function(x,
                      k = 10,
                      scale = TRUE,
                      method = c("weighted", "median"),
                      distData = NULL) {

  method <- match.arg(method)

  if(!is.data.frame(x))
    stop("Argument 'x' must be a data frame.")

  if(!is.numeric(k) || length(k) != 1L || !is.finite(k) ||
     k < 1 || k != round(k))
    stop("Argument 'k' must be a single positive whole number.")

  if(!is.logical(scale) || length(scale) != 1L || is.na(scale))
    stop("Argument 'scale' must be a single non-missing logical value.")

  n <- nrow(x)

  if(n == 0L)
    stop("Argument 'x' must have at least one row.")

  if(!is.null(distData)) {

    if(!is.data.frame(distData))
      stop("Argument 'distData' must be a data frame.")

    if(!identical(names(x), names(distData)))
      stop("Arguments 'x' and 'distData' must have the same variables.")

    full <- rbind(x, distData)
    poolIdx <- (n + 1L):nrow(full)

  } else {

    full <- x
    poolIdx <- seq_len(n)

  }

  isNominal <- vapply(full, is.factor, logical(1))
  nomIdx <- which(isNominal)
  contIdx <- which(!isNominal)

  if(!all(vapply(full[contIdx], is.numeric, logical(1))))
    stop("Argument 'x' must contain only numeric and factor variables.")

  # Two separate matrices rather than one: mixing integer-coded factors into
  # the Euclidean part would both count them twice - once as a squared
  # difference, once as a mismatch - and impose an ordering on levels that
  # nominal data does not have.
  cont <- if(length(contIdx))
    as.matrix(full[, contIdx, drop = FALSE])
  else
    matrix(numeric(0), nrow = nrow(full), ncol = 0L)

  nom <- if(length(nomIdx))
    vapply(full[, nomIdx, drop = FALSE], as.integer, integer(nrow(full)))
  else
    matrix(integer(0), nrow = nrow(full), ncol = 0L)

  if(!is.matrix(nom))
    nom <- matrix(nom, nrow = nrow(full))

  # scale() computes its constants with na.rm = TRUE, so only the missing
  # cells stay missing and the column remains usable.
  if(scale && ncol(cont)) {

    # A constant column has sd 0, and scale() then divides by it: the whole
    # column becomes NaN, every distance becomes NaN, and order() on an
    # all-NaN vector returns 1..n - so the "nearest" neighbours would
    # silently be the first k rows of the pool. Such a column carries no
    # information about distance anyway, so drop it from the scaling
    # rather than let it poison the metric.
    sds <- apply(cont, 2L, stats::sd, na.rm = TRUE)
    keep <- is.finite(sds) & sds > 0

    if(!all(keep))
      warning(gettextf(
        "%d numeric variable(s) have no variation and are ignored in the distance",
        sum(!keep)), domain = NA)

    cont <- if(any(keep))
      scale(cont[, keep, drop = FALSE])
    else
      matrix(numeric(0), nrow = nrow(cont), ncol = 0L)

    contIdx <- contIdx[keep]

  }

  naRows <- which(!complete.cases(full))
  target <- naRows[naRows <= n]

  if(length(target) == 0L) {

    warning("No missing values found.")
    return(x)

  }

  poolComplete <- setdiff(poolIdx, naRows)

  if(length(poolComplete) < k)
    stop(gettextf(
      "Only %d complete observations are available, fewer than k = %d.",
      length(poolComplete), k), domain = NA)

  # One pass per pattern of missingness rather than per row: rows sharing a
  # pattern share their observed variables and therefore their distance
  # definition, so their neighbours can be found in a single query.
  missPattern <- apply(is.na(full[target, , drop = FALSE]), 1,
                       function(z) paste(which(z), collapse = ","))

  useTree <- requireNamespace("dbscan", quietly = TRUE)

  for(pat in unique(missPattern)) {

    rows <- target[missPattern == pat]
    missCols <- as.integer(strsplit(pat, ",", fixed = TRUE)[[1]])
    obsCols <- setdiff(seq_len(ncol(full)), missCols)

    if(length(obsCols) == 0L)
      stop("Some observations have no observed variables to match on.")

    obsCont <- intersect(contIdx, obsCols)
    obsNom <- intersect(nomIdx, obsCols)

    # Column positions within cont / nom, which hold only their own kind.
    cCols <- match(obsCont, contIdx)
    nCols <- match(obsNom, nomIdx)

    nbr <- .knnNeighbours(
      queryCont = cont[rows, cCols, drop = FALSE],
      queryNom  = nom[rows, nCols, drop = FALSE],
      poolCont  = cont[poolComplete, cCols, drop = FALSE],
      poolNom   = nom[poolComplete, nCols, drop = FALSE],
      k         = k,
      useTree   = useTree
    )

    for(r in seq_along(rows)) {

      i <- rows[r]
      idx <- poolComplete[nbr$id[r, ]]

      w <- if(method == "weighted")
        .knnWeights(nbr$dist[r, ])
      else
        NULL

      for(j in missCols)
        x[i, j] <- .centralValue(full[idx, j], weights = w)

    }

  }

  x

}


# == internal helper functions ================================================

# Neighbour search over a mixed-type distance: squared differences on the
# numeric block plus a 0/1 mismatch on the nominal block.
#
# Returns id and dist matrices with one row per query point, as dbscan::kNN
# does, so that both paths are interchangeable.
.knnNeighbours <- function(queryCont, queryNom, poolCont, poolNom,
                           k, useTree) {

  nQuery <- nrow(queryCont)
  nPool <- nrow(poolCont)

  # With no nominal variables in play the distance is plain Euclidean, which
  # is exactly what the kd-tree indexes. Any mismatch term would break the
  # metric assumptions the tree relies on, so the tree is used only here.
  if(useTree && ncol(queryNom) == 0L && ncol(queryCont) > 0L) {

    res <- dbscan::kNN(x = poolCont, k = k, query = queryCont)

    return(list(id = res$id, dist = res$dist))

  }

  id <- matrix(NA_integer_, nQuery, k)
  dst <- matrix(NA_real_, nQuery, k)

  for(r in seq_len(nQuery)) {

    d2 <- if(ncol(poolCont))
      colSums((t(poolCont) - queryCont[r, ])^2)
    else
      numeric(nPool)

    if(ncol(poolNom))
      d2 <- d2 + colSums(t(poolNom) != queryNom[r, ])

    d <- sqrt(d2)

    # partial sort: only the k smallest are needed, not a full ordering
    ord <- order(d)[seq_len(k)]

    id[r, ] <- ord
    dst[r, ] <- d[ord]

  }

  list(id = id, dist = dst)

}


# exp(-d) decays on the scale of d itself, so distances in large units drive
# every weight to zero and leave sum(w) == 0. Shifting by the minimum first
# fixes the largest weight at 1 and keeps the ratios between neighbours,
# since a common factor cancels in the normalization.
.knnWeights <- function(d) {

  if(all(!is.finite(d)))
    return(rep(1, length(d)))

  w <- exp(-(d - min(d, na.rm = TRUE)))

  w[!is.finite(w)] <- 0

  if(sum(w) == 0)
    rep(1, length(d))
  else
    w

}


.centralValue <- function(x, weights = NULL) {

  if(is.numeric(x)) {

    if(is.null(weights))
      return(median(x, na.rm = TRUE))

    s <- sum(weights)

    if(s > 0)
      sum(x * (weights / s))
    else
      NA

  } else {

    x <- as.factor(x)

    if(is.null(weights))
      levels(x)[which.max(table(x))]
    else
      levels(x)[which.max(tapply(weights, x, sum, default = 0))]

  }

}
