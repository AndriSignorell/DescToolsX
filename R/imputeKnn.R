
#' K-Nearest Neighbors Imputation
#'
#' Imputes missing values using a k-nearest neighbors (KNN) approach.
#' Supports both numeric and categorical (factor) variables.
#'
#' @param data A data frame with missing values.
#' @param k Positive integer specifying the number of nearest neighbors.
#' @param scale Logical; should numeric variables be scaled before distance computation?
#'   Scaling is only used for distance calculation and does not affect imputed values.
#' @param method Character string specifying the aggregation method:
#'   \itemize{
#'     \item \code{"weighted"}: weighted mean (numeric) / weighted mode (factor)
#'     \item \code{"median"}: median (numeric) / mode (factor)
#'   }
#' @param distData Optional data frame used only for distance computation.
#'   If provided, nearest neighbors are searched in \code{rbind(data, distData)},
#'   but only \code{data} is imputed.
#'
#' @details
#' Numeric variables are scaled (if \code{scale = TRUE}) before computing distances.
#' Imputed values are always taken from the original (unscaled) data.
#'
#' Distances are computed as Euclidean distances for numeric variables.
#' For categorical variables, a simple mismatch penalty (Hamming distance)
#' is added. Note that continuous and categorical distances are combined
#' additively without further weighting, which may affect their relative influence.
#'
#' @return A data frame with imputed values.
#'
#' @seealso \code{\link{impute}}, \code{\link{locf}}
#'
#' @examples
#' set.seed(123)
#' dat <- data.frame(
#'   x = c(1, 2, NA, 4),
#'   y = c(NA, 2, 3, 4),
#'   z = factor(c("a", "b", "a", NA))
#' )
#'
#' imputeKnn(dat, k = 2)
#'

#' @export
imputeKnn <- function(data,
                      k = 10,
                      scale = TRUE,
                      method = c("weighted", "median"),
                      distData = NULL) {
  
  method <- match.arg(method)
  
  # --- Validation ---
  if (!is.data.frame(data))
    stop("'data' must be a data.frame.")
  
  if (!is.numeric(k) || length(k) != 1 || k < 1 || k != floor(k))
    stop("'k' must be a positive integer.")
  
  n <- nrow(data)
  
  # --- Combine data if distData is provided ---
  if (!is.null(distData)) {
    data_full <- rbind(data, distData)
    dist_idx <- (n + 1):nrow(data_full)
  } else {
    data_full <- data
    dist_idx <- seq_len(nrow(data_full))
  }
  
  # --- Identify variable types ---
  is_nominal <- vapply(data_full, is.factor, logical(1))
  nom_idx <- which(is_nominal)
  cont_idx <- which(!is_nominal)
  
  # --- Working copy for distance computation ---
  dm <- data_full
  
  # Note: scaling is applied to dm only (for distance computation),
  # imputed values are taken from the original data_full
  if (scale && length(cont_idx)) {
    dm[, cont_idx] <- scale(dm[, cont_idx])
  }
  
  # encode factors as integers (only for distance)
  if (length(nom_idx)) {
    for (j in nom_idx)
      dm[, j] <- as.integer(dm[, j])
  }
  
  dm <- as.matrix(dm)
  
  # --- Missing rows ---
  na_rows <- which(!complete.cases(dm))
  
  if (length(na_rows) == 0) {
    warning("No missing values found.")
    return(data)
  }
  
  # --- Complete cases for neighbors ---
  complete_idx <- setdiff(dist_idx, na_rows)
  
  if (length(complete_idx) < k)
    stop("Not enough complete cases for k nearest neighbors.")
  
  xcomplete <- dm[complete_idx, , drop = FALSE]
  
  # --- Main loop ---
  for (i in na_rows[na_rows <= n]) {
    
    miss_cols <- which(is.na(dm[i, ]))
    obs_cols <- setdiff(seq_len(ncol(dm)), miss_cols)
    
    xi <- dm[i, obs_cols]
    xc <- xcomplete[, obs_cols, drop = FALSE]
    
    # Euclidean distance (numeric part)
    d <- sqrt(rowSums((xc - matrix(xi, nrow(xc), length(xi), byrow = TRUE))^2))
    
    # Add mismatch penalty for categorical variables
    if (length(nom_idx)) {
      nom_obs <- intersect(nom_idx, obs_cols)
      if (length(nom_obs)) {
        for (j in nom_obs) {
          d <- d + (xcomplete[, j] != dm[i, j])
        }
      }
    }
    
    nn <- order(d)[seq_len(k)]
    
    for (j in miss_cols) {
      
      vals <- data_full[complete_idx[nn], j]
      
      if (method == "median") {
        data[i, j] <- .centralValue(vals)
      } else {
        w <- exp(-d[nn])
        data[i, j] <- .centralValue(vals, weights = w)
      }
    }
  }
  
  return(data)
}





# == internal helper functions ==========================================

#' @keywords internal
.centralValue <- function(x, weights=NULL) {
  if (is.numeric(x)) {
    if (is.null(weights)) median(x,na.rm=TRUE)
    else if ((s <- sum(weights)) > 0) sum(x*(weights/s)) else NA
  } else {
    x <- as.factor(x)
    if (is.null(weights)) levels(x)[which.max(table(x))]
    else levels(x)[which.max(aggregate(weights, list(x), sum)[, 2])]
  }
}

