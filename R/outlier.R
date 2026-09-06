
#' Outlier
#' 
#' Return outliers following Tukey's boxplot and Hampel's median/mad
#' definition.
#' 
#' Outlier detection is a tricky problem and should be handled with care. We
#' implement Tukey's boxplot rule as a rough idea of spotting extreme values.
#' The fences are built from the hinges, exactly as
#' [grDevices::boxplot.stats()] does, so the result matches what
#' a boxplot of the same data draws. Note that the hinges are not the
#' type-7 quartiles of [stats::quantile()] and differ from them
#' for many sample sizes.
#' 
#' Hampel considers values outside of median +/- 3 * (median absolute
#' deviation) to be outliers.
#' 
#' @param x a non-empty numeric vector of data values
#' @param method the method to be used. So far Tukey's boxplot and Hampel's
#' rule are implemented.
#' @param value logical. If `FALSE`, a vector containing the (integer)
#' indices of the outliers is returned, and if `TRUE` (default), a vector
#' containing the matching elements themselves is returned.
#' @param na.rm logical. Should missing values be removed? Defaults to
#' `FALSE`.
#' 
#' @return the outlying values if `value = TRUE`; otherwise their indices
#' 
#' @note Performance improvement by Luis Gustavo Schuck.
#' 
#' @references Hampel F. R. (1974) The influence curve and its role in robust
#' estimation, *Journal of the American Statistical Association*, 69,
#' 382-393
#' 
#' @examples
#' outlier(Pizza$temperature, na.rm=TRUE)
#' 
#' # it's the same as the result from boxplot
#' sort(Pizza$temperature[outlier(Pizza$temperature, value=FALSE, na.rm=TRUE)])
#' b <- boxplot(Pizza$temperature, plot=FALSE)
#' sort(b$out)
#' 
#' # nice to find the corresponding rows
#' Pizza[outlier(Pizza$temperature, value=FALSE, na.rm=TRUE), ]
#' 
#' # compare to Hampel's rule
#' outlier(Pizza$temperature, method="hampel", na.rm=TRUE)
#' 
#' 
#' # outliers for the each driver
#' tapply(Pizza$temperature, Pizza$driver, outlier, na.rm=TRUE)
#' 
#' # the same as:
#' boxplot(temperature ~ driver, Pizza)$out
#' 
#' 
#' @seealso [boxplot()]
#' 
#' @family data.inspection
#' @concept outlier-detection
#' @export
outlier <- function(x, method=c("boxplot", "hampel"), value=TRUE, na.rm=FALSE){
  
  switch(match.arg(arg = method, choices = c("boxplot", "hampel")),
         
         boxplot =  {
           # fivenum(), not quantile(): boxplot.stats() builds its fences
           # from the HINGES, and those differ from the type-7 quartiles
           # for many sample sizes. For 1:20 the hinges are 5.5 and 15.5
           # while the quartiles are 5.75 and 15.25, so the quantile
           # fences are the narrower pair and this function flagged
           # points that the boxplot next to it did not draw as outliers
           # - although the examples assert the two agree.
           fn  <- fivenum(as.numeric(x), na.rm = na.rm)
           qq  <- fn[c(2L, 4L)]
           iqr <- diff(qq)
           id <- x < (qq[1] - 1.5 * iqr) | x > (qq[2] + 1.5 * iqr)
         },
         
         hampel = {
           med_x <- median(x, na.rm=na.rm)
           
           # hampel considers values outside of median +/- 3*(median absolute deviation) 
           # to be outliers
           id <- x %][% (med_x + c(-3, 3) * mad(x, na.rm=na.rm, center = med_x))
         }
  )
  
  if(value)
    res <- x[id]
  else
    res <- which(id)
  
  res <- res[!is.na(res)]
  
  return(res)
  
}



