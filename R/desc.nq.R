


.desc_nq <- function(x, g, ... ) {

  kw <- kruskal.test(x~g)
  
  res <- list(
          tab = .build_summary_table(
                   tapply(x, g, desc, plotit=FALSE)   # groupwise numeric description
                  ),
          test  = kw,
          vtest = leveneTest(x~g),
          eta   = .eta2_kruskal(H = kw$statistic, 
                                k = length(unique(g)), 
                                n = length(x))
          
        )
          
}  




#' @exportS3Method
print.Desc.nq <- function(x, digits = NULL, ...) {

  .printHeader(x$meta)
  
  cat(x$pair$strOut)
  printCharMatrix(x$res$tab, sep = 3, ...)
  
  out <- strTrim(capture.output(x$res$test)[c(2,5)])
  cat(gettextf("\n%s:\n  %s\n", out[1], out[2]))
  cat(gettextf("  \u03b7\u00b2 = %.3f (%s)\n\n", x$res$eta, attr(x$res$eta, "label")))
  
  out <- strTrim(capture.output(x$res$vtest)[c(2,5)])
  cat(gettextf("%s:\n  %s\n\n", out[1], out[2]))
  
  if(x$pair$missing_groups > 0){
    warning(gettextf("  Grouping variable contains %s NAs (%s).", 
            x$pair$missing_groups, fm(x$pair$missing_groups_p, fmt="per.sty")), 
            call. = FALSE)
  }
  
}



#' @exportS3Method
plot.Desc.nq <- function(x, which = NULL, ...){
  
  switch(as.character(which %||% "1"),
         "1" = {
           boxplot(x$data$y ~ x$data$x, ...)
           
           abline(h=mean(x$data$y, na.rm=TRUE), col="grey", lty="dotted")
           
           points(x=seq(length(unique(x$data$x))), 
                  y=tapply(x$data$y, x$data$x, mean, na.rm=TRUE),
                  pch=4)
           
         },
         "2" ={
           plotDens(x$data$y ~ x$data$x, ...)
           
         }
  )
  
}




# == internal helper functions ===============================================


.extract_nq_summary <- function(x) {
  
  out <- c(
    mean   = x$mean,
    median = unname(x$quant["median"]),
    sd     = x$sd,
    IQR    = x$IQR,
    n      = x$n,
    np     = x$n /x$length,
    NAs    = x$NAs,
    zeros  = x$`0s`
  )
  
  return(out)
}


.build_summary_table <- function(x) {
  
  # dd = Liste von Resultaten (benannt!)
  
  mat <- sapply(x, .extract_nq_summary)
  
  # calc percentages of valid cases
  mat[6,] <- mat[5,] / sum(mat[5,])
  
  # sicherstellen, dass Matrix
  mat <- as.matrix(mat)
  
  res <- rbind(
    fm(mat[c(1:4),], fmt=style("num.sty")),
    fm(mat[c(5),, drop=FALSE], fmt=style("abs.sty")),
    fm(mat[c(6),, drop=FALSE], fmt=style("per.sty")),
    mat[c(7:8),] <- fm(mat[c(7:8),], fmt=style("abs.sty"))
  )
  
  return(res)
  
}



# Eta² aus Kruskal-Wallis (Tomczak & Tomczak 2014)
# H = Kruskal-Wallis Statistik, k = Anzahl Gruppen, n = Gesamtn
.eta2_kruskal <- function(H, k, n) {
  eta2 <- (H - k + 1) / (n - k)
  eta2 <- max(0, eta2)   # kann bei kleinen n leicht negativ werden
  
  label <- cut(eta2,
               breaks = c(-Inf, 0.01, 0.06, 0.14, Inf),
               labels = c("negligible", "small", "moderate", "large"),
               right  = FALSE)
  
  structure(eta2, label = as.character(label))
}

