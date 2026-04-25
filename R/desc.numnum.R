


.desc_num_num <- function(x, y) {
  
  # NA Handling
  if (na.rm) {
    ok <- complete.cases(x, y)
    x <- x[ok]
    y <- y[ok]
  }
  
  n <- length(x)
  
  if (n == 0) {
    stop("No complete observations")
  }
  
  # Basiskennzahlen
  stats <- list(
    n = n,
    
    x = list(
      mean = mean(x),
      sd   = sd(x),
      median = median(x),
      iqr = IQR(x),
      min = min(x),
      max = max(x)
    ),
    
    y = list(
      mean = mean(y),
      sd   = sd(y),
      median = median(y),
      iqr = IQR(y),
      min = min(y),
      max = max(y)
    ),
    
    cor_pearson  = cor(x, y, method = "pearson"),
    cor_spearman = cor(x, y, method = "spearman")
  )
  
  # Modelle
  fit_lm <- lm(y ~ x)
  
  # LOESS nur wenn sinnvoll
  fit_loess <- NULL
  if (length(unique(x)) >= 10 && n >= 20) {
    fit_loess <- loess(y ~ x)
  }
  
  models <- list(
    lm = fit_lm,
    loess = fit_loess
  )
  
  # Erweiterte Infos
  model_stats <- list(
    lm = list(
      coef = coef(fit_lm),
      r2   = summary(fit_lm)$r.squared
    )
  )
  
  # Output
  res <- list(
    type = "num-num",
    stats = stats,
    models = models,
    model_stats = model_stats
  )
  
  class(res) <- "desc_num_num"
  
  return(res)
}
