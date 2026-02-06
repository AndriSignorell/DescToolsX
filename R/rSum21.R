
rSum21 <- function(size, digits=NULL){
  
  rnd <- (p <- runif(n = size))/sum(p)
  
  if(!is.null(digits)){
    rnd <- round(rnd, digits = digits)
    rnd[1] <- rnd[1] + (1-sum(rnd))
  }
  
  rnd
  
}

