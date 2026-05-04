


.assocsGen <- function(x, y = NULL,
                   which = c("all","gamma","tau_a","tau_b","tau_c","somers","cstat"),
                   conf.level = NA,
                   direction = c("row","column")) {
  
  direction <- match.arg(direction)
  which <- match.arg(which)
  
  # ============================
  # XY MODE (use C++)
  # ============================
  if(!is.null(y)){
    
    cl <- if(is.na(conf.level)) NA_real_ else conf.level
    z <- assoc_cpp(x, y, cl)
    
    res_all <- list(
      gamma  = if(is.na(conf.level)) z["gamma"] else z[c("gamma","gamma_l","gamma_u")],
      tau_a  = if(is.na(conf.level)) z["tau_a"] else z[c("tau_a","tau_a_l","tau_a_u")],
      tau_b  = if(is.na(conf.level)) z["tau_b"] else z[c("tau_b","tau_b_l","tau_b_u")],
      tau_c  = if(is.na(conf.level)) z["tau_c"] else z[c("tau_c","tau_c_l","tau_c_u")],
      somers = if(is.na(conf.level)) z["somers"] else z[c("somers","somers_l","somers_u")],
      cstat  = if(is.na(conf.level)) z["cstat"] else z[c("cstat","cstat_l","cstat_u")]
    )
    
  } else {
    
    # ============================
    # TABLE MODE
    # ============================
    tab <- as.table(x)
    
    # base counts
    cd <- conDisPairs(tab)
    
    C  <- unname(cd[["C"]])
    D  <- unname(cd[["D"]])
    Tx <- unname(cd[["Ties_X"]])
    Ty <- unname(cd[["Ties_Y"]])
    
    n  <- sum(tab)
    n0 <- n*(n-1)/2
    S  <- C - D
    
    # measures
    gamma <- S / (C + D)
    tau_a <- S / n0
    tau_b <- S / sqrt((n0 - Tx)*(n0 - Ty))
    
    m <- min(dim(tab))
    tau_c <- 2*S*m/(n^2*(m-1))
    
    ni <- if(direction=="row") colSums(tab) else rowSums(tab)
    denom <- n0 - sum(ni*(ni-1)/2)
    
    somers <- S / denom
    cstat  <- (somers + 1)/2
    
    # no CI case
    if(is.na(conf.level)){
      res_all <- list(
        gamma  = gamma,
        tau_a  = tau_a,
        tau_b  = tau_b,
        tau_c  = tau_c,
        somers = somers,
        cstat  = cstat
      )
      
    } else {
      
      # exact table CI
      ci_all <- .table_assoc_ci(tab, conf.level, direction)
      
      res_all <- list(
        gamma  = ci_all$gamma,
        tau_a  = ci_all$tau_a,
        tau_b  = ci_all$tau_b,
        tau_c  = ci_all$tau_c,
        somers = ci_all$somers,
        cstat  = ci_all$cstat
      )
    }
  }
  
  # ============================
  # SELECT WHICH
  # ============================
  if(which != "all"){
    res_all <- res_all[which]
  }
  
  return(res_all)
}




.table_assoc_ci <- function(tab, conf.level=0.95, direction="row"){
  
  cd <- conDisPairsTab_cpp(tab)
  
  pi.c <- cd$pi.c
  pi.d <- cd$pi.d
  
  n <- sum(tab)
  S <- cd$C - cd$D
  
  n0 <- n*(n-1)/2
  
  # ties
  rowSum <- rowSums(tab)
  colSum <- colSums(tab)
  
  Tx <- cd$Ties_X
  Ty <- cd$Ties_Y
  
  # ============================
  # SOMERS
  # ============================
  ni <- if(direction=="row") colSum else rowSum
  T  <- n0 - sum(ni*(ni-1)/2)

  psi_s <- (T*(pi.c - pi.d) - S*(n - ni)) / T^2
  var_s <- sum(tab * psi_s^2)

  somers <- S / T


  # ============================
  # GAMMA
  # ============================
  denom <- cd$C + cd$D
  
  psi_g <- ((pi.c - pi.d)*denom - S*(pi.c + pi.d)) / denom^2
  var_g <- sum(tab * psi_g^2)
  
  gamma <- S / denom
  
  # ============================
  # TAU-A
  # ============================
  psi_a <- (pi.c - pi.d) / n0
  var_a <- sum(tab * psi_a^2)
  
  tau_a <- S / n0
  
  # ============================
  # TAU-C
  # ============================
  m <- min(dim(tab))
  k <- 2*m/(n^2*(m-1))
  
  psi_c <- k * (pi.c - pi.d)
  var_c <- sum(tab * psi_c^2)
  
  tau_c <- k * S
  
  # ============================
  # TAU-B 
  # ============================
  ti <- rowSum
  uj <- colSum
  
  n1 <- sum(ti * (ti-1) / 2)
  n2 <- sum(uj * (uj-1) / 2)
  
  tau_b <- S / sqrt((n0-n1)*(n0-n2))
  
  # probabilities
  pi <- tab / n
  
  pdiff <- (pi.c - pi.d) / n
  Pdiff <- 2 * S / n^2
  
  rowsum <- rowSums(pi)
  colsum <- colSums(pi)
  
  rowmat <- matrix(rep(rowsum, ncol(tab)), ncol = ncol(tab))
  colmat <- matrix(rep(colsum, nrow(tab)), nrow = nrow(tab), byrow = TRUE)
  
  delta1 <- sqrt(1 - sum(rowsum^2))
  delta2 <- sqrt(1 - sum(colsum^2))
  
  tauphi <- (2 * pdiff + Pdiff * colmat) * delta2 * delta1 +
    (Pdiff * rowmat * delta2) / delta1
  
  sigma2_b <- ((sum(pi * tauphi^2) - sum(pi * tauphi)^2) /
                 (delta1 * delta2)^4) / n
  
  if(sigma2_b < .Machine$double.eps * 10) sigma2_b <- 0
  
  # ============================
  # CI helper
  # ============================
  z <- qnorm(1-(1-conf.level)/2)
  
  mk <- function(est, var){
    se <- sqrt(var)
    c(est=est,
      lci=max(est - z*se, -1),
      uci=min(est + z*se, 1))
  }
  
  list(
    somers = mk(somers,  var_s),
    gamma  = mk(gamma,  var_g),
    tau_a  = mk(tau_a,  var_a),
    tau_b  = mk(tau_b,  sigma2_b),
    tau_c  = mk(tau_c,  var_c),
    cstat  = mk((somers+1)/2, var_s/4)
  )
}




