
# bivariate density function

dBsimplex <- function(theta, dados, log = FALSE){
  mux <- theta[1]
  muy <- theta[2]
  sigmax <- theta[3]
  sigmay <- theta[4]
  lambda <- theta[5]
  
  y1 <- unlist(dados[,1])
  y2 <- unlist(dados[,2])
  
  fx <- dSIMPLEX(x=y1, mu=mux, sigma=sigmax, log = FALSE)
  fy <- dSIMPLEX(x=y2, mu=muy, sigma=sigmay, log = FALSE)
  
  Fx <- pSIMPLEX(q=y1, mu=mux, sigma=sigmax,lower.tail=TRUE, log.p = FALSE)
  Fy <- pSIMPLEX(q=y2, mu=muy, sigma=sigmay,lower.tail=TRUE, log.p = FALSE)
  
  fxy = (fx*fy)*(1+lambda*(2*Fx-1)*(2*Fy-1))
  
  if(log==TRUE){return(log(fxy))}else{return(fxy)}
}

# log-likelihood

lvBsimplex = function(theta, dados){
  func = dBsimplex(theta, dados, log = TRUE)
  return(-sum(func))
}
