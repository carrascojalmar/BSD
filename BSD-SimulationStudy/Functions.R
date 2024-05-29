library(gamlss)

# Função de Densidade -----------------------------------------------------
dBsimplex <- function(theta, dados, log = FALSE){
  
  mux <- theta[1]
  muy <- theta[2]
  sigmax <- theta[3]
  sigmay <- theta[4]
  lambda <- theta[5]
  
  x <- dados[, 1]
  y <- dados[, 2]
  
  fx <- dSIMPLEX(x, mux, sigmax)
  fy <- dSIMPLEX(y, muy, sigmay)
  
  Fx <- pSIMPLEX(x, mux, sigmax)
  Fy <- pSIMPLEX(y, muy, sigmay)
  
  fxy <- (fx*fy)*(1+lambda*(2*Fx-1)*(2*Fy-1))
  
  if(log==TRUE){return(log(fxy))}else{return(fxy)}
}

# Função da log-verossimilhança -------------------------------------------
lvBsimplex <- function(theta, dados){
  func <- dBsimplex(theta, dados, log = TRUE)
  return(-sum(func))
}

# Função geradora de números aleatórios -----------------------------------
rBsimplex <- function(n,theta){
  u1 <- runif(n)
  v <- runif(n)
  A <- (theta[5]*(2*u1-1))-1
  B <- ((1-theta[5]*(2*u1-1))^2) + 4*v*theta[5]*(2*u1-1)
  u2 <- (2*v)/(sqrt(B)-A)
  y1 <- qSIMPLEX(u1, theta[1], theta[3])
  y2 <- qSIMPLEX(u2, theta[2], theta[4])
  return(cbind(y1,y2))
}
