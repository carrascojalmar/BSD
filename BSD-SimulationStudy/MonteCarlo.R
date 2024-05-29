# Monte Carlo -------------------------------------------------------------
simula <- function(R, nSample, theta) {
  
  Para <- matrix(NA, ncol=length(theta), nrow=length(nSample))
  Vies <- matrix(NA, ncol=length(theta), nrow=length(nSample))
  REQM <- matrix(NA, ncol=length(theta), nrow=length(nSample))
  Cov <- matrix(NA, ncol=length(theta), nrow=length(nSample))
  qntd <- matrix(NA, ncol=1, nrow=length(nSample))
  
  for(i in 1:length(nSample)){
    n <- nSample[i]
    
    mPara <- matrix(NA, ncol=length(theta), nrow=R)
    mVies <- matrix(NA, ncol=length(theta), nrow=R)
    mEQM <- matrix(NA, ncol=length(theta), nrow=R)
    mCov <- matrix(NA, ncol=length(theta), nrow=R)
    
    r <- 1
    qtd <- 0
    
    while (r <= R) {
      qtd <- qtd + 1
      
      dados <- rBsimplex(n=n,theta=theta)
      
      op <- try(optim(par = theta, fn = lvBsimplex, dados = dados, hessian = T,
                     method = "L-BFGS-B",
                     lower=c(0,0,0,0,-1),upper=c(1,1,Inf,Inf,1)),silent = TRUE)
      
      if(!class(op)=="try-error"){
        
        ep <- sqrt(diag(solve(op$hessian)))
        
        if(anyNA(ep)==FALSE){
          
          mPara[r,] <- op$par
          mVies[r,] <- op$par-theta
          mEQM[r,] <- (op$par-theta)^2
          
          Li <- op$par-1.96*ep
          Ls <- op$par+1.96*ep
          
          for(j in 1:length(theta)){
            if(theta[j]>Li[j] & theta[j]<Ls[j]){
              mCov[r,j] <- 1}else{
                mCov[r,j] <-0}
          }
          
          print(r)
          r <- r+1
          
        }
      }
      print(c("qtd", qtd))
    }
    
    Para[i,] <- apply(mPara,2,mean)
    Vies[i,] <- apply(mVies,2,mean)
    REQM[i,] <- sqrt(apply(mEQM,2,mean))
    Cov[i,] <- 100*(apply(mCov,2,mean))
    
    qntd[i,] <- qtd
  }
  
  resultado <- list()
  resultado$Medias <- Para
  resultado$Vies <- Vies
  resultado$REQM <- REQM
  resultado$Cov <- Cov
  resultado$qntd <- qntd
  
  return(resultado)
}
