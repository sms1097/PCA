PCA <- function(x) {
  
  #Normalize Data
  for (i in 1:ncol(x)) {
    if (is.numeric(x[, i])) {
      mu <- mean(x[, i], na.rm = TRUE)
      s <- sd(x[, i], na.rm = TRUE)
      
      for (j in 1:nrow(x)) {
        if (!is.na(x[j, i]))
          x[j, i] <- (x[j, i] - mu) / s
      }
    }
  }
  
  #Find eigenvalues and eigenvectors of covariance matrix
  eVal <- eigen(cov(x))$values
  eVec <- eigen(cov(x))$vectors
  
  #Make list of eVal, eVec pairs sorted descending
  ePairs <- matrix(nrow = ncol(eVec) * nrow(eVec), ncol = 2)
  temp <- 0
  for (j in 1:ncol(eVec)) {
    for (i in 1:nrow(eVec)) {
      ePairs[i + temp, 1] <- eVal[j]
      ePairs[i + temp, 2] <- eVec[i, j]
    }
    temp <- temp + 11
  }
  
  #Choose most important components to reduce dimensionality
  expVarPer <- 97
  varExp <- matrix()
  
  for (i in 1:length(eVal)) {
    varExp[i] <- (eVal[i] / sum(eVal)) * 100
  }
  
  cumVar <- cumsum(varExp)
  
  vecToKeep <- 0
  
  for (i in 1:length(varExp)) {
    if (expVarPer <= cumVar[i]) {
      break
    } else {
      vecToKeep <- vecToKeep + 1
    }
  }
  
  #Build projection matrix and transform space to new components
  projMat <- matrix(ncol = vecToKeep, nrow = nrow(eVec))
  
  for (i in 1:vecToKeep) {
    projMat[,i] <- eVec[,i]
  }

  #project data into new space
  return(as.matrix(x) %*% projMat)
  
}
