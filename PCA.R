PCA <- function(x, s = F, expVarPer = 0.99) {
  require(ggplot2)
  require(ggfortify)
  
  #center the data
  cen(x)
  
  #Find eigenvalues and eigenvectors of covariance matrix (principal components)
  eVal <- eigen(cov(x))$values
  eVec <- eigen(cov(x))$vectors
  
  if (s == FALSE) {
    #return principal components
    cat("standard deviations: \n")
    print(sqrt(eVal))
    cat("\n")
    cat("Principal components from 1 to", ncol(eVec))
    cat("\n")
    return(eVec)
  } else {
    summarize(x, expVarPer, eVal, eVec)
  }
}

cen <- function(x) {
  for (i in 1:ncol(x)) {
    if (is.numeric(x[, i])) {
      mu <- mean(x[, i], na.rm = TRUE)
      
      for (j in 1:nrow(x)) {
        if (!is.na(x[j, i]))
          x[j, i] <- (x[j, i] - mu)
      }
    }
  }
}



summarize <- function(x, expVarPer, eVal, eVec) {
  
  #Make list of eVal, eVec pairs sorted descending
  ePairs <- matrix(nrow = (ncol(eVec) * nrow(eVec)), ncol = 2)
  temp <- 0
  for (j in 1:ncol(eVec)) {
    for (i in 1:nrow(eVec)) {
      ePairs[i + temp, 1] <- eVal[j]
      ePairs[i + temp, 2] <- eVec[i, j]
    }
    temp <- temp + nrow(eVec)
  }
  
  #Choose most important components to reduce dimensionality
  varExp <- matrix()
  
  for (i in 1:length(eVal)) {
    varExp[i] <- (eVal[i] / sum(eVal))
  }
  
  cumVar <- cumsum(varExp)
  vectokeep <- 0
  
  for (i in 1:length(varExp)) {
    if (expVarPer <= cumVar[i]) {
      break
    } else {
      vectokeep <- vectokeep + 1
    }
  }
  
  cat("Use first", vectokeep, "Principal components to expalain", 
    cumVar[vectokeep] * 100, "% of variability in the data.")
}
