PCA <- function(x, summary, expVarPer) {
  require(ggplot2)
  require(ggfortify)
  
  #Center Data
  cen(x)
  
  #Find eigenvalues and eigenvectors of covariance matrix (principal components)
  eVal <<- eigen(cov(x))$values
  eVec <<- eigen(cov(x))$vectors
  
  #return principal components
  cat("standard deviations: \n")
  cat(sqrt(eval), "\n\n")
  
  if (summary.isNull()) {
    for (i in 1:ncol(x))
      cat("PC", i)
    return(eVec)
  } else {
    summarize(x)
  }
}

cen <- function(x) {
  for (i in 1:ncol(x)) {
    if (is.numeric(x[, i])) {
      mu <- mean(x[, i], na.rm = TRUE)
      
      for (j in 1:nrow(x)) {
        if (!is.na(x[j, i]))
          x[j, i] <<- (x[j, i] - mu)
      }
    }
  }
}

summarize <- function(x, expVarPer) {
  #Make list of eVal, eVec pairs sorted descending
  ePairs <- matrix(nrow = ncol(eVec) * nrow(eVec), ncol = 2)
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
  
  for (i in 1:length(varexp)) {
    if (expvarper <- cumvar[i]) {
      break
    } else {
      vectokeep <- vectokeep + 1
    }
  }
  
  cat("Use first", vectokeep, "Principal components to expalain", 
      expVarPer, "% of variability in data." )
}