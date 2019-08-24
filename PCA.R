PCA <- setRefClass(
  "PCA",
  fields = list(eVal = "numeric", eVec = "numeric", cumVar = "numeric", norm = "logical"),
  methods = list(
    
    norm <- function(x) {
      #Check if data has been normalised
      if (norm == FALSE || norm == null) {
        #normalise data
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
    }, 
    
    PCs <- function(x) {
      #Center Data
      norm(x)
      
      #Find eigenvalues and eigenvectors of covariance matrix (principal components)
      eVal <<- eigen(cov(x))$values
      eVec <<- eigen(cov(x))$vectors
      
      #return principal components
      print("Standard Deviations: \n")
      print(sqrt(eVal) + "\n\n")
    }
    
    
    #Make list of eVal, eVec pairs sorted descending
    # ePairs <- matrix(nrow = ncol(eVec) * nrow(eVec), ncol = 2)
    # temp <- 0
    # for (j in 1:ncol(eVec)) {
    #   for (i in 1:nrow(eVec)) {
    #     ePairs[i + temp, 1] <- eVal[j]
    #     ePairs[i + temp, 2] <- eVec[i, j]
    #   }
    #   temp <- temp + nrow(eVec)
    # }
    
    # #Choose most important components to reduce dimensionality
    # expVarPer <- 97
    # varExp <- matrix()
    #
    # for (i in 1:length(eVal)) {
    #   varExp[i] <- (eVal[i] / sum(eVal))
    # }
    #
    # cumVar <- cumsum(varExp)
    # vectokeep <- 0
    #
    # for (i in 1:length(varexp)) {
    #   if (expvarper <= cumvar[i]) {
    #     break
    #   } else {
    #     vectokeep <- vectokeep + 1
    #   }
    # }
    
    #Build projection matrix and transform space to new components
    #projMat <- matrix(ncol = vecToKeep, nrow = nrow(eVec))
    
    # for (i in 1:vecToKeep) {
    #   projMat[,i] <- eVec[,i]
    # }
    
  )
)
