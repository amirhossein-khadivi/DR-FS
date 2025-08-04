pre_data_normalization <- function(data, method = "zscore") {
  # The methods for normalization
  # (a) 'zscore': mean=0; std=1 (default)
  # (b) 'normc': sum(c_1 .* c_1) = 1
  # (c) 'linear': (x-min(X))/(max(X) - min(X))
  # (d) 'clinear': x/max(abs(X))
  # while in Mutual Information analysis, (c) is equal to (d)
  
  
  if (method != 'zscore' & method != 'linear' & method != 'clinear'& method != 'normc')stop('method ra dorost vared konid.')
  
  numSample <- nrow(data)
  numFeature <- ncol(data)
  
  if (method == 'zscore'){
    dataNorm <- scale(data)
    dataNorm[is.na(dataNorm)] <- 0
  }else if (method == 'normc'){
    dataNorm <- sweep(data, 2, sqrt(colSums(data^2)), "/")
  }else if (method == 'linear'){
    dataNorm <- matrix(0, nrow = numSample, ncol = numFeature)
    for (ii in 1:numFeature) {
      tmpFeature <- data[, ii]
      dataNorm[, ii] <- 
        (tmpFeature - min(tmpFeature)) / (max(tmpFeature) - min(tmpFeature)) 
    }
    }else{
      dataNorm <- matrix(0, nrow = numSample, ncol = numFeature)
      for (ii in 1:numFeature) {
        tmpFeature <- data[, ii]
        tmpMax <- max(abs(tmpFeature))
        dataNorm[, ii] <- tmpFeature / tmpMax
      }
    }
      
  return(dataNorm)
}

