
  crush <- function(data){
    #Select break point
    data$Load <- abs(data$Load)
    x11() # for some reason identify wants its own window now
      plot(data$Time, data$Load, main = "Select the Crack Point, hold start, hold stop.'")
      userPts <- sapply(list(data$Time,data$Load),"[",identify(data$Time,data$Load, n=3))
      crackIndex <- which(userPts[1] == data$Time)
      ExtensionAtCrack = data$Extension[crackIndex]
      loadAtCrack = data$Load[crackIndex]
    
    #Hold Points
      holdStartIndex <- which(userPts[2] == data$Time)
      loadAtHoldStart = data$Load[holdStartIndex]
      holdEndIndex <- which(userPts[3] == data$Time)
      loadAtHoldEnd = data$Load[holdEndIndex]
    
    #Find Max Values
      maxLoadIndex <- which.max(data$Load)
      extensionAtMaxLoad <- data$Extension[maxLoadIndex]
      maxLoad <- data$Load[maxLoadIndex]
      
      maxExtensionIndex <- which.max(data$Extension)
      maxExtension <- data$Extension[maxExtensionIndex]
      loadAtMaxExt <- data$Load[maxExtensionIndex]
      
    #Data Subsets
      dataToCrack = data[c(1:crackIndex),]
      dataToMaxLoad = data[c(1:maxLoadIndex),]
      dataToMaxExtension = data[c(1:maxExtensionIndex),]
    
    #Work Calculations
      workToCrack <- trapz(dataToCrack$Extension, dataToCrack$Load)
      workToMaxLoad <- trapz(dataToMaxLoad$Extension, dataToMaxLoad$Load)
      workToMaxExtension <- trapz(dataToMaxExtension$Extension, dataToMaxExtension$Load)
    
      results <- data.frame(maxLoad, extensionAtMaxLoad, workToMaxLoad,
                            loadAtMaxExt, maxExtension, workToMaxExtension,
                            loadAtCrack, ExtensionAtCrack, workToCrack,
                            loadAtHoldStart, loadAtHoldEnd)
    return(results)
  }