
  crush <- function(data){
    #Select break point
    data$Load <- abs(data$Load)
    x11() # for some reason identify wants its own window now
      plot(data$Time, data$Load, main = "Select the Crack Point then click 'Stop Locator.'")
      crack <- sapply(list(data$Time,data$Load),"[",identify(data$Time,data$Load, n=1))
      crackIndex <- which(crack[1] == data$Time)
      ExtensionAtCrack = data$Extension[crackIndex]
      CrackPropigationExt = ExtensionAtCrack+2
    
    #Select hold time
      # plot(data$Time, data$Extension, main = "Select hold start, then hold stop, then click 'finish.'")
      # hold <- sapply(list(data$Time,data$Extension),"[",identify(data$Time,data$Extension))
      # holdStartIndex <- which(hold[1,1] == data$Time)
      # print(holdStartIndex)
      # holdEndIndex <- which(hold[2,1] == data$Time)
      # print(holdEndIndex)
    
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
    
      results <- data.frame(maxLoad, extensionAtMaxLoad, maxExtension, loadAtMaxExt,
                            workToCrack, workToMaxLoad, workToMaxExtension)
    return(results)
  }