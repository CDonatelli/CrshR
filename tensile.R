
  tensile <- function(data, CSA, L0){
    #Select break point
    data$Load <- abs(data$Load)
    data$Extension <- abs(data$Extension)
    
    data$StressCalc = data$Load/CSA
    data$StrainCalc = (L0 - data$Extension)/L0
    
    #Find Max Values
      maxLoadIndex <- which.max(data$Load)
      extensionAtMaxLoad <- data$Extension[maxLoadIndex]
      maxLoad <- data$Load[maxLoadIndex]
      
      maxExtensionIndex <- which.max(data$Extension)
      maxExtension <- data$Extension[maxExtensionIndex]
      loadAtMaxExt <- data$Load[maxExtensionIndex]
      
    #Data Subsets
      dataToMaxLoad = data[c(1:maxLoadIndex),]
      dataToMaxExtension = data[c(1:maxExtensionIndex),]
    
    #Work Calculations
      workToMaxLoad <- trapz(dataToMaxLoad$Extension, dataToMaxLoad$Load)
      workToMaxExtension <- trapz(dataToMaxExtension$Extension, dataToMaxExtension$Load)
      toughness <- trapz(dataToMaxLoad$Stress, dataToMaxLoad$Strain)
      toughnessCalc <- trapz(dataToMaxLoad$StressCalc, dataToMaxLoad$StrainCalc)
    
      results <- data.frame(maxLoad, extensionAtMaxLoad, workToMaxLoad,
                            loadAtMaxExt, maxExtension, workToMaxExtension,
                            toughness, toughnessCalc)
    return(results)
  }