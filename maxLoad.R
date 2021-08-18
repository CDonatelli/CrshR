
maxLoad <- function(data){
  #Select break point
    data$Extension = data$Extension + abs(data$Extension[1])
  
    plot(data$Extension, data$Load, 
         main = "Select the Crack Point then click 'finish.'",
         pch = 16, cex = 0.1)
    lines(data$Extension, data$Load, col = 'grey')
    # crack <- sapply(list(data$Time,data$Load),"[",identify(data$Time,data$Load))
    # crackIndex <- which(crack[1] == data$Time)
    # ExtensionAtCrack = data$Extension[crackIndex]
    # CrackPropigationExt = ExtensionAtCrack+2
  
  #Find Max Values
    maxLoadIndex <- which.max(data$Load)
    extensionAtMaxLoad <- data$Extension[maxLoadIndex]
    maxLoad <- data$Load[maxLoadIndex]
    
    points(extensionAtMaxLoad, maxLoad, pch = 23, bg = 'red')
    
    maxExtensionIndex <- which.max(data$Extension)
    maxExtension <- data$Extension[maxExtensionIndex]
    loadAtMaxExt <- data$Load[maxExtensionIndex]
    
  #Data Subsets
    dataToMaxLoad = data[c(1:maxLoadIndex),]
  
  #Work Calculations
    workToMaxLoad <- trapz(dataToMaxLoad$Extension, dataToMaxLoad$Load)
  
    results <- data.frame(maxLoad, extensionAtMaxLoad, 
                          maxExtension, loadAtMaxExt, 
                          workToMaxLoad)
  return(results)
}