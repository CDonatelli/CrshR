
stabWithHold <- function(data){
  #Select break point
    plot(data$Time, data$Load, main = "Select the Crack Point then click 'finish.'")
    crack <- sapply(list(data$Time,data$Load),"[",identify(data$Time,data$Load))
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
    
    differences <- diff(data$Extension)
    holdIndices <- data.frame(which(differences == 0, arr.ind = T))
    colnames(holdIndices)[1]<-"diffs"
    holdIndices<-holdIndices[!(holdIndices$diffs <= 5 | 
                               holdIndices$diffs >= holdIndices$diffs[nrow(holdIndices)]-5),]
    holdStartIndex <- holdIndices[1]
    holdEndIndex <- holdIndices[length(holdIndices)]
    
    
  
  #Find Max Values
    maxLoadIndex <- which.max(data$Load)
    extensionAtMaxLoad <- data$Extension[maxLoadIndex]
    maxLoad <- data$Load[maxLoadIndex]
    
    maxExtensionIndex <- which.max(data$Extension)
    maxExtension <- data$Extension[maxExtensionIndex]
    loadAtMaxExt <- data$Load[maxExtensionIndex]
    
    creepLoss <- data$Load[holdStartIndex]-data$Load[holdEndIndex]
    creepLoadStart <- data$Load[holdStartIndex]
    creepLoadEnd <- data$Load[holdEndIndex]
    
  
  #Data Subsets
    dataToCrack = data[c(1:crackIndex),]
    dataToMaxLoad = data[c(1:maxLoadIndex),]
    dataToHold = data[c(1:holdStartIndex),]
    dataCreep = data[c(holdStartIndex:holdEndIndex),]
    dataToRemove = data[c(holdEndIndex,nrow(data)),]
    
    CrackPropigationIndex <- which(abs(dataToHold$Extension-CrackPropigationExt)
                                   ==min(abs(dataToHold$Extension-CrackPropigationExt)))
    dataToPropigate = data[c(crackIndex, CrackPropigationIndex),]
  
  #Work Calculations
    workToCrack <- trapz(dataToCrack$Extension, dataToCrack$Load)
    workToMaxLoad <- trapz(dataToMaxLoad$Extension, dataToMaxLoad$Load)
    workToHoldStart <- trapz(dataToHold$Extension, dataToHold$Load)
    workToRemove <- trapz(dataToRemove$Extension, dataToRemove$Load)
    workToCrackProp <- trapz(dataToPropigate$Extension, dataToPropigate$Load)
  
    results <- data.frame(maxLoad, extensionAtMaxLoad, maxExtension, loadAtMaxExt, ExtensionAtCrack,
                          CrackPropigationExt, creepLoadStart, creepLoadEnd, creepLoss,
                          workToCrack, workToCrackProp, workToMaxLoad, workToHoldStart, workToRemove)
  return(results)
}