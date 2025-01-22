
  bend_3pt <- function(data, length, setDisp, setLoad){
    #pracma package required for trapz
    
    data$Time = as.numeric(data$Time)
    data$Extension = as.numeric(data$Extension)
    data$Load = as.numeric(data$Load)
    
    #data$Load <- abs(data$Load)
    data$h = sqrt((length/2)^2 + abs(data$Extension^2))
    data$Angle = rad2deg(asin(data$Extension * (sin(90)/data$h)))
    
    #Select break and change points
    #Comment out if you don't need break or hold data
    #########
      # x11() # for some reason identify wants its own window now
      # plot(data$Time, data$Load, main = "Select the Change in Slope first. \n Select Hold Start second. \n Select Hold End third \n Then close window")      
      # userPts <- sapply(list(data$Time,data$Load),"[",identify(data$Time,data$Load, n=3))
      # 
      # deltaSlopeIndex <- which(userPts[1] == data$Time)
      # ExtensionAtDeltaSlope = data$Extension[deltaSlopeIndex]
      # loadAtDeltaSlope = data$Load[deltaSlopeIndex]
      # angleAtDeltaSlope = data$Angle[deltaSlopeIndex]
      # 
      # # breakIndex <- which(userPts[2] == data$Time)
      # # ExtensionAtBreak = data$Extension[breakIndex]
      # # loadAtBreak = data$Load[breakIndex]
      # # angleAtBreak = data$Angle[breakIndex]
      # 
      # holdStartIndex <- which(userPts[2] == data$Time)
      # loadAtHoldStart = data$Load[holdStartIndex]
      # holdEndIndex <- which(userPts[3] == data$Time)
      # loadAtHoldEnd = data$Load[holdEndIndex]
    ##########
    
    #Find Max Values
      maxLoadIndex <- which.max(data$Load)
      extensionAtMaxLoad <- data$Extension[maxLoadIndex]
      angleAtMaxLoad <- data$Angle[maxLoadIndex]
      maxLoad <- data$Load[maxLoadIndex]
      
      maxExtensionIndex <- which.max(data$Extension)
      maxExtension <- data$Extension[maxExtensionIndex]
      loadAtMaxExt <- data$Load[maxExtensionIndex]
      angleAtMaxExt <- data$Angle[maxExtensionIndex]
    
    
      # this is because we will pass 5 on the way up and down and we 
      # want values on the way up
      # dataLoading <- data[c(1:maxExtensionIndex),] # for cyclic
      
      #Find values at set displacement
      setDispIndex <- which(abs(data$Extension - setDisp) == min(abs(data$Extension - setDisp)))
      loadAtSetDisp <- data$Load[setDispIndex]
      extensionAtSetDisp <- data$Extension[setDispIndex]
      angleAtSetDisp <- data$Angle[setDispIndex]
      
    #Find values at set Load
      setLoadIndex <- which(abs(data$Load - setLoad) == min(abs(data$Load - setLoad)))
      dispAtSetLoad <- data$Extension[setLoadIndex]
      LoadAtSetLoad <- data$Extension[setLoadIndex]
      angleAtSetLoad <- data$Angle[setLoadIndex]
      
    #Data Subsets
      #### uncomment if you want break data
      # dataToBreak = data[c(1:breakIndex),]
      # dataToDeltaSlope = data[c(1:deltaSlopeIndex),]
      ####
      
      dataToMaxLoad = data[c(1:maxLoadIndex),]
      dataToMaxExtension = data[c(1:maxExtensionIndex),]
      dataToSetDisp = data[c(1:setDispIndex),]
      dataToSetLoad = data[c(1:setLoadIndex),]
    
    #Work Calculations
      #### uncomment if you want break data
      # workToBreak <- trapz(dataToBreak$Extension, dataToBreak$Load)
      # workToDeltaSlope <- trapz(dataToDeltaSlope$Extension, dataToDeltaSlope$Load)
      ####
      workToMaxLoad <- trapz(dataToMaxLoad$Extension, dataToMaxLoad$Load)
      workToMaxExtension <- trapz(dataToMaxExtension$Extension, dataToMaxExtension$Load)
      workToSetDisp <- trapz(dataToSetDisp$Extension, dataToSetDisp$Load)
      workToSetLoad <- trapz(dataToSetLoad$Extension, dataToSetLoad$Load)
      totalWork <- trapz(data$Extension, data$Load)
    
      results <- data.frame(length, totalWork, 
                            maxLoad, angleAtMaxLoad, extensionAtMaxLoad, workToMaxLoad,
                            loadAtMaxExt, angleAtMaxExt, maxExtension, workToMaxExtension,
                            # loadAtBreak, angleAtBreak, ExtensionAtBreak, workToBreak,
                            # loadAtHoldStart, loadAtHoldEnd,
                            # loadAtDeltaSlope, angleAtDeltaSlope, ExtensionAtDeltaSlope, workToDeltaSlope,
                            loadAtSetDisp, extensionAtSetDisp, angleAtSetDisp, workToSetDisp,
                            dispAtSetLoad, LoadAtSetLoad, angleAtSetLoad, workToSetLoad)
    return(results)
  }
  