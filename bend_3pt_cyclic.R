
  bend_3pt_cyclic <- function(data, length, cycles){
    #pracma package required for trapz
    
    #data$Load <- abs(data$Load)
    data$h = sqrt((length/2)^2 + abs(data$Extension^2))
    data$Angle = rad2deg(asin(data$Extension * (sin(90)/data$h)))
    
    #Select break and change points
    x11() # for some reason identify wants its own window now
      plot(data$Time,data$Load, main = "Select the start point then the valleys between cycles.")
      userPts <- sapply(list(data$Time,data$Load),"[",identify(data$Time,data$Load, n=cycles+1))
    
    cycleList = 
    #Data Subsets
    for (i in 1:cycles){
      start = which(userPts[i] == data$Time)
      end = which(userPts[i+1] == data$Time)
      assign(paste0("cycle",i), data[c(start:end),])
    }
      
      # dataToBreak = data[c(1:breakIndex),]
      dataToMaxLoad = data[c(1:maxLoadIndex),]
      dataToDeltaSlope = data[c(1:deltaSlopeIndex),]
      dataToMaxExtension = data[c(1:maxExtensionIndex),]
      dataToSetDisp = data[c(1:setDispIndex),]
      
      deltaSlopeIndex <- which(userPts[1] == data$Time)
      ExtensionAtDeltaSlope = data$Extension[deltaSlopeIndex]
      loadAtDeltaSlope = data$Load[deltaSlopeIndex]
      angleAtDeltaSlope = data$Angle[deltaSlopeIndex]
    
    #Select hold time
      # plot(data$Time, data$Extension, main = "Select hold start, then hold stop, then click 'finish.'")
      # hold <- sapply(list(data$Time,data$Extension),"[",identify(data$Time,data$Extension))
      holdStartIndex <- 
      loadAtHoldStart = data$Load[holdStartIndex]
      
      holdEndIndex <- which(userPts[3] == data$Time)
      loadAtHoldEnd = data$Load[holdEndIndex]
    
    #Find Max Values
      maxLoadIndex <- which.max(data$Load)
      extensionAtMaxLoad <- data$Extension[maxLoadIndex]
      angleAtMaxLoad <- data$Angle[maxLoadIndex]
      maxLoad <- data$Load[maxLoadIndex]
      
      maxExtensionIndex <- which.max(data$Extension)
      maxExtension <- data$Extension[maxExtensionIndex]
      loadAtMaxExt <- data$Load[maxExtensionIndex]
      angleAtMaxExt <- data$Angle[maxExtensionIndex]
    
    #Find values at set displacement (5mm in this case)
      # this is because we will pass 5 on the way up and down and we 
      # want values on the way up
      dataLoading <- data[c(1:maxExtensionIndex),] 
      setDispIndex <- which(abs(dataLoading$Extension - 5) == min(abs(dataLoading$Extension - 5)))
      loadAtSetDisp <- data$Load[setDispIndex]
      extensionAtSetDisp <- data$Extension[setDispIndex]
      angleAtSetDisp <- data$Angle[setDispIndex]
    
    #Work Calculations
      # workToBreak <- trapz(dataToBreak$Extension, dataToBreak$Load)
      workToDeltaSlope <- trapz(dataToDeltaSlope$Extension, dataToDeltaSlope$Load)
      workToMaxLoad <- trapz(dataToMaxLoad$Extension, dataToMaxLoad$Load)
      workToMaxExtension <- trapz(dataToMaxExtension$Extension, dataToMaxExtension$Load)
      workToSetDisp <- trapz(dataToSetDisp$Extension, dataToSetDisp$Load)
      totalWork <- trapz(data$Extension, data$Load)
    
      results <- data.frame(length, totalWork, 
                            maxLoad, angleAtMaxLoad, extensionAtMaxLoad, workToMaxLoad,
                            loadAtMaxExt, angleAtMaxExt, maxExtension, workToMaxExtension,
                            # loadAtBreak, angleAtBreak, ExtensionAtBreak, workToBreak,
                            loadAtHoldStart, loadAtHoldEnd,
                            loadAtDeltaSlope, angleAtDeltaSlope, ExtensionAtDeltaSlope, workToDeltaSlope,
                            loadAtSetDisp, extensionAtSetDisp, angleAtSetDisp, workToSetDisp)
    return(results)
  }
  