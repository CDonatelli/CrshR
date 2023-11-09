
  bend_3pt_Inst <- function(data, length){
    #pracma package required for trapz
    
    #data$Load <- abs(data$Load)
    data$h = sqrt((length/2)^2 + abs(data$Extension^2))
    data$Angle = rad2deg(asin(data$Extension * (sin(90)/data$h)))
    
    #Select break and change points
    x11() # for some reason identify wants its own window now
      plot(data$Time, data$Load, main = "Select the valley of your cycles")      
      userPts <- sapply(list(data$Time,data$Load),"[",identify(data$Time,data$Load, n=3))
    
    #Data Subsets
      cycle1Index <- which(userPts[1] == data$Time)
      cycle2Index <- which(userPts[2] == data$Time)
      cycle3Index <- which(userPts[3] == data$Time)
      cycle1 = data[c(1:cycle1Index),]
      cycle2 = data[c(cycle1Index:cycle2Index),]
      cycle3 = data[c(cycle2Index:cycle3Index),]
    
    #Find Max Values
      cycle1MaxIndex  <- which.max(cycle1$Load)
      cycle1MaxExt    <- cycle1$Extension[cycle1MaxIndex]
      cycle1MaxAngle  <- cycle1$Angle[cycle1MaxIndex]
      cycle1maxLoad   <- cycle1$Load[cycle1MaxIndex]
      
      cycle2MaxIndex  <- which.max(cycle2$Load)
      cycle2MaxExt    <- cycle2$Extension[cycle2MaxIndex]
      cycle2MaxAngle  <- cycle2$Angle[cycle2MaxIndex]
      cycle2maxLoad   <- cycle2$Load[cycle2MaxIndex]
      
      cycle3MaxIndex  <- which.max(cycle3$Load)
      cycle3MaxExt    <- cycle3$Extension[cycle3MaxIndex]
      cycle3MaxAngle  <- cycle3$Angle[cycle3MaxIndex]
      cycle3maxLoad   <- cycle3$Load[cycle3MaxIndex]
    
    #Work Calculations
      # workToBreak <- trapz(dataToBreak$Extension, dataToBreak$Load)
      cycle1Work <- trapz(cycle1$Extension, cycle1$Load)
      cycle2Work <- trapz(cycle2$Extension, cycle2$Load)
      cycle3Work <- trapz(cycle3$Extension, cycle3$Load)
    
      results <- data.frame(
                            cycle1maxLoad, cycle1MaxAngle, cycle1MaxExt, cycle1Work,
                            cycle2maxLoad, cycle2MaxAngle, cycle2MaxExt, cycle2Work,
                            cycle3maxLoad, cycle3MaxAngle, cycle3MaxExt, cycle3Work
                                                                                    )
    return(results)
  }
  