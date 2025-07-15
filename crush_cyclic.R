
  crush_cyclic <- function(data, cycles, setDisp, setLoad){
    data$Load <- abs(data$Load)
    
    ### Step 1 is to break data up by cycles
      n <- nrow(data)
      split_data <- split(data, cut(1:n, breaks=3, labels=FALSE))
      
      # Each element in split_data is a data frame representing a cycle
      cycle1 <- split_data[[1]]
      cycle2 <- split_data[[2]]
      cycle3 <- split_data[[3]]
    
    ### Next we will determine the linear region for cycle 1
      # Initial linear model
      n2 <- nrow(cycle1)
      split_data2 <- split(cycle1, cut(1:n2, breaks=2, labels=FALSE))
      cycle1Loading <- split_data2[[1]]
      lm1 <- lm(Load ~ Extension, data = cycle1Loading)
      
      # plot(cycle1Loading$Extension, cycle1Loading$Load, main = "Select Estimate of Linear region end")
      # userPts <- sapply(list(cycle1Loading$Extension, cycle1Loading$Load),"[",
      #                   identify(cycle1Loading$Extension, cycle1Loading$Load, n=2))
      # Index1 <- which(userPts[1] == cycle1Loading$Extension)
      # Index2 <- which(userPts[2] == cycle1Loading$Extension)
      
      # Add a breakpoint (you can guess a starting value if needed)
      n3 = nrow(cycle1Loading)/3
      initial_psi <- cycle1Loading$Extension[c(n3, n3+n3)]
      seg_fit <- segmented(lm1, seg.Z = ~Extension, psi = list(Extension = initial_psi))  # guess initial psi
      
      # Plot to check if you want
      plot(cycle1Loading$Extension, cycle1Loading$Load, type = "l")
      plot(seg_fit, add = TRUE, col = "blue", lwd = 2)
      readline(prompt = "Press [Enter] to continue...")
      
      linearRegionSlope <- as.numeric(seg_fit$coefficients["Extension"])
      bpExtension = seg_fit$psi[1,"Est."]
      bp_index <- which.min(abs(cycle1Loading$Extension - bpExtension))
      linearRegionExt = cycle1Loading$Extension[bp_index]
      linearRegionLoad = cycle1Loading$Load[bp_index]
    
    # x11() # for some reason identify wants its own window now
    #   plot(data$Time, data$Load, main = "Select the Crack Point, hold start, hold stop.'")
    #   userPts <- sapply(list(data$Time,data$Load),"[",identify(data$Time,data$Load, n=3))
    #   crackIndex <- which(userPts[1] == data$Time)
    #   ExtensionAtCrack = data$Extension[crackIndex]
    #   loadAtCrack = data$Load[crackIndex]
    
    # #Hold Points
    #   holdStartIndex <- which(userPts[2] == data$Time)
    #   loadAtHoldStart = data$Load[holdStartIndex]
    #   holdEndIndex <- which(userPts[3] == data$Time)
    #   loadAtHoldEnd = data$Load[holdEndIndex]
      
      cycleList <- list(cycle1, cycle2, cycle3)
      
      for (i in 1:cycles){
        cycle = as.data.frame(cycleList[i])
        
        cycleMaxIndex  <- which.max(cycle$Load)
        cycleMaxExt    <- cycle$Extension[cycleMaxIndex]
        cycleMaxLoad   <- cycle$Load[cycleMaxIndex]
        cycleWorkTotal <- trapz(cycle$Extension, cycle$Load)
        
        dataToMaxLoad = cycle[c(1:cycleMaxIndex),]
        cycleWorkMaxLoad <- trapz(dataToMaxLoad$Extension, dataToMaxLoad$Load)
        
        dataMaxToEnd = cycle[c(cycleMaxIndex:nrow(cycle)),]
        cycleWorkReturn <- trapz(dataMaxToEnd$Extension, dataMaxToEnd$Load)
        
        assign(paste0("cycle",i,"MaxIndex"), cycleMaxIndex)
        assign(paste0("cycle",i,"MaxExt"), cycleMaxExt)
        assign(paste0("cycle",i,"MaxLoad"), cycleMaxLoad)
        assign(paste0("cycle",i,"WorkTotal"), cycleWorkTotal)
        assign(paste0("cycle",i,"WorkMaxLoad"), cycleWorkMaxLoad)
        assign(paste0("cycle",i,"WorkReturn"), cycleWorkReturn)
      }
      
      setDispIndex <- which.min(abs(cycle1Loading$Extension - setDisp))
      setLoadIndex <- which.min(abs(cycle1Loading$Load - setLoad))
      
      extAtSetDisp <- cycle1Loading$Extension[setDispIndex]
      loadAtSetDisp <- cycle1Loading$Load[setDispIndex]
      data2SetDisp = cycle1Loading[c(1:setDispIndex),]
      work2SetDisp = trapz(data2SetDisp$Extension, data2SetDisp$Load)
      
      extAtSetLoad <- cycle1Loading$Extension[setLoadIndex]
      loadAtSetLoad <- cycle1Loading$Load[setLoadIndex]
      data2SetLoad = cycle1Loading[c(1:setLoadIndex),]
      work2SetLoad = trapz(data2SetLoad$Extension, data2SetLoad$Load)
      
    
      results <- data.frame(bpExtension, linearRegionExt, linearRegionLoad, linearRegionSlope,
                            cycle1MaxExt, cycle1MaxLoad, cycle1WorkReturn, cycle1WorkTotal, cycle1WorkMaxLoad,
                            cycle2MaxExt, cycle2MaxLoad, cycle2WorkReturn, cycle2WorkTotal, cycle2WorkMaxLoad,
                            cycle3MaxExt, cycle3MaxLoad, cycle3WorkReturn, cycle3WorkTotal, cycle3WorkMaxLoad,
                            extAtSetDisp, loadAtSetDisp, work2SetDisp,
                            extAtSetLoad, loadAtSetLoad, work2SetLoad)
    return(results)
  }