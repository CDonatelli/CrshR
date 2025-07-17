crush_cyclic2 <- function(data, cycles, setDisp, setLoad, name){
    #data$Load <- abs(data$Load)
    n <- nrow(data)
  
  # --- Split data into N cycles ---
    plot(data$Time, data$Load, type = "l", main = "Click start, valleys, and end")
    internalValleys <- identify(data$Time, data$Load, n = cycles - 1)
    valleyIndices <- sort(c(1, internalValleys, nrow(data)))
    cycleList <- Map(function(start, end) data[start:end, ],
                     valleyIndices[-length(valleyIndices)],
                     valleyIndices[-1])
    
    #cycle1 = data[1:which(userPts[2] == data$Time), ]
    #cycle2 = data[which(userPts[2] == data$Time):which(userPts[3] == data$Time), ]
    #cycle3 = data[which(userPts[3] == data$Time):n, ]
    
    #split_data <- split(data, cut(1:n, breaks = cycles, labels = FALSE))
    #cycleList <- lapply(splitData, as.data.frame)
    #cycleList <- list(cycle1, cycle2, cycle3)
  
  # --- Determine linear region from loading portion of the first cycle ---
    cycle1 <- cycleList[[1]]
    n2 <- nrow(cycle1)
    split_data2 <- split(cycle1, cut(1:n2, breaks = 2, labels = FALSE))
    cycle1Loading <- split_data2[[1]]
    
    plot(cycle1Loading$Extension, cycle1Loading$Load, type = "l", main = name)
    userPts <- sapply(list(cycle1Loading$Extension, cycle1Loading$Load),"[",
                  identify(cycle1Loading$Extension, cycle1Loading$Load, n=2))
    startIndex <- which(userPts[1] == cycle1Loading$Extension)
    endIndex <- which(userPts[2] == cycle1Loading$Extension)
    cycle1LoadingSub = cycle1Loading[startIndex:endIndex, ]
    
    lm1 <- lm(Load ~ Extension, data = cycle1LoadingSub)
    abline(lm1, col = "blue", lwd = 2)
    # n3 <- floor(nrow(cycle1LoadingSub) / 5)
    # initial_psi <- cycle1LoadingSub$Extension[c(n3, 2*n3, 3*n3, 4*n3)]
    # seg_fit <- segmented(lm1, seg.Z = ~Extension, psi = list(Extension = initial_psi))
  
    # Plot for visual check
      # plot(seg_fit, add = TRUE, col = "blue", lwd = 2)
      readline(prompt = "Press [Enter] to continue...")
    
    # Extract linear region data
      # linearRegionSlope <- slope(seg_fit)$Extension["slope1", "Est."]
      # bpExtension <- seg_fit$psi[1,"Est."]
      # bp_index <- which.min(abs(cycle1Loading$Extension - bpExtension))
      # linearRegionExt <- cycle1Loading$Extension[bp_index]
      # linearRegionLoad <- cycle1Loading$Load[bp_index]
      linearRegionSlope <- lm1$coefficients[2]
      bpExtension <- cycle1Loading$Extension[endIndex]
      # bp_index <- which.min(abs(cycle1Loading$Extension - bpExtension))
      linearRegionExt <- cycle1Loading$Extension[endIndex]
      linearRegionLoad <- cycle1Loading$Load[endIndex]
  
  # --- Cycle calculaitons ---
    cycleSummary <- data.frame(
      MaxExt = numeric(cycles),
      MaxLoad = numeric(cycles),
      WorkTotal = numeric(cycles),
      WorkMaxLoad = numeric(cycles),
      WorkReturn = numeric(cycles)
    )
    
    for (i in 1:cycles) {
      cycle <- cycleList[[i]]
      
      cycleMaxIndex <- which.max(cycle$Load)
      cycleMaxExt <- cycle$Extension[cycleMaxIndex]
      cycleMaxLoad <- cycle$Load[cycleMaxIndex]
      cycleWorkTotal <- trapz(cycle$Extension, cycle$Load)
      
      dataToMaxLoad <- cycle[1:cycleMaxIndex, ]
      cycleWorkMaxLoad <- trapz(dataToMaxLoad$Extension, dataToMaxLoad$Load)
      
      dataMaxToEnd <- cycle[cycleMaxIndex:nrow(cycle), ]
      cycleWorkReturn <- trapz(dataMaxToEnd$Extension, dataMaxToEnd$Load)
      
      cycleSummary[i, ] <- c(cycleMaxExt, cycleMaxLoad, cycleWorkTotal, cycleWorkMaxLoad, cycleWorkReturn)
    }
  
  # --- Work to set points (from first cycle) ---
    setDispIndex <- which.min(abs(cycle1Loading$Extension - setDisp))
    setLoadIndex <- which.min(abs(cycle1Loading$Load - setLoad))
    
    extAtSetDisp <- cycle1Loading$Extension[setDispIndex]
    loadAtSetDisp <- cycle1Loading$Load[setDispIndex]
    data2setDisp <- cycle1Loading[1:setDispIndex,]
    work2SetDisp <- trapz(data2setDisp$Extension, data2setDisp$Load)
    
    extAtSetLoad <- cycle1Loading$Extension[setLoadIndex]
    loadAtSetLoad <- cycle1Loading$Load[setLoadIndex]
    data2setLoad <- cycle1Loading[1:setLoadIndex,]
    work2SetLoad <- trapz(data2setLoad$Extension, data2setLoad$Load)    
    
  # --- Combine all results into one data frame ---
    cycle_names <- as.vector(outer(paste0("cycle", 1:cycles), names(cycleSummary), paste, sep = "_"))
    cycle_values <- unlist(cycleSummary)
    names(cycle_values) <- cycle_names
    
    # --- Create final single-row data frame ---
    results <- data.frame(
      bpExtension = bpExtension,
      linearRegionExt = linearRegionExt,
      linearRegionLoad = linearRegionLoad,
      linearRegionSlope = linearRegionSlope,
      extAtSetDisp = extAtSetDisp,
      loadAtSetDisp = loadAtSetDisp,
      work2SetDisp = work2SetDisp,
      extAtSetLoad = extAtSetLoad,
      loadAtSetLoad = loadAtSetLoad,
      work2SetLoad = work2SetLoad
    )
    
    # Bind cycle values
    results <- cbind(results, as.data.frame(as.list(cycle_values)))
  
  return(results)
}