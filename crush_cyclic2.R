crush_cyclic2 <- function(data, cycles, setDisp, setLoad, name){
    data$Load <- abs(data$Load)
    n <- nrow(data)
  
  # --- Split data into N cycles ---
    split_data <- split(data, cut(1:n, breaks = cycles, labels = FALSE))
    cycleList <- lapply(split_data, as.data.frame)
  
  # --- Determine linear region from loading portion of the first cycle ---
    cycle1 <- cycleList[[1]]
    n2 <- nrow(cycle1)
    split_data2 <- split(cycle1, cut(1:n2, breaks = 2, labels = FALSE))
    cycle1Loading <- split_data2[[1]]
    
    lm1 <- lm(Load ~ Extension, data = cycle1Loading)
    n3 <- floor(nrow(cycle1Loading) / 3)
    initial_psi <- cycle1Loading$Extension[c(n3, 2 * n3)]
    seg_fit <- segmented(lm1, seg.Z = ~Extension, psi = list(Extension = initial_psi))
  
    # Plot for visual check
      plot(cycle1Loading$Extension, cycle1Loading$Load, type = "l", 
           main = name)
      plot(seg_fit, add = TRUE, col = "blue", lwd = 2)
      readline(prompt = "Press [Enter] to continue...")
    
    # Extract linear region data
      linearRegionSlope <- slope(seg_fit)$Extension["slope1", "Est."]
      bpExtension <- seg_fit$psi[1,"Est."]
      bp_index <- which.min(abs(cycle1Loading$Extension - bpExtension))
      linearRegionExt <- cycle1Loading$Extension[bp_index]
      linearRegionLoad <- cycle1Loading$Load[bp_index]
  
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
    work2SetDisp <- trapz(cycle1Loading[1:setDispIndex, "Extension"], cycle1Loading[1:setDispIndex, "Load"])
    
    extAtSetLoad <- cycle1Loading$Extension[setLoadIndex]
    loadAtSetLoad <- cycle1Loading$Load[setLoadIndex]
    work2SetLoad <- trapz(cycle1Loading[1:setLoadIndex, "Extension"], cycle1Loading[1:setLoadIndex, "Load"])
    
  # --- Combine all results into one data frame ---
    cycle_values <- as.vector(t(cycleSummary))
    cycle_names <- outer(paste0("cycle", 1:cycles),names(cycleSummary), paste, sep = "_")
    colnames_flat <- as.vector(cycle_names)
    
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
    results <- cbind(results, setNames(as.data.frame(t(cycle_values)), colnames_flat))
  
  return(results)
}