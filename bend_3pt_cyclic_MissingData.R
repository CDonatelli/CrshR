
  bend_3pt_cyclic_MissingData <- function(data, length, cycles, userPts, maxExt){
    #pracma package required for trapz
    
    #data$Load <- abs(data$Load)
    h = sqrt((length/2)^2 + abs(maxExt^2))
    Angle = rad2deg(asin(maxExt * (sin(90)/h)))
    
    cycleList = list()
    #Data Subsets
    for (i in 1:cycles){
      start = which.min(abs(data$Time - userPts[i]))
      end = which.min(abs(data$Time - userPts[i+1]))
      cycleList[[i]] = as.data.frame(assign(paste0("cycle",i), data[c(start:end),]))
    }
    
    for (i in 1:cycles){
      cycle = as.data.frame(cycleList[i])
      
      cycleMaxIndex  <- which.max(cycle$Load)
      #cycleMaxExt    <- cycle$Extension[cycleMaxIndex]
      dataToMaxLoad = data[c(1:cycleMaxIndex),]
      dataToMaxLoad$Extension <- ppoints(nrow(dataToMaxLoad),1)*5
      
      #cycleMaxAngle  <- cycle$Angle[cycleMaxIndex]
      cycleMaxLoad   <- cycle$Load[cycleMaxIndex]
      cycleWork <- trapz(dataToMaxLoad$Extension, dataToMaxLoad$Load)
      
      assign(paste0("cycle",i,"MaxIndex"), cycleMaxIndex)
      #assign(paste0("cycle",i,"MaxExt"), cycleMaxExt)
      #assign(paste0("cycle",i,"MaxAngle"), cycleMaxAngle)
      assign(paste0("cycle",i,"MaxLoad"), cycleMaxLoad)
      assign(paste0("cycle",i,"Work"), cycleWork)
      
    }
    
    rm(cycles, cycle, cycleMaxLoad, cycleWork, cycle4, cycle5,
       cycle1, cycle2, cycle3, cycleMaxIndex, cycleList, dataToMaxLoad)
    
    all_variables = ls()
    matching_variables <- grep(".*cycle.*", all_variables, value = TRUE)
    
    all_values = list()
    for (variable_name in matching_variables) {
      variable_value <- get(variable_name)
      all_values = c(all_values, variable_value)
    }
    
    all_values = as.data.frame(all_values)
    colnames(all_values) <- matching_variables
    
    return(all_values)
  }
  