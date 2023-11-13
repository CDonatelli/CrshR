
  bend_3pt_cyclic <- function(data, length, cycles, userPts){
    #pracma package required for trapz
    
    #data$Load <- abs(data$Load)
    data$h = sqrt((length/2)^2 + abs(data$Extension^2))
    data$Angle = rad2deg(asin(data$Extension * (sin(90)/data$h)))
    
    cycleList = list()
    #Data Subsets
    for (i in 1:cycles){
      start = which(userPts[i] == data$Time)
      end = which(userPts[i+1] == data$Time)
      cycleList[[i]] = as.data.frame(assign(paste0("cycle",i), data[c(start:end),]))
    }
    
    for (i in 1:cycles){
      cycle = as.data.frame(cycleList[1])
      
      cycleMaxIndex  <- which.max(cycle$Load)
      cycleMaxExt    <- cycle$Extension[cycleMaxIndex]
      cycleMaxAngle  <- cycle$Angle[cycleMaxIndex]
      cycleMaxLoad   <- cycle$Load[cycleMaxIndex]
      cycleWork <- trapz(cycle$Extension, cycle$Load)
      
      assign(paste0("cycle",i,"MaxIndex"), cycleMaxIndex)
      assign(paste0("cycle",i,"MaxExt"), cycleMaxExt)
      assign(paste0("cycle",i,"MaxAngle"), cycleMaxAngle)
      assign(paste0("cycle",i,"MaxLoad"), cycleMaxLoad)
      assign(paste0("cycle",i,"Work"), cycleWork)
      
    }
    
    rm(cycles, cycle, cycleMaxAngle, cycleMaxExt, cycleMaxLoad, cycleWork,
       cycle1, cycle2, cycle3, cycleMaxIndex, cycleList)
    
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
  