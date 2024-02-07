
  bend_3pt_Inst <- function(data, length, cycles, userPts){
    #pracma package required for trapz
    
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
      cycleMaxAngle  <- cycle$Angle[cycleMaxIndex]
      cycleMaxLoad   <- cycle$Load[cycleMaxIndex]
      
      assign(paste0("cycle",i,"MaxIndex"), cycleMaxIndex)
      assign(paste0("cycle",i,"MaxAngle"), cycleMaxAngle)
      assign(paste0("cycle",i,"MaxLoad"), cycleMaxLoad)
      
    }
    
    rm(cycles, cycle, cycleMaxAngle, cycleMaxLoad, cycle1, cycle2, cycle3, 
       cycleMaxIndex, cycleList)
    
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
  