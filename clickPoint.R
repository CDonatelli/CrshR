clickPoint <- function(data){
  # This function will output max load with associated work and extension 
  # as well as load, work, and extension at a user specified point
  #Select break point
  data$Extension = data$Extension + abs(data$Extension[1])
  
  plot(data$Extension, data$Load, 
       main = "Select the Crack Point then click 'finish.'",
       pch = 16, cex = 0.1)
  lines(data$Extension, data$Load, col = 'grey')
  
  userPoint <- sapply(list(data$Extension,data$Load),"[",identify(data$Extension,data$Load))
  userIndex <- which(userPoint[1] == data$Extension)
  ExtensionAtUserPoint = data$Extension[userIndex]
  userPointLoad = data$Load[userIndex]
  
  points(ExtensionAtUserPoint, userPointLoad, pch = 23, bg = "blue")
  
  #Find Max Values
  maxLoadIndex <- which.max(data$Load)
  extensionAtMaxLoad <- data$Extension[maxLoadIndex]
  maxLoad <- data$Load[maxLoadIndex]
  
  points(extensionAtMaxLoad, maxLoad, pch = 23, bg = 'red')
  
  maxExtensionIndex <- which.max(data$Extension)
  maxExtension <- data$Extension[maxExtensionIndex]
  loadAtMaxExt <- data$Load[maxExtensionIndex]
  
  #Data Subsets
  dataToMaxLoad = data[c(1:maxLoadIndex),]
  dataToUserPoint = data[c(1:userIndex),]
  
  #Work Calculations
  workToMaxLoad <- trapz(dataToMaxLoad$Extension, dataToMaxLoad$Load)
  workTosUserPoint <- trapz(dataToUserPoint$Extension, dataToUserPoint$Load)
  
  results <- data.frame(maxLoad, extensionAtMaxLoad, maxExtension, loadAtMaxExt, workToMaxLoad,
                        userPointLoad, ExtensionAtUserPoint, workTosUserPoint)
  return(results)
}