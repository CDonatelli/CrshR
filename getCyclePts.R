getCyclePts <- function(data, cycles){

  #Select break and change points
  x11() # for some reason identify wants its own window now
  plot(data$Time,data$Load, main = "Select the start point then the valleys between cycles.")
  userPts <- sapply(list(data$Time,data$Load),"[",identify(data$Time,data$Load, n=cycles+1))
 
  return(userPts) 
}