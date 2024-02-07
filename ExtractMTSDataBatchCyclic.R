library(readr)
library(ggplot2)
library(pracma)

#Read in files
files <- as.data.frame(dir(pattern = ".csv", full.names = TRUE, ignore.case = TRUE))
names(files)[1] <- 'files'
files$files<- as.character(files$files)
finalResult <- data.frame()

### Change this to equal the span of your rig
  # Distance between arms for 3-pt bending
  # Distance from gripper to load cell for canteliever bending
  # Distance between grippers for torsion

length = 50
cycles = 3


### Use this if timing is the same for all trials
#data <- read.csv(files$files[1])
#userPts <- getCyclePts(data, cycles)

for (i in 1:nrow(files)){
  data <- read.csv(files$files[i])
  ### Use this if timing is different for some trials
  #userPts <- getCyclePts(data, cycles)
  result <- bend_3pt_cyclic(data, length, cycles, userPts)
  finalResult <- rbind(finalResult, result)
}

row.names(finalResult) <- files$files
#row.names(finalResult) <- files[2:5,]


### Change this to a file name that makes sense
fileName = "testDemoThing.csv"

write.csv(finalResult, fileName, row.names = TRUE)
