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
maxExt = 10

## Lateral: 10
## Dorsal: 5

### Use this if timing is the same for all trials ###
#data <- read.csv(files$files[1])
#userPts <- getCyclePts(data, cycles)
### Use this if timing is the same for all trials ###

for (i in 1:nrow(files)){
  ## data <- read.csv(files$files[i]) ## uncomment if your CSV is formatted properlu
  
  #### Use if you need to format headers ####
  data <- read.csv(files$files[i], skip = 3, header = F)
  colnames(data)<- c("Time","Load","Angle")
  #### Use if you need to format headers ####
  
  ### Use this if timing is different for some trials ###
  dataPts = nrow(data)
  cycleLength = dataPts/cycles
  userPts = floor(ppoints(cycles+1,1)*dataPts)
  userPts[1]=1
  userPts = data$Time[userPts]
  ### Use this if timing is different for some trials ###
  
  ### Change function here to match the type of data you have
  result <- torsion_cyclic_Dana(data, length, cycles, userPts)
  finalResult <- rbind(finalResult, result)
}

row.names(finalResult) <- files$files
#row.names(finalResult) <- files[2:5,]


### Change this to a file name that makes sense
fileName = "SturgeonTorsion_CorrForZero.csv"

write.csv(finalResult, fileName, row.names = TRUE)
