library(readr)
library(ggplot2)
library(pracma)

#Read in files
files <- as.data.frame(dir(pattern = ".csv", full.names = TRUE, ignore.case = TRUE))
names(files)[1] <- 'files'
files$files<- as.character(files$files)
finalResult <- data.frame()

### Change this to equal the span of your 3pt bending rig
length = 50

data <- read.csv(files$files[1])
userPts <- getCyclePts(data, 3)

for (i in 1:nrow(files)){
  data <- read.csv(files$files[i])
  result <- bend_3pt_cyclic(data, length, 3, userPts)
  finalResult <- rbind(finalResult, result)
}

row.names(finalResult) <- files$files
#row.names(finalResult) <- files[2:5,]


### Change this to a file name that makes sense
fileName = "testDemoThing.csv"

write.csv(finalResult, fileName, row.names = TRUE)