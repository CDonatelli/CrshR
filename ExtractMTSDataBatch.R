library(readr)
library(ggplot2)
library(pracma)
library(segmented)

### Change this to the directory where you have your data files
setwd("G:/My Drive/Research/00. FishLab/04. Project Folders/FHL Summer 2025/09. Personal Folders/Brody - Buckling/02. Data/MTS Data/Individual Data Files")

#Read in files
files <- as.data.frame(dir(pattern = ".csv", full.names = TRUE, ignore.case = TRUE))
names(files)[1] <- 'files'
files$files<- as.character(files$files)
finalResult <- data.frame()

### Change this to equal the span of your 3pt bending rig (mm)
#length = 50

### Change this to get values at a specific displacement and load
setDisp = 5
setLoad = 3
cycles = 3

for (i in 1:nrow(files)){
  #data <- read.csv(files$files[i])
  data <- read_csv(files$files[i])
  name = files$files[i]
  data = data[-1,]
  result <- crush_cyclic2(data, cycles, setDisp, setLoad, name)
  finalResult <- rbind(finalResult, result)
  #readline(prompt="Press [enter] to continue")
}

row.names(finalResult) <- files$files
#row.names(finalResult) <- files[2:5,]

### Change this to a file name that makes sense
fileName = "testDemoThing.csv"

write.csv(finalResult, fileName, row.names = TRUE)
