library(readr)
library(ggplot2)
library(pracma)

#Read in files
files <- as.data.frame(dir(pattern = ".csv", full.names = TRUE, ignore.case = TRUE))
names(files)[1] <- 'files'
files$files<- as.character(files$files)
finalResult <- data.frame()

### Change this to the length and width of your dogbone (mm)
width = 20
thickness = 1
CSA = thickness*width
origalLen = 75

for (i in 1:nrow(files)){
  data <- read.csv(files$files[i])
  result <- tensile(data, CSA, origalLen)
  finalResult <- rbind(finalResult, result)
  #readline(prompt="Press [enter] to continue")
}

row.names(finalResult) <- files$files
#row.names(finalResult) <- files[2:5,]

### Change this to a file name that makes sense
fileName = "processedData.csv"

write.csv(finalResult, fileName, row.names = TRUE)
