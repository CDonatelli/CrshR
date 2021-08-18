library(readr)
library(ggplot2)
library(pracma)

#Read in files
files <- as.data.frame(dir(pattern = ".csv", full.names = TRUE, ignore.case = TRUE))
names(files)[1] <- 'files'
files$files<- as.character(files$files)
finalResult <- data.frame()

for (i in 1:nrow(files)){
  data <- read.csv(files$files[i])
  result <- maxLoad(data)
  finalResult <- rbind(finalResult, result)
  readline(prompt="Press [enter] to continue")
}

row.names(finalResult) <- files$files
#row.names(finalResult) <- files[2:5,]

write.csv(finalResult,"finClipData.csv", row.names = TRUE)