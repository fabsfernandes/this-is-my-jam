RowVar <- function(x) {
  rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
}

listOfTimesteps <- list()
for( i in 2:16 ) {
  file <- paste("/Users/fabiola/Desktop/BraSNAM2018/dataset/results/artist-bet-", i, sep="")
  file <- paste( file, ".csv", sep="")
  listOfTimesteps[[i-1]] <- read.csv(file)
}
  

betDataFrame <- data.frame(listOfTimesteps[[1]]$Id)
colnames(betDataFrame) <- c("artist")
for( i in 1:15 ) {
  timeStep <- listOfTimesteps[[i]]
  colName <- paste(i, ".bet", sep="")
  betDataFrame[colName] <- timeStep$betweenesscentrality
}
variance <- RowVar(betDataFrame[,-1])
betDataFrame["variance"] <- variance
betDataFrameOrdered <- betDataFrame[order(betDataFrame$variance, decreasing = TRUE),]