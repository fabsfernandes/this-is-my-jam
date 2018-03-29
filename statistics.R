RowVar <- function(x) {
  rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
}

RowAvg <- function(x) {
  rowMeans(x, na.rm = FALSE, dims = 1)
}

listOfTimesteps <- list()
for( i in 2:16 ) {
  file <- paste("/Users/fabiola/Desktop/BraSNAM2018/dataset/results/artist-bet-", i, sep="")
  file <- paste( file, ".csv", sep="")
  listOfTimesteps[[i-1]] <- read.csv(file)
}
  
### betweenness variance
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
log10betArt <- log10(betDataFrameOrdered$variance)

### closeness variance
cloDataFrame <- data.frame(listOfTimesteps[[1]]$Id)
colnames(cloDataFrame) <- c("artist")
for( i in 1:15 ) {
  timeStep <- listOfTimesteps[[i]]
  colNameClo <- paste(i, ".clo", sep="")
  cloDataFrame[colNameClo] <- timeStep$closnesscentrality
}
varianceClo <- RowVar(cloDataFrame[,-1])
cloDataFrame["variance"] <- varianceClo
cloDataFrameOrdered <- cloDataFrame[order(cloDataFrame$variance, decreasing = TRUE),]
log10cloArt <- log10(cloDataFrameOrdered$variance)

### pageRank variance
prDataFrame <- data.frame(listOfTimesteps[[1]]$Id)
colnames(prDataFrame) <- c("artist")
for( i in 1:15 ) {
  timeStep <- listOfTimesteps[[i]]
  colNamePr <- paste(i, ".pr", sep="")
  prDataFrame[colNamePr] <- timeStep$pageranks
}
variancePr <- RowVar(prDataFrame[,-1])
prDataFrame["variance"] <- variancePr
prDataFrameOrdered <- prDataFrame[order(prDataFrame$variance, decreasing = TRUE),]
log10prArt <- log10(prDataFrameOrdered$variance)

## plot graph
pdf("histogram-art.pdf", width=8.5, height=5.5)
par(mfrow=c(2,3))
hist(log10betArt, xlab="vari\u00E2ncia (log10)", ylab="frequ\u00EAncia", main="Ponte", col="blue2")
hist(log10prArt, xlab="vari\u00E2ncia (log10)", ylab="frequ\u00EAncia", main="Influ\u00EAncia", col="darkcyan")
hist(log10cloArt, xlab="vari\u00E2ncia (log10)", ylab="frequ\u00EAncia", main="Versatilidade", col="chocolate3")

boxplot(betDataFrameOrdered$variance,ylab="vari\u00E2ncia", main="Ponte")
boxplot(prDataFrameOrdered$variance,ylab="vari\u00E2ncia", main="Influ\u00EAncia")
boxplot(cloDataFrameOrdered$variance,ylab="vari\u00E2ncia", main="Versatilidade")
dev.off()
