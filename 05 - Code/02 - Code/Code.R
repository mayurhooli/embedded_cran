##Read data from training data set
newdf <- read.csv("D://Courses//CSCE691//Embedded CRAN Analytics//06 - Demo//02 - DFW KPIs//Sample-01.csv", na.strings=c("","NA"))
##Training data in newdf

##Remove Empty Columns
blanks <- c()
blanks <- sapply(newdf, function(x) sum(is.na(x)))
deleteval <- c()
for(i in 1:74){
  j <- 0
  if(blanks[i] > 1000000){
    deleteval <- c(deleteval, i)
  }
}
newdf <- newdf[-c(deleteval)]
##Removed Columns whose NULL values are greater than 80%

write.csv(newdf,"D://Sample-02.csv")
newdf <- read.csv("D://Sample-02.csv", na.strings=c("","NA"))

qw <- unique(newdf[4])

x <- list()
y <- data.frame()
z <- data.frame()
for(i in 1: 3661){
  newdf2 <- newdf[newdf$eNodeB == qw[i,],]
  x[[i]] <- newdf2
  newdf3 <- data.frame(nrow(newdf2),sum(is.na(newdf2)))
  y <- rbind(y, newdf3)
  newdf4 <- data.frame()
  for(j in 1:63){
    blanks3 <- sapply(newdf2, function(x) sum(is.na(x)))
    blanks4 <- blanks3*100/nrow(newdf2)
  }
  z <- rbind(z, blanks4)
}

colnames(z) <- colnames(newdf)

z2 <- z[c(20,27,34,37,39)]
z2 <- data.frame(qw, z2)
z3 <- z2[z2$Downlink.User.Throughput..Mbps. < 20,]
z3 <- z3[z3$MAC.DL.BLER.... < 20,]
z3 <- z3[z3$Avg.Number.of.RRC.Connected.Users < 20,]
z3 <- z3[z3$PUSCH.SINR < 20,]
z3 <- z3[z3$CQI < 20,]

w <- list()
for(i in 1:2){
  newdf5 <- newdf[c(4,20,27,34,37,39)]
  newdf5 <- newdf5[newdf5[i] == z3[i,1],]
}

write.csv(z, "D://sample-03.csv")

getmode <- function(x){
  uni <- unique(x, exclude=NULL)
  uni[which.max(tabulate(match(x, uni)))]
}
