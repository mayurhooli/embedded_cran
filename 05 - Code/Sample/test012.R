##Read data from training data set
newdf <- read.csv("./Variables.csv", na.strings=c("","NA"))
##Training data in newdf


##Remove Empty Columns
blanks <- c()
blanks <- sapply(newdf, function(x) sum(is.na(x)))
deleteval <- c()
for(i in 1:230){
  j <- 0
  if(blanks[i] > 10000){
    deleteval <- c(deleteval, i)
  }
}
newdf <- newdf[-c(deleteval)]
##Removed Columns whose NULL values are greater than 80%


write.csv(newdf,"./Variables3.csv")
newdf <- read.csv("./Variables3.csv", na.strings=c("","NA"))


##Remove Empty Rows
blanks2 <- c()
blanks2 <- apply(newdf, 1, function(x) sum(is.na(x)))
deleteval2 <- c()
for(i in 1:50000){
  if(blanks2[i] > 8){
    deleteval2 <- c(deleteval2, i)
  }
}
newdf <- newdf[-c(deleteval2),]
##Removed Rows whose NULL values are greater than 85%


##Create new Data Frame
outdf <- newdf[,68]
newdf <- newdf[-c(68)]
newdf <- newdf[-c(1)]
##New Data Frames Created

getmode <- function(x){
  uni <- unique(x, exclude=NULL)
  uni[which.max(tabulate(match(x, uni)))]
}

##Get Mode
for(i in 1:66){
  newdf[,i][is.na(newdf[,i])] <- getmode(newdf[,i])
}
##Calculate the Mode of all remaining columns and replace NULL by Mode

newdf <- newdf[-c(57)]

##Convert string to integer
for(i in 39:65)
  newdf[,i] <- as.numeric(newdf[,i])
##All strings converted to Integer values


##Convert to Binary
other<-!(outdf %in% c(1))
outdf[other] <- 0
##


##Copy Data into Training and Testing Data Frames
k <- nrow(newdf)
train <- newdf[1:(k-10000),]
outdf1 <- outdf[1:(k-10000)]
test <- newdf[(k-9999):k,]
outdf2 <- outdf[(k-9999):k]
##


##Perform Logistic Regression
tr_ch <- glm(outdf ~ ., data=newdf, family="binomial")
train_ch <- glm(outdf1 ~ ., data=train, family="binomial")
train_ch2 <- glm(outdf2 ~ ., data=test, family="binomial")



##
##Testing Data
##

##Predict Using the model developed Earlier
newdata1 <- with(train, data.frame(test))
test_ch <- predict(train_ch, newdata=newdata1, type="response")


##Check the Plot
plot(test_ch)


##Find the Mean Value and add 0.5 for Error correction
mean(test_ch)

##Use Floor value function to get 0 or 1
outdf3 <- test_ch + 0.8745
outdf4 <- floor(outdf3)

##Find difference of Test output and predicted output
outdf5 <- outdf4 - outdf2
values <- table(outdf5)

##This model gives an accuracy of 86.59% on the test data set