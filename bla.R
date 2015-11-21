library(caret)
library(randomForest)

set.seed(1337)
trainraw<- read.csv("pml-training.csv")
n <- length(trainraw[,1])
trainset <- trainraw[ (runif(n) > 0) ,]
# get rid of NA cols
n <- length(trainset[1,])
bc <- numeric(n)
for ( i in 1:n ) {
  bc[i] <- sum(is.na(trainset[,i]))
}
nabegone <- ( bc > ( .9 * length(trainset[,1]) ) )
trimset <- trainset[,!nabegone]
# remove first 7 cols irrelevent for fitting
n <- length(trimset[1,])
trim2 <- trimset[,8:n]
ys <- trainset$classe
# remove factors, since they appear highly skewed
wat <- unname( unlist( lapply(trim2,class) ) )
whonums <- ( wat == "factor" )
factfree <- trim2[,!whonums]
#now split the cleaned up data into 70/30 training-validation sets
inTrain <- createDataPartition(trainset$classe, p = .7, list = FALSE)
trainMod <- factfree[inTrain,]
validMod <- factfree[-inTrain,]
ytrain <- ys[inTrain]
yvalid <- ys[-inTrain]
# call train function
choochoo <- train( x=trainMod,y=ytrain ,preProcess = "pca")
# validate stuff
validationPredRF <- predict(choochoo, validMod)
# SHOW ME WHAT YOU GOT
check <- ( as.character(ys[-inTrain]) == as.character(validationPredRF) )
sum(check)/length(check) 


pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

testset <- read.csv("pml-testing.csv")
n <- length(testset[1,])
bc <- numeric(n)
for ( i in 1:n ) {
  bc[i] <- sum(is.na(trainset[,i]))
}
testnabegone <- ( bc > ( .9 * length(trainset[,1]) ) )
testtrimset <- testset[,!nabegone]
# remove first 7 cols irrelevent for fitting
n <- length(testtrimset[1,])
testtrim2 <- testtrimset[,8:n]
# remove factors, since they appear highly skewed
wat2 <- unname( unlist( lapply(testtrim2,class) ) )
whonums2 <- ( wat == "factor" )
factfree2 <- testtrim2[,!whonums]
ans <- predict(choochoo,factfree2)
pml_write_files(ans)