library(MASS)

bost <- Boston

head(bost)

mins <- apply(bost,2,min)
head(mins)

maxs<- apply(bost,2,max)

scaled <- as.data.frame( scale(bost,center = mins , scale = maxs - mins))
head(scaled)
 




library(neuralnet)

library(caTools)

bostsample <- sample.split(scaled,SplitRatio = 0.7)

traindata <- subset(scaled,bostsample = TRUE)
testdata <- subset(scaled,bostsample = FALSE)

n <- names(traindata)
n

f <- as.formula(paste("medv ~" , paste(n[!n %in% "medv"],collapse = "+"))) 
f

model.nn <- neuralnet(f,data = traindata,hidden = c(5,3),linear.output = TRUE)
summary(model.nn) 

plot(model.nn)

predict.nn <- compute(model.nn,testdata[1:13])

str(predict.nn)

truepredict <- predict.nn$net.result * (max(bost$medv) - min(bost$medv)) + min(bost$medv)

test.r <- testdata$medv * (max(bost$medv) - min(bost$medv)) + min(bost$medv)

MSE <- sum((test.r - truepredict)^2)/nrow(testdata)

MSE 

error.df <- data.frame(test.r,truepredict)

head(error.df)

library(ggplot2)

ggplot(error.df,aes(test.r,truepredict)) + geom_point() + stat_smooth()
