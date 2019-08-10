library(ggplot2)
library(maps)

install.packages("ggmap")
library(ggmap)

library(caTools)

statesMap = map_data("state")
str(statesMap)

table(statesMap$group)

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")
polling <- read.csv("PollingImputed.csv")

str(polling)

train <- subset(polling, Year!=2012)
test <- subset(polling, Year==2012)

mod2 = glm(Republican~SurveyUSA+DiffCount, data=train, family="binomial")
TestPrediction = predict(mod2, newdata=test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)

predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, test$State)
table(predictionDataFrame$test.State ==1)

mean(TestPrediction)
predictionDataFrame

predictionDataFrame$region <- tolower(predictionDataFrame$test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]

nrow(predictionMap)

nrow(statesMap)

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
