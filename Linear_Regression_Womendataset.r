#dataset contains 15 observations with 2 variables
?women
View(women)
#scatter plot is drawn to visulaize any linear relationship between the explanatory and the response variable. 
#This is a scatter plot along with the smoothing line
scatter.smooth(x=women$weight, y=women$height, main="Weight ~ Height")  # scatterplot
#Boxplots are mainly used to check for outliers.
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(women$weight, main="Weight", sub=paste("Outlier rows: ", boxplot.stats(women$weight)$out))  # box plot for 'speed'
boxplot(women$height, main="Height", sub=paste("Outlier rows: ", boxplot.stats(women$height)$out))  # box plot for 'distance'

#checking for croelation between the response and the explanatory variables 
cor(women$weight, women$height)
#Building a linear model. So far we have checked for outliers using box plot, and also checked for linearity using scatter plots and the smoothing line. 
#We also checked the corelation using the cor function
linearMod <- lm(height ~ weight, data=women)  # build linear regression model on full data
print(linearMod)
summary(linearMod)
#height =  25.723456  + 0.287249∗weight
## Create Training and Test data -

set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(women), 0.8*nrow(women))  # row indices for training data
trainingData <- women[trainingRowIndex, ]  # model training data
testData  <- women[-trainingRowIndex, ] 

lmMod <- lm(height ~ weight, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict height
summary (lmMod) 

actuals_preds <- data.frame(cbind(actuals=testData$height, predicteds=heightPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy
#head(actuals_preds)
#View(actuals_preds)