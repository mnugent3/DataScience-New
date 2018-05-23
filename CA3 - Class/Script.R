library(car)
scatter.smooth(x = cars$speed, y = cars$dist, main = "Dist ~ Speed") #main = Title of plot

#normality of data
install.packages("e1071")
library(e1071)

#divide graph area in 2 columns
par(mfrow = c(1, 2))

#density plot for 'speed'
plot(density(cars$speed), main = "Density Plot: Speed", ylab = "Frequency", sub = paste("Skewness:", round(e1071::skewness(cars$speed), 2))) # sub = subtitle
polygon(density(cars$speed), col = "red")

#density plot for 'dist'
plot(density(cars$dist), main = "Density Plot: Distance", ylab = "Frequency", sub = paste("Skewness:", round(e1071::skewness(cars$dist), 2))) # sub = subtitle
polygon(density(cars$dist), col = "red")

#calculate correlation between speed and distance
cor(cars$speed, cars$dist) # positive correlation result = 0.89

#build linear regression model on full data
linearMod <- lm(dist ~ speed, data = cars)
print(linearMod)

#dist = Intercept + (B * speed)
#dist= -17.579 + 3.932

#model summary to see how accurate the model is going to be
summary(linearMod)

#***the more stars the most statistically significant the variable is and the final p value is an indicator 
#Pr( > | t | )
#0.0123 *
#1.49e-12 ** *

#how good the model is for the null hypothesis
#p - value:1.49e-12

# measures of the goodness of fit of an estimated statistical model,
#can be used for model selection
AIC(linearMod) 
BIC(linearMod)

#Both criteria depend on the maximized value of the ilkelihood of function L for the estimated model.
#For model comparison, the model with the lowest AIC and BIC score is preferred.

#Predicting Linear Models
#Split 80:20 (80 for sample and build model, 20 for testing the model)

#sample chooses a random sample
#from 1:all records from cards, 80% of rows
no_of_records <- sample(1:nrow(cars), 0.8 * nrow(cars))
#model training data
training_data <- cars[no_of_records, ] #80%
#test data
testing_data <- cars[-no_of_records,] #20% or whatever is left of the dataset

#Build the model on the training data
lr_model <- lm(dist ~ speed, data = training_data)

#predict distance from testing data
dist_predicted <- predict(lr_model, testing_data)

summary(lr_model)

#make actual-predicteds dataframe.
actuals_preds <- data.frame(cbind(actuals = testing_data$dist, predicted = dist_predicted))
head(actuals_preds)

#Calculate correlation accuracy
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

#correlation_accuracy Results
#actuals predicted
#actuals 1.0000000 0.6781012 - 67.8% correlation accuracy
#predicted 0.6781012 1.0000000

#Min- max accuracy #difference in range between actuals and predicted
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy

#MAPE
mape <- mean(abs((actuals_preds$predicted - actuals_preds$actuals)) / actuals_preds$actuals)
mape

#ARIMA

# ts data
# Daily Closing Prices of Major European Stock Indices, 1991 --1998
ts_data <- EuStockMarkets[, 1]
opar <- par()
par(mfrow = c(1, 2))

# use type = "additive for additive components"
decomposed_result <- decompose(ts_data, type = "mult")
plot(decomposed_result)
decomposed_result <- decompose(ts_data, type = "additive")
plot(decomposed_result)
seasonal_trend_error <- stl(ts_data, s.window = "periodic")
par <- opar
#Examine first few rows of time series
seasonal_trend_error$time.series

# How to create lags of a time-series 
# shifted 3 periods earlier. Use '-3' to shift by 3 periods forward.
lagged_ts <- lag(ts_data, 3)


install.packages("DataCombine")
library(DataCombine)
#Create a data frame with 1 lag and 1 lead value
my_dataframe <- as.data.frame(ts_data)
#create lag1 variable
my_dataframe <- slide(my_dataframe, "x", NewVar = "xLag1", slideBy = -1)
# create lead1 variable
my_dataframe <- slide(my_dataframe, "x", NewVar = "xLead1", slideBy = 1)
head(my_dataframe)


# acf() generates plot by default
acf_res <- acf(AirPassengers) # autocorrelation

#partial autcorrelation
pacf_res <- pacf(AirPassengers)

# de-trending a time series
# This example uses the Johnson & Johnson dataset
# Recall that lm() used to build linear model
plot(JohnsonJohnson)
trained_model <- lm(JohnsonJohnson ~ c(l:length(JohnsonJohnson)))
# resid(trained_model) contains the de-trended series.
plot(resid(trained_model), type = "l")

#De-seasonalise a time series
install.packages("forecast")
library(forecast)
#decompose the time series
#either the character string "periodic" or the span (in lags)
#of the loess window for seasonal extraction is entered in function
? stl
ts_decompose <- stl(AirPassengers, "periodic")
#de-seasonalise the time series
ts_seasonal_adjust <- seasadj(ts_decompose)
#original series
plot(AirPassengers, type = "l")
#seasonal adjusted
plot(ts_seasonal_adjust, type = "l")

#seasonal frequency set as 12 for monthly data.
seasonplot(ts_seasonal_adjust, 12, col = rainbow)


library(tseries)
adf.test(ts_data)
?kpss.test
kpss.test(ts_data)

nsdiffs(AirPassengers) #tells us how much to go back.

AirPassengers_seasdiff <- diff(AirPassengers, lag = frequency(AirPassengers), differences = 1)
plot(AirPassengers_seasdiff, type = "l", main = "Seasonally Differenced")

adf.test(AirPassengers_seasdiff)
kpss.test(AirPassengers_seasdiff)
nsdiffs(AirPassengers_seasdiff)

library(forecast)
library(tseries)
plot(Nile)
ndiffs(Nile)

d_nile <- diff(Nile)
plot(d_nile)
ndiffs(d_nile)

adf.test(d_nile)

# p, d and q 
Acf(d_nile)
Pacf(d_nile)

library(forecast)
fit <- Arima(Nile, order = c(0, 1, 1))
fit
accuracy(fit) #MAPE = prediction accuracy, 13% from zero

help("qqnorm")
qqnorm(fit$residuals)
qqline(fit$residuals)

Box.test(fit$residuals, type = "Ljung-Box")

forecast(fit, 3) # lo 80 = lo 80% Confidence

fit <- auto.arima(Nile)
fit