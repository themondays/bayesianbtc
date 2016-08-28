# Simple Bitcoin Forecast implementation based on Bayesian regrssion #
# Reason: internal researches
# Author: Jared Denisov jd@themondays.ca #

library("bsts")
library("forecast")

# Settings
train <- read.csv("data/btc/historical.csv")
start_date <- c(2013,4,28)
train_start_line <- 2
train_end_line <- nrow(train)
head(train)
# tail(train)
train$Date <- as.Date(train$Date, format='%b %d, %Y')

train$Volume <- gsub(",", "", train$Volume)
train$Market.Cap <- gsub(",", "", train$Market.Cap)
train$Market.Cap <- as.numeric(train$Market.Cap)
train$Volume <- as.numeric(train$Volume)

# Daily differences
diffs <- matrix(c(0), nrow = 0, ncol = 1)
for (i in 1:nrow(train)) {
  diffs <- rbind(diffs, train[i,3] - train[i,4])
  i <- i + 1
}

train <- cbind(train,diffs)

# Damn, data not flat
# Manage with NA values
avg1 <- round(mean(train$Volume[train$diffs < 50], na.rm = TRUE), digits = 2)
avg2 <- round(mean(train$Volume[train$diffs > 50], na.rm = TRUE), digits = 2)
for (i in 1:nrow(train)) {
  if (is.na(train[i, 6])) {
    if (train$diffs[i] < 50) {
      train$Volume[i] <- avg1
    } else {
      train$Volume[i] <- avg2
    }
  }
}

# Extensible time series #
Train <- xts(train[, -1], order.by = as.POSIXct(train$Date))

# Linear models
sspecs <- AddLocalLinearTrend(list(), Train[,4])
sspecs <- AddSeasonal(sspecs, Train[,4], nseasons = 365.25)

# Bayesian Structural Time Series #
bayesian <- bsts(Train[,4], state.specification = sspecs, niter = 20)
plot(bayesian, ylim = c(0,1000)) 

brprediction <- predict(bayesian, horizon = 10)

brprediction$mean

layer1 <- bsts(Close ~ ., state.specification = sspecs,
               niter = 20,
               data = as.data.frame(Train))
layer2 <- bsts(Close ~ ., state.specification = sspecs,
               niter = 20,
               data = as.data.frame(Train),
               expected.model.size = 20)
layer3 <- bsts(Close ~ ., state.specification = sspecs,
               niter = 20,
               data = as.data.frame(Train),
               expected.model.size = 20)
CompareBstsModels(list("Main" = bayesian, "Layer M1" = layer1, "Layer M2" = layer2, "Layer M3" = layer3), colors = c("red", "green", "blue", "black"))
