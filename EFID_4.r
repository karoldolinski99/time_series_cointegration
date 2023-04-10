library(aTSA)
library(dplyr)
library(urca)
library(zoo)

eursek <- read.csv('eursek_d.csv')
eursek <- cbind.data.frame(eursek$Data, eursek$Zamkniecie)
colnames(eursek) <- c('Date', 'Closing')
eurisk <- read.csv('eurisk_d.csv')
eurisk <- cbind.data.frame(eurisk$Data, eurisk$Zamkniecie)
colnames(eurisk) <- c('Date', 'Closing')
eurhuf <- read.csv('eurhuf_d.csv')
eurhuf <- cbind.data.frame(eurhuf$Data, eurhuf$Zamkniecie)
colnames(eurhuf) <- c('Date', 'Closing')

dataset <- merge(eursek, eurisk, by="Date", incomparables = NA, all.x = T, all.y = T)
dataset <- merge(dataset, eurhuf, by="Date", incomparables = NA, all.x = T, all.y = T)
colnames(dataset) <- c('Date', 'SEK', 'ISK', 'HUF')

for (i in 2:4){
  dataset[,i] <- na.approx(dataset[,i], rule=2)
}

# 1. stationarity
# Null hyphotesis: time series is not stationary
index <- list(dataset$SEK, dataset$ISK, dataset$HUF)
sapply(index, ur.df)

ur.df(dataset$SEK)
summary(ur.df(dataset$SEK))
ur.df(dataset$ISK)
summary(ur.df(dataset$ISK))
ur.df(dataset$HUF)
summary(ur.df(dataset$HUF))

# 2. LM model
model_sek_isk <- lm(SEK~ISK, data = dataset)
model_isk_sek <- lm(ISK~SEK, data = dataset)

model_sek_huf <- lm(SEK~HUF, data = dataset)
model_huf_sek <- lm(HUF~SEK, data = dataset)

model_isk_huf <- lm(ISK~HUF, data = dataset)
model_huf_isk <- lm(HUF~ISK, data = dataset)

residuals <- list(model_sek_isk$residuals, model_isk_sek$residuals,
                  model_sek_huf$residuals, model_huf_sek$residuals,
                  model_isk_huf$residuals, model_huf_isk$residuals)
sapply(residuals, ur.ers)

ur_ers_result <- as.data.frame(matrix(NA, length(residuals), 2))
colnames(ur_ers_result) <- c('TestStat', 'CValue')
for (i in 1:length(residuals)){
  ur_ers_result[i,1] <- summary(ur.ers(residuals[[i]]))@teststat
  ur_ers_result[i,2] <- summary(ur.ers(residuals[[i]]))@cval[2]
}
