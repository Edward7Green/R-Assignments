install.packages("readxl")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("forecast")

library("readxl")
SectorData<-read_excel("Energy_modified.xlsx")
SectorDataTS <- ts(SectorData, start=1973, frequency=12)

library(ggplot2)
library(ggfortify)
library(forecast)
autoplot(SectorDataTS, ts.colour = 'blue', xlab = "Month", ylab = "Trillion Btu")
ggseasonplot(SectorDataTS[,"Energy Consumption by the Residential Sector"], season.labels = NULL, year.labels = FALSE, year.labels.left = FALSE)
#Polar Version
ggseasonplot(SectorDataTS[,"Energy Consumption by the Residential Sector"], polar=TRUE, season.labels = NULL, year.labels = FALSE, year.labels.left = FALSE)
#Seasonal subseries plots show where the data for each season are collected together in separate mini time plots.
ggsubseriesplot(SectorDataTS[,"Energy Consumption by the Residential Sector"])
#lag plot of energy consumption by the residential sector
gglagplot(SectorDataTS[,"Energy Consumption by the Residential Sector"], lags = 40, nrow = NULL, ncol = NULL)
#decompose the time series into trend component, seasonal component and random component.  
components.ts = decompose(SectorDataTS[,"Energy Consumption by the Residential Sector"])
plot(components.ts)
#remove the seasonal data from the original time series. We check the stationarity of the remaining part
#and difference the remaining data to make it to stationary. 
seasonal <- components.ts$seasonal
withoutSeasonal <- SectorDataTS[,"Energy Consumption by the Residential Sector"] - seasonal
plot(withoutSeasonal)
#difference the data until the time series is stationary
ndiffs(withoutSeasonal)
diff(withoutSeasonal)
#autocorrelation function (ACF) of energy consumption by the residential sector
withoutSeasonalAfterdiff <- diff(SectorDataTS[,"Energy Consumption by the Residential Sector"] - seasonal)
Acf(withoutSeasonalAfterdiff, plot=FALSE)
AIC(arima(withoutSeasonalAfterdiff, order=c(4, 0, 1)), arima(withoutSeasonalAfterdiff, order=c(4, 0, 5)))
#use auto.arima() to fit the time series
trainingModel <- auto.arima(SectorDataTS[1:541,"Energy Consumption by the Residential Sector"], stationary = FALSE, seasonal = TRUE)
#Test the training model using the data from February 2018 to September 2019. 
testModel <- Arima(SectorDataTS[542:561,"Energy Consumption by the Residential Sector"], model= trainingModel)
accuracy(testModel)
#Forecast the energy consumption energy consumption by the residential sector from October 2019 to September 2020.
trainingModel <- auto.arima(SectorDataTS[,"Energy Consumption by the Residential Sector"], stationary = FALSE, seasonal = TRUE)
forecast <- forecast(trainingModel, h=12)
plot(forecast)


