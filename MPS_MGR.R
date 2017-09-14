# Dane do pracy MGR
# Micha³ Siarkiewicz

# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("scales")
# install.packages("forecast")
# install.packages("lubridate")
# install.packages("plyr")

library(dplyr)
library(ggplot2)
library(scales)
library(forecast)
library(lubridate)
library(plyr)

# Wczytanie danych

dest <- "C:/Users/siarkmi2/OneDrive dla firm 1/NEW_WORLD_ORDER/SGH/MGR/BBreal_01.csv"
sales.raw <- read.csv2(dest)

sales <- tbl_df(sales.raw)

# zastapienie brakow danych warotœci¹ zero
sales[is.na(sales)] <- 0

sales$data <- date(sales$data)

# wstepna analiza

# stworzenie zmiennych tydzien i misiac dla obserwacji:
sales$Month <- as.Date(cut(sales$data,
                         breaks = "month"))
sales$Week <- as.Date(cut(sales$data,
                        breaks = "week",
                        start.on.monday = TRUE)) 



# wykres misieczny
ggplot(data = sales,
       aes(Month, sprzedaz)) +
        stat_summary(fun.y = mean, # sum adds up all observations for the month
                     geom = "line") + # "bar" "line"
        scale_x_date(
                date_labels = "%Y-%m",
                date_breaks = "1 month")  # custom x-axis labels


# wykres tygodniowy:
ggplot(data = sales,
       aes(Week, sprzedaz)) +
        stat_summary(fun.y = mean, # sum adds up all observations for the week
                     geom = "line") + # "bar" "line"
        scale_x_date(
                date_labels = "%Y",
                date_breaks = "1 week") # custom x-axis labels

# konwersja do klasy time series
sales.xts <-  xts(sales$sprzedaz, as.Date(sales$data, format='%Y-%m-%d'))

monthplot(sales.xts,ylab="$ million",xlab="Month",xaxt="n",
          main="Seasonal deviation plot: antidiabetic drug sales")
axis(1,at=1:12,labels=month.abb,cex=0.8)

##
sales.ts <- ts(sales.raw$sprzedaz,start=c(2015,1,1), end = c(2017,1,1),frequency=365)

plot.ts(sales.ts)


str(sales.ts)
class(sales.ts)

beer2 <- window(sales.ts)
beerfit1 <- meanf(beer2, h=11)
beerfit2 <- naive(beer2, h=11)
beerfit3 <- snaive(beer2, h=11)

plot(beerfit1, PI=FALSE,
     main="Forecasts for quarterly beer production")
lines(beerfit2$mean,col=2)
lines(beerfit3$mean,col=3)
legend("topright",lty=1,col=c(4,2,3),
       legend=c("Mean method","Naive method","Seasonal naive method"))


# dane tygodniowe

sales$week <- floor_date(sales$data,"week") +8
sales

x<-ddply(sales, .(week), function(z) mean(z$sprzedaz))

x.ts <- ts(x$V1, start=c(2015,1,1), end=c(2017,1,1),frequency=52)
x.ts
plot.ts(x.ts)

beer2 <- window(x.ts)
beerfit1 <- meanf(beer2, h=11)
beerfit2 <- naive(beer2, h=11)
beerfit3 <- snaive(beer2, h=11)


plot(beerfit1, PI=FALSE, main="Forecasts for quarterly beer production")
lines(beerfit2$mean,col=2)
lines(beerfit3$mean,col=3)
legend("topright",lty=1,col=c(4,2,3),
       legend=c("Mean method","Naive method","Seasonal naive method"))

# Evaluating forecast accuracy

beer2 <- window(x.ts,start=2015,end=2017-.1)

beerfit1 <- meanf(beer2,h=11)
beerfit2 <- rwf(beer2,h=11)
beerfit3 <- snaive(beer2,h=11)
fit4     <- rwf(beer2,drift=TRUE,h=11)


plot(beerfit1, PI=FALSE,
     main="Forecasts for quarterly beer production")
lines(beerfit2$mean,col=2)
lines(beerfit3$mean,col=3)
lines(fit4$mean, col=4)
lines(x.ts)
legend("topleft", lty=1, col=c(4,2,3),
       legend=c("Mean method","Naive method","Seasonal naive method"))

beer3 <- window(x.ts, start=2015)
accuracy(beerfit1, beer3)
accuracy(beerfit2, beer3)
accuracy(beerfit3, beer3)
accuracy(fit4, beer3)

## residuals

dj2 <- window(x.ts, end=2017)
plot(dj2, main="Dow Jones Index (daily ending 15 Jul 94)",
     ylab="", xlab="Day")
res <- residuals(naive(x.ts))
plot(res, main="Residuals from naive method",
     ylab="", xlab="Day")
Acf(res, main="ACF of residuals", lag.max = 54)
hist(res, nclass="FD", main="Histogram of residuals")

## test for correlation of resdiuals
# lag=h and fitdf=K
# https://www.youtube.com/watch?v=FETjCDysdWY
# https://www.youtube.com/watch?v=ZjsDE3SyDlk
# https://www.youtube.com/watch?v=QDrmpphIfLE

Box.test(res, lag=52, fitdf=0)
Box.test(res,lag=10, fitdf=0, type="Lj")



## forecating
# x.ts <- ts(x$V1, start=c(2015,1,1), end=c(2017,1,1),frequency=52)

fcast <- forecast(x.ts)
plot(fcast, xlab="City (mpg)", ylab="Carbon footprint (tons per year)")
lines(x.ts2)

x.ts2 <- ts(x$V1, start=c(2015,1,1), end=c(2017,12,1),frequency=52)
beer3 <- window(x.ts2, start=2015)
accuracy(fcast, beer3)










