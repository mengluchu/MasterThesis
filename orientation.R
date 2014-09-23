library(zoo)

deter = read.csv("/home/christopher/Documents/Studium/Master Thesis/Orientation/sample.csv")

dates = strsplit(as.vector(deter$dates), ';')
dates = unlist(dates)
dates = as.Date(dates, format="%Y-%m-%d")

evi = strsplit(as.vector(deter$evi), ';')
evi = as.numeric(unlist(evi))

zoots = zoo(evi, dates, frequency=24)
# check whether the time series is regular. If regular use ts(), if not zoo()
isRegular = is.regular(zoots, strict = FALSE)
isRegular
plot(zoots, typ = "l")

# dont use ts for not strict regular / not equispaced time series
tsts = ts(evi, start = c(2000, 3), end = c(2012, 9), frequency = 24)
plot(tsts)
tsts_decom = decompose(tsts, type = "additive")
plot(tsts_decom)

# Do the linear regression on the time series
lm = lm(evi ~ dates)
plot(zoots)
abline(lm)
plot(lm)

# Decompose the time series into seasonality, trend and remainder with stl().
na.new <- function(x) ts(na.exclude(x), frequency = 12)
m<-na.exclude(zoots)
m1-m1[-1]
m1<-time(m)
as.ts(zoots)
zoots_decom = stl(zoots, na.action = na.new, s.window = "per")
plot(zoots_decom)

# Interpret stl() result.

# Decomposition procedures are used in time series to describe the trend and seasonal factors in a time series.
# One of the main objectives for a decomposition is to estimate seasonal effects that can be used to create and present
# seasonally adjusted values. A seasonally adjusted value removes the seasonal effect from a value so that trends can be seen
# more clearly. The additive decomposition model is useful when the seasonal variation is relatively constant over time.

# The seasonal variation looked to be about the same magnitude across time, so an additive decomposition might be good. 
# There is a major trend change from the year 2009 on. The seasonality is a very regularly repeating pattern.

# Compute linear regression on seasonal trend and remainder component, respectively.
str(zoots_decom)
zoots_decom_seasonal = zoots_decom$time.series[,"seasonal"]
plot(zoots_decom_seasonal)
lm_seasonal = lm(zoots_decom_seasonal ~ dates)
abline(lm_seasonal)

zoots_decom_remainder = zoots_decom$time.series[,"remainder"]
plot(zoots_decom_remainder)
lm_remainder = lm(zoots_decom_remainder ~ dates)
abline(lm_remainder)

zoots_decom_trend = zoots_decom$time.series[,"trend"]
plot(zoots_decom_trend,ylim=c(0,1.5))
lm_trend = lm(zoots_decom_trend ~ dates)
abline(lm_trend)

# Learn about bfast parameters and try to perform bfast on the time series
library(bfast)
ts = ts(rowSums(zoots_decom$time.series))
# assign start, end and frequency
tsp(ts) = tsp(zoots_decom$time.series)
bf = bfast(ts, h=0.15, season="harmonic", max.iter=1)
plot(bf)


# Interpret the bfast result

# Do the same thing to other records

stlmon<-stl(monmean, na.action = na.new, s.window = "per")
datamon <- ts(rowSums(stlmon$time.series)) 
tsp(datamon) <- tsp(stlmon$time.series)
spt<-zoo(spt1,a1)
monmean <- aggregate(spt, as.Date(as.yearmon(a1)), median) #should aggregate in SciDB
summary(monmean)
frequency(monmean)<-12
na.new <- function(x) ts(na.exclude(x), frequency = 12)
stlmon<-stl(monmean, na.action = na.new, s.window = "per")
datamon <- ts(rowSums(stlmon$time.series)) 
tsp(datamon) <- tsp(stlmon$time.series)
fitmon <- bfast(datamon,h=0.15, season="harmonic", max.iter=1)

require(graphics)

ts(1:10, frequency = 4, start = c(1959, 2)) # 2nd Quarter of 1959
print( ts(1:10, frequency = 7, start = c(12, 2)), calendar = TRUE)
# print.ts(.)
## Using July 1954 as start date:
gnp <- ts(cumsum(1 + round(rnorm(100), 2)),
          start = c(1954, 7), frequency = 12)
plot(gnp) # using 'plot.ts' for time-series plot

## Multivariate
z <- ts(matrix(rnorm(300), 100, 3), start = c(1961, 1), frequency = 12)
class(z)
head(z) # as "matrix"
plot(z)
plot(z, plot.type = "single", lty = 1:3)

## A phase plot:
plot(nhtemp, lag(nhtemp, 1), cex = .8, col = "blue",
     main = "Lag plot of New Haven temperatures")