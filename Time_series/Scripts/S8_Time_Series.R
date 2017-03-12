###################################################
#####     Time Series Analysis and Mining     #####
###################################################
# free memory
rm(list = ls())
gc()

# Here we look at examples on time series 
# decomposition, forecasting, clustering and 
# classification.

# Class ts represents data which has been 
# sampled at equispaced points in time. 
# A frequency of 7 indicates that a time series
# is composed of weekly data, and 12 and 4 are
# used respectively for monthly and quarterly series. 

# Example: Unemployment in Maine

# monthly unemployment rate for the US 
# state of Maine Jan 1996 until Aug 2006

# get the data (is also in folder as Maine_dat.txt)
www <- "http://elena.aut.ac.nz/~pcowpert/ts/Maine.dat"
Maine.month <- read.table(www, header = TRUE)
# so can manipulate single variable in data frame
attach(Maine.month)
class(Maine.month)
# is a data frame
# We convert it to a time series object
Maine.month.ts <- ts(unemploy, 
                     start = c(1996, 1), 
                     freq = 12)
# take a look
Maine.month.ts

# we divide by 12 to get a mean annual rate:
Maine.annual.ts <- aggregate(Maine.month.ts)/12
# take another look
Maine.annual.ts

# We now plot both time series. 
# There is clear monthly variation.
# it seems that the February figure 
# is typically about 20% more than the
# annual average, whereas the August figure
# tends to be roughly 20% less.
layout(1:2)
plot(Maine.month.ts, ylab = "unemployed (%)")
plot(Maine.annual.ts, ylab = "unemployed (%)")
layout(1:1)
# We calculate the precise percentages with window(). 
# This function will extract that part of 
# the time series between specified start
# and end points and will sample with an 
# interval equal to frequency if its argument
# is set to TRUE. So, the first lines below gives
# a time series of February figures.
Maine.Feb <- window(Maine.month.ts, 
                    start = c(1996,2), 
                    freq = TRUE)
Maine.Feb
Maine.Aug <- window(Maine.month.ts, 
                    start = c(1996,8), 
                    freq = TRUE)
Maine.Aug
Feb.ratio <- mean(Maine.Feb) / mean(Maine.month.ts)
Aug.ratio <- mean(Maine.Aug) / mean(Maine.month.ts)
Feb.ratio
Aug.ratio
detach(Maine.month)

# On average, unemployment is 22% higher in 
# February and 18% lower in August. A possible
# explanation is that Maine attracts tourists 
# during the summer, and this creates more jobs. 

# Also, the period before Christmas and over the
# New Year's holiday tends to have higher employment
# rates than the first few months of the new year.

# If we had sampled the data in August 
# of each year, for example, rather than
# taking yearly averages, we would have 
# consistently underestimated the
# unemployment rate by a factor of 
# about 0.8.

# The monthly unemployment rate for 
# all of the United States from January
# 1996 until October 2006 is plotted below. 
# The decrease in the unemployment rate
# around the millennium is common to Maine
# and the United States as a whole, but 
# Maine does not seem to be sharing the
# current US decrease in unemployment.

www <- "http://elena.aut.ac.nz/~pcowpert/ts/USunemp.dat"
US.month <- read.table(www, header = T)
# also in your folder as "USunemp_dat")
attach(US.month)
US.month.ts <- ts(USun, start=c(1996,1), 
                  end=c(2006,10), freq = 12)
US.month.ts
layout(1:1)
plot(US.month.ts, ylab = "unemployed (%)")
detach(US.month)

#########################################################

# Example: Airline Travel
# This An example shows the construction of a 
# time series with 30 values (1 to 30). 

# Frequency=12 and start=c(2011,3) specify it
# is a monthly series starting March 2011.
a <- ts(1:30, frequency=12, start=c(2011,3))
print(a)
str(a)
attributes(a)

###  Time Series Decomposition

# Time Series Decomposition is to decompose a
# time series into these components:

# trend is long term trend;

# seasonal is seasonal variation;

# cyclical is repeated, but non-periodic fluctuations; and

# irregular are the residuals.

# Time series of AirPassengers example 
# demonstrates time series decomposition.

# Is composed of monthly totals of Box & Jenkins
# international airline passengers from 1949 to 1960. 

# It has 144(=12*12) values.
plot(AirPassengers)

# decompose() breaks it into various components
apts <- ts(AirPassengers, frequency=12);apts
f <- decompose(apts);f
# seasonal figures
f$figure
plot(f$figure, type="b", xaxt="n", xlab="")
# get names of 12 months in English words
monthNames <- months(ISOdate(2011,1:12,1))
# label x-axis with month names 
# las is set to 2 for vertical label orientation
axis(1, at=1:12, labels=monthNames, las=2) 

# Time series decomposition figure:
plot(f)

### Time Series Forecasting
# This means forecasting future events based
# on historical data. One example is to predict
# stock opening price based on past performance. 

# Two popular models are:
# autoregressive moving average (ARMA) and 
# autoregressive integrated moving average (ARIMA).

# Here we fit an ARIMA model to a univariate
# time series and then use it for forecasting.

fit <- arima(AirPassengers, order=c(1,0,0), 
             list(order=c(2,1,0), period=12))
fit
fore <- predict(fit, n.ahead=24)
fore
# error bounds at 95% confidence level
U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$se
# the red solid line shows the forecasted values, 
# and the blue dotted lines are error
# bounds at a confidence level of 95%.
ts.plot(AirPassengers, fore$pred, U, L, col=c(1,2,4,4), 
        lty = c(1,1,2,2))
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"),
       col=c(1,2,4), lty=c(1,1,2))

#########################################################
# Example Multiple time series: 
# Electricity, beer and chocolate data

# Here we illustrate a few important ideas 
# and concepts related to multiple time
# series data. 

# The monthly supply of electricity (millions of kWh), 
# beer (Ml), and chocolate-based production (tonnes)
# in Australia over the period January 1958 to 
# to December 1990 are available from the 
# Australian Bureau of Statistics. The three
# series have been stored in a single file online:
www <- "http://elena.aut.ac.nz/~pcowpert/ts/cbe.dat"
CBE <- read.table(www, header = T)
# also in your folder as "cbe_dat"
CBE[1:4, ]
class(CBE)
# Now create time series objects for the 
# electricity, beer, and chocolate data.

Elec.ts <- ts(CBE[, 3], 
              start = 1958, 
              freq = 12)
Elec.ts
Beer.ts <- ts(CBE[, 2], 
              start = 1958, 
              freq = 12)
Beer.ts
Choc.ts <- ts(CBE[, 1], 
              start = 1958, 
              freq = 12)
Choc.ts
plot(cbind(Elec.ts, Beer.ts, Choc.ts))

# The plots in show increasing trends in
# production for all three goods mainly due
# to an increasing population. 

# But electricity production has risen 
# by a factor of 7, and chocolate production
# by a factor of 4, over this period during
# which the population has not quite doubled.

# The three series constitute a multiple 
# time series. There are many functions
# in R for handling more than one series, 
# including ts.intersect to obtain the
# intersection of two series that overlap 
# in time. 

# Get air passenger data again
data(AirPassengers)
AP <- AirPassengers
AP

# The intersection between the air passenger
# data and the electricity data is obtained as follows:
AP.elec <- ts.intersect(AP, Elec.ts)
AP.elec
# Now check your output:
start(AP.elec)
# [1] 1958 1
end(AP.elec)
# [1] 1960 12
AP.elec[1:3, ]
#       AP Elec.ts
# [1,] 340 1497
# [2,] 318 1463
# [3,] 362 1648

# the data for each series are extracted 
# and plotted
AP <- AP.elec[,1]; Elec <- AP.elec[,2]
layout(1:2)
plot(AP, main = "", ylab = "Air passengers / 1000's")
plot(Elec, main = "", ylab = "Electricity production / MkWh")
layout(1:1)
# International air passengers and Australian 
# electricity production for the period 1958-1960.
# The plots look similar because both series
# have an increasing trend and seasonal cycle. 

# as.vector is needed to convert the ts objects to
# ordinary vectors suitable for a scatter plot.
plot(as.vector(AP), as.vector(Elec),
     xlab = "Air passengers / 1000's",
     ylab = "Electricity production / MWh")
abline(reg = lm(Elec ~ AP))
layout(1:1)
# The two time series are highly correlated, 
# as can be seen in the plots, with a
# correlation coefficient of 0.88.
cor(AP, Elec)

# it is not plausible that higher numbers of
# air passengers in the United States cause, 
# or are caused by, higher electricity production
# in Australia. A reasonable explanation for 
# the correlation is that the increasing prosperity
# and technological development in both countries
# over this period accounts for the increasing trends. 

# The two time series also happen to have similar
# seasonal variations. For these reasons, it 
# is usually appropriate to remove trends and
# seasonal effects before comparing multiple series.

#################################################################
### Time Series Clustering

# partitions time series data into groups 
# based on similarity or distance, so that
# time series in the same cluster are 
# similar to each other. 

# Measures of distance (dissimilarity) include

# Euclidean distance, Manhattan distance, Maximum norm,
# Hamming distance, the angle between two vectors 
# (inner product), and Dynamic Time Warping
# (DTW) distance.

# Dynamic Time Warping Distance

# Package dtw finds optimal alignment 
# between two time series. 

# function dtw(x, y, ...) computes dynamic 
# time warp and finds optimal alignment between
# two time series x and y. 

# function dtwDist(mx, my=mx, ...) or 
# dist(mx, my=mx, method="DTW", ...)
# calculates the distances between time series mx and my.

library(dtw)
idx <- seq(0, 2*pi, len=100);idx
a <- sin(idx) + runif(100)/10;a
b <- cos(idx);b
align <- dtw(a, b, step=asymmetricP1, keep=T);align
# Alignment with dynamic time warping:
dtwPlotTwoWay(align)

### Synthetic Control Chart Time Series Data

# This dataset contains 600 examples of control
# charts synthetically generated by the process in Alcock
# and Manolopoulos (1999). 

# http://kdd.ics.uci.edu/databases/synthetic_control/synthetic_control.html

# Each control chart is a time series of 60 values,
# and there are six classes:
# 001-100: Normal
# 101-200: Cyclic
# 201-300: Increasing Trend
# 301-400: Decreasing Trend
# 401-500: Upward shift; and
# 501-600: Downward shift

# read in synthetic_control_data.txt
sc <- read.table(file.choose(), 
                 header=F, sep="");sc
# show one sample from each class
idx <- c(1,101,201,301,401,501);idx
sample1 <- t(sc[idx,]);sample1
# Plot six classes in synthetic control chart time series
plot.ts(sample1, main="")

#### Hierarchical Clustering with Euclidean Distance
# We set a seed for replicability
set.seed(6218)
# We select 10 cases randomly from each class
n <- 10;n
s <- sample(1:100, n);s
idx <- c(s, 100+s, 200+s, 300+s, 400+s, 500+s);idx
sample2 <- sc[idx,];sample2
observedLabels <- rep(1:6, each=n)
# hierarchical clustering with Euclidean distance
hc <- hclust(dist(sample2), method="average")
plot(hc, labels=observedLabels, main="")
# cut tree to get 6 clusters
rect.hclust(hc, k=6)
memb <- cutree(hc, k=6)
table(observedLabels, memb)
# plot shows shows increasing trend (class 3)
# and upward shift (class 5) are not well 
# separated, and decreasing trend (class 4) 
# and downward shift (class 6) are also mixed

##### Hierarchical Clustering with DTW Distance

library(dtw)
distMatrix <- dist(sample2, method="DTW")
hc <- hclust(distMatrix, method="average")
plot(hc, labels=observedLabels, main="")
# cut tree to get 6 clusters
rect.hclust(hc, k=6)
memb <- cutree(hc, k=6)
table(observedLabels, memb)

# We note that DTW distance is better than Euclidean
# distance for measuring the similarity betweem
# time series

##### Time Series Classification

# Time series classification models are based on 
# labeled time series and we use these model to 
# predict the label of unlabeled time series. 

# New features extracted from time series may help
# to improve the performance of classification models. 

# Techniques for feature extraction include Singular
# Value Decomposition (SVD), Discrete Fourier Transform 
# (DFT), DiscreteWavelet Transform (DWT), Piecewise 
# Aggregate Approximation (PAA), Perpetually Important Points
# (PIP), Piecewise Linear Representation, and Symbolic Representation.

# Classification with Original Data

# We use ctree() from package party [Hothorn et al., 2010]
# to demonstrate classification of time series with the
# the original data. The class labels are changed 
# into categorical values before feeding the data
# into ctree(), so that we won't get class labels
# as a real numbers like 1.35. 

classId <- rep(as.character(1:6), each=100)
classId
newSc <- data.frame(cbind(classId, sc))
newSc
library(party)
ct <- ctree(classId ~ ., data=newSc, 
            controls = ctree_control(minsplit=30, 
                                     minbucket=10, 
                                     maxdepth=5))
pClassId <- predict(ct)
pClassId
table(classId, pClassId)
# accuracy
(sum(classId==pClassId)) / nrow(sc)
plot(ct, ip_args=list(pval=FALSE), 
     ep_args=list(digits=0))

### Classification with Extracted Features
# Next, we use DWT (Discrete Wavelet Transform) [Burrus et al., 1998]
# to extract features from time series 
# and then build a classification model. 

# Wavelet transform provides a multi-resolution
# representation using wavelets. An example of 
# Haar Wavelet Transform, the simplest DWT, is
# available at http://dmr.ath.cx/gfx/haar/. 

# Another popular feature extraction technique is
# Discrete Fourier Transform (DFT) [Agrawal et al., 1993].

# An example on extracting DWT coefficients is shown below.
# Package wavelets [Aldrich, 2010] is used for discrete
# wavelet transform. In the package, function dwt(X, filter,                                                                             
# n.levels, ...) computes the discrete wavelet 
# transform coefficients, where X is a univariate
# or multi-variate time series, filter indicates
# which wavelet filter to use, and n.levels specifies
# the level of decomposition. 

# It returns an object of class dwt, whose slot 
# W contains wavelet coefficients and V contains
# scaling coefficients. The original time series
# can be reconstructed via an inverse discrete
# wavelet transform with function idwt().

library(wavelets)
# set up structure to hold results
wtData <- NULL
for (i in 1:nrow(sc)) {
  a <- t(sc[i,])
  wt <- dwt(a, filter="haar", boundary="periodic")
  wtData <- rbind(wtData, unlist(c(wt@W, wt@V[[wt@level]])))
}
wtData <- as.data.frame(wtData)
head(wtData)
wtSc <- data.frame(cbind(classId, wtData))
head(wtSc)

# build a decision tree with DWT coefficients
ct <- ctree(classId ~ ., data=wtSc, 
            controls = ctree_control(minsplit=30, 
                                     minbucket=10, 
                                     maxdepth=5))
pClassId <- predict(ct)
table(classId, pClassId)
(sum(classId==pClassId)) / nrow(wtSc)
plot(ct, ip_args=list(pval=FALSE), ep_args=list(digits=0))

### k-NN Classification

# The k-NN classification can also be used. 
# It determines the k nearest neighbors of a new 
# instance and then labels it by majority voting. 

# However, the time complexity of a naive way to find
# k nearest neighbors is O(n^2), where n 
# is the size of data. Therefore, an efficient
# indexing structure is needed for large datasets. 

# Package RANN supports fast nearest neighbor
# search with a time complexity of O(n log n) 
# using Arya and Mount's ANN library. Below
# is an example of k-NN classification of time 
# series without indexing.

# fix seed to get a fixed result
set.seed(100)
# k is 20
k <- 20
# create a new time series by adding
# noise to time series 501
newTS <- sc[501,] + runif(100)*15
distances <- dist(newTS, sc, method="DTW")
s <- sort(as.vector(distances), index.return=TRUE)
# class IDs of k nearest neighbors
table(classId[s$ix[1:k]])

# For the 20 nearest neighbors of the 
# new time series, three of them are of 
# class 4, and 17 are of class 6. 

# With majority voting, that is, taking 
# the more frequent label as winner, the
# label of the new time series is set
# to class 6.

