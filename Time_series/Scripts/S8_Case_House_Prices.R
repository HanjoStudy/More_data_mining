#####################################
###   Analysis and Forecasting    ###
###    of House Price Indices     ###
#####################################
# free memory
rm(list = ls())
gc()

# data are Canberra house price trading indices
# import data House_index_canberra.csv"
# Are HPIs at end of every month from Jan 1990
# to Jan 2011
houseIndex <- read.csv(file.choose(), header=FALSE)
names(houseIndex) <- c("date", "index")
n <- nrow(houseIndex)
# check start date and end date
cat(paste("HPI from", houseIndex$date[1],
          "to", houseIndex$date[n], "\n"))
# extract year and month into POSIXlt format
dates <- strptime(houseIndex$date, 
                  format="%d-%b-%y")
# look
dates
# get year
(houseIndex$year <- dates$year + 1900)
# get month
(houseIndex$month <- dates$mon + 1)
fromYear <- houseIndex$year[1]
# look
fromYear

###################################################
### Another way to do the data conversions
###################################################
## dates <- as.Date(houseIndex$date, format="%d-%b-%y")
## houseIndex$year <- as.numeric(format(dates, "%Y"))
## houseIndex$month <- as.numeric(format(dates, "%m"))

# We explore the data with various plots
plot(houseIndex$index, pty=1, type="l", 
     lty="solid", xaxt="n", xlab="",
     ylab="Index", main=paste("HPI (Canberra) - Since ", 
                              fromYear, sep=""))
# draw tick-marks at 31 Jan of every year
nYear <- ceiling(n/12)
posEveryYear <- 12 * (1:nYear) - 11
axis(1, labels=houseIndex$date[posEveryYear], 
     las=3, at=posEveryYear)
# add horizontal reference lines
abline(h=1:4, col="gray", lty="dotted")
# draw a vertical reference line every five years
# calculate positions of reference for grid lines
posEvery5years <- 12 * (5* 1:ceiling(nYear/5) - 4) - 11
# draw grid lines
abline(v=posEvery5years, col="gray", lty="dotted")

# Calculate delta, the increase of HPI in every month
houseIndex$delta <- houseIndex$index - c(1, houseIndex$index[-n])
plot(houseIndex$delta, main="Increase in HPI",
     xaxt="n", xlab="")
axis(1, labels=houseIndex$date[posEveryYear], 
     las=3, at=posEveryYear)
# add a reference line
abline(h=0, lty="dotted")

# It looks like HPI fluctuated more after 2003

# But we check further by looking at the ratio
# of increase in every month:
houseIndex$rate <- houseIndex$index / c(1, houseIndex$index[-n]) - 1
# percentage of months having positive increases in HPI
100 * sum(houseIndex$rate>0)/n
# use ifelse() to set positive values
# to green and and negative ones to red
plot(houseIndex$rate, xaxt="n", xlab="", 
     ylab="HPI Increase Rate",
     col=ifelse(houseIndex$rate>0,"green","red"),
     pch=ifelse(houseIndex$rate>0,"+","o"))
axis(1, labels=houseIndex$date[posEveryYear], 
     las=3, at=posEveryYear)
abline(h=0, lty="dotted")
# We can see that:
# 1) There are more increases than decreases.
# 2) The increase rates (green) are generally
#    bigger than the decrease rates (red).
# 3) There are two periods with big decreases:
#    1995-1996 and 2008-2009; period with
#    biggest increases is 2002-203

# Alternatively, we can make a table of increase rate
# with each row representing a month and each column
# a year, and then we show the monthly increase rates
# with a grouped bar chart.
rateMatrix <- xtabs(rate ~ month + year, data=houseIndex)
# show the first four years, rounded to 4 decimal places
round(rateMatrix[,1:4], digits=4)
# plot a grouped barchart: 
# beside=TRUE justaposes bars; 
# space=c(0,2) sets space between bars
barplot(rateMatrix, beside=TRUE, space=c(0,2),
        col=ifelse(rateMatrix>0,"lightgreen","lightpink"),
        ylab="HPI Increase Rate", cex.names=1.2)

