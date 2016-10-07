
#Input: edges between users and apps
#Output: estimation of parameters lambda, alpha, and beta from the leskovec model

library(data.table)
library(ggplot2)
library(stats4)
setwd('/Users/cperez/Desktop/iTunesData')

#####################
### 1. Import and clean data
#####################

metaColNames = c("appid","reviewId","userid","username","stars","version","dateMonth", "dateYear","numHelpfulVotes","totalVotes","unixTimestamp")
metaColClasses = c("character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character")

reviewMetaData = read.csv("itunes3_reviews_meta.csv", header = F, col.names = metaColNames, colClasses = metaColClasses)

reviewMetaData$unixTimestamp = as.numeric(reviewMetaData$unixTimestamp)
reviewMetaData$appid = as.numeric(reviewMetaData$appid)
reviewMetaData$reviewId = as.numeric(reviewMetaData$reviewId)
reviewMetaData$userid = as.numeric(reviewMetaData$userid)

#Clean data:
reviewMetaData = reviewMetaData[which(is.na(reviewMetaData$unixTimestamp) == F),]
reviewMetaData = reviewMetaData[which(reviewMetaData$unixTimestamp > 10000000),]


#####################
### 2. Produce node-level dataset (nodes are both users and apps).
#####################

#Get user/app arrival times:
min_Time = 1215648000
# ^ this is July 10, 2008

#For each node, get the their earliest time stamp (arrival time):
usersWithMinTimestamps = aggregate(reviewMetaData$unixTimestamp, by = list(reviewMetaData$userid), FUN = min)
setnames(usersWithMinTimestamps, "Group.1", "userid")
setnames(usersWithMinTimestamps, "x", "userMinTime")

userOutdegrees = as.data.frame(unique(reviewMetaData[c("userid", "appid")]))
userOutdegrees$dummyCol = 1
userOutdegrees = aggregate(userOutdegrees$dummyCol, by = list(userOutdegrees$userid), FUN = sum)
setnames(userOutdegrees, "Group.1", "userid")
setnames(userOutdegrees, "x", "userOutdegree")

# Combine:
useridsAttributes = merge(usersWithMinTimestamps,userOutdegrees, by = "userid" )


#Fit power law formula:
LL <- function(lambda=.01, alpha=.8, beta=.002) {
  useridsAttributes$value = useridsAttributes$userOutdegree ^ (- (1 + (lambda*gamma(2 - alpha))/(beta*gamma(1 - alpha))  ) )
  return(-sum(log(useridsAttributes$value)))
}

mle(LL, start = list(lambda=.01, alpha=.8, beta=.002), method = "L-BFGS-B", lower = c(0,0, 0),upper = c(Inf, Inf,Inf))






