
#Goal: test a few iterations of a basic generative graph process

library(data.table)
library(ggplot2)
library(sqldf)
setwd('/Users/Chris/Downloads/iTunesData')

inputMetaData = prepareData("itunes3_reviews_meta.csv")
appsWithCategories = read.csv("appCategories.csv")
N_minReviews = 3


#Attach app categories
inputMetaData = merge(inputMetaData, appsWithCategories, by = "appid")

#For now, impute the categories for the apps that didn't return results in our initial scrape:
#unknownApps = appsWithCategories[which(appsWithCategories$category == "NoneFound"),]

#remove apps with missing categories
inputMetaData = inputMetaData[which(inputMetaData$category != "NoneFound" & inputMetaData$category != "TimedOut"),]


#For now, restrict data to only activity from users with at least N_minReviews reviews
usersNumReviews = as.data.frame(unique(inputMetaData[c("userid", "unixTimestamp")]))
usersNumReviews$tempCol = 1
usersNumReviews = aggregate(usersNumReviews$tempCol, by = list(usersNumReviews$userid), FUN = sum)
table(usersNumReviews$x)

usersNumReviews = usersNumReviews[which(usersNumReviews$x >= N_minReviews),]
setnames(usersNumReviews, "Group.1", "userid")
setnames(usersNumReviews, "x", "numReviews")
inputMetaData = merge(inputMetaData, usersNumReviews, by = c("userid"))




#####################
#### 1. Calculate histograms, MM's, and lamdbas for the Basic model
#####################

#1. lifetime histograms

userMinTimes = aggregate(inputMetaData$unixTimestamp, by = list(inputMetaData$userid), FUN = min)
userMaxTimes = aggregate(inputMetaData$unixTimestamp, by = list(inputMetaData$userid), FUN = max)
setnames(userMinTimes, "Group.1","userid")
setnames(userMinTimes, "x","minTime")
setnames(userMaxTimes, "Group.1","userid")
setnames(userMaxTimes, "x","maxTime")
userLifetimes = merge(userMinTimes, userMaxTimes, by = "userid")
userLifetimes$lifetime = ((userLifetimes$maxTime - userLifetimes$minTime)/(24*60*60))

lifetimeHist = hist(userLifetimes$lifetime)

#2. sleep time histogram
usersWithTimestamps = inputMetaData[c("userid", "unixTimestamp")]
usersWithTimestamps = usersWithTimestamps[order(usersWithTimestamps$unixTimestamp),]
usersWithTimestamps = usersWithTimestamps[order(usersWithTimestamps$userid),]

usersWithTimestamps$prevTimestamp = shift(usersWithTimestamps$unixTimestamp, -1)
usersWithTimestamps$prevUserid = shift(usersWithTimestamps$userid, -1)
usersWithTimestamps = usersWithTimestamps[which(usersWithTimestamps$userid == usersWithTimestamps$prevUserid),]
usersWithTimestamps$userReviewTimeInterval = (usersWithTimestamps$unixTimestamp - usersWithTimestamps$prevTimestamp)/(24*60*60)

hist(usersWithTimestamps$userReviewTimeInterval[which(usersWithTimestamps$userReviewTimeInterval <= 50)], breaks = 30)


##### 
#Need:
#N_A(), N_U(), app/user lifetime exp lamdba's
#category histogram




#####################
#### Iteration sketches
#####################

######
#1. Basic
######

#a. App nodes arrive, according to N_A() and sample a lifetime according to an expfit.
  #App lifetimes capture their "trending" period
#a. Nodes arrive, according to the N() arrival function for the Itunes data
#b. Nodes choose a type (genre), for now according to histogram
#c. Nodes draw lifetime, for now according to exponential fit
#d. Nodes draw sleep delta, according to histogram_k

#
#e. Nodes choose a genre, according to 1-MM
#f. Nodes choose an app, according to hist_k
#g. Nodes choose a rating, according to hist_k



#After we've completed the model and have compared the macroscopic statistics,
#How should we categorize anomalies?
#Using likelihood:
#Given our model, what the probability that it generated each users' distributions?
#Ratings:
#delta_ij = pi_k * expfit(lifetime_i) * P(sleepDeltaHist(sleep_delta))
#ratings_ij = pi_k

#genre choices (1-MM; evaluate chain of probabilities)
#ratings choices (1-MM; evaluate chain of probabilities)
#time choices (exp(lambda); evaluate chain of probabilities)





prepareData <-function(inputDataString) {
  metaColNames = c("appid","reviewId","userid","username","stars","version","dateMonth", "dateYear","numHelpfulVotes","totalVotes","unixTimestamp")
  metaColClasses = c("character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character")
  
  reviewMetaData = read.csv(inputDataString, header = F, col.names = metaColNames, colClasses = metaColClasses)
  
  reviewMetaData$unixTimestamp = as.numeric(reviewMetaData$unixTimestamp)
  reviewMetaData$appid = as.numeric(reviewMetaData$appid)
  reviewMetaData$reviewId = as.numeric(reviewMetaData$reviewId)
  reviewMetaData$userid = as.numeric(reviewMetaData$userid)
  
  reviewMetaData = reviewMetaData[which(is.na(reviewMetaData$unixTimestamp) == F),]
  reviewMetaData = reviewMetaData[which(reviewMetaData$unixTimestamp > 10000000),]
  return(reviewMetaData)
}


shift<-function(x,shift_by){
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  
  if (length(shift_by)>1)
    return(sapply(shift_by,shift, x=x))
  
  out<-NULL
  abs_shift_by=abs(shift_by)
  if (shift_by > 0 )
    out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0 )
    out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
    out<-x
  out
}
