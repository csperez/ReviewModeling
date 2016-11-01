
#Goal: test a few iterations of a basic generative graph process

library(data.table)
library(ggplot2)
library(sqldf)
library(MASS)
setwd('/Users/Chris/Downloads/iTunesData')

inputMetaData = prepareData("itunes3_reviews_meta.csv")
appsWithCategories = read.csv("appCategories.csv")
N_minReviews = 2
appCatsSensorTower = read.csv("appCategories_SensorTowerOutput copy.csv")
appCatsSensorTower = as.data.frame(unique(appCatsSensorTower))
appCatsSensorTower[which(appCatsSensorTower$subcategory == "Games/Adventure"),]$subcategory = "Games"
appCatsSensorTower[which(appCatsSensorTower$subcategory == "Games/Arcade"),]$subcategory = "Games"
appCatsSensorTower[which(appCatsSensorTower$subcategory == "Games/Board"),]$subcategory = "Games"
appCatsSensorTower[which(appCatsSensorTower$subcategory == "Games/Card"),]$subcategory = "Games"
appCatsSensorTower[which(appCatsSensorTower$subcategory == "Games/Casino"),]$subcategory = "Games"
appCatsSensorTower[which(appCatsSensorTower$subcategory == "Games/Dice"),]$subcategory = "Games"
appCatsSensorTower[which(appCatsSensorTower$subcategory == "Games/Educational"),]$subcategory = "Games"
appCatsSensorTower[which(appCatsSensorTower$subcategory == "Games/Family"),]$subcategory = "Games"
appCatsSensorTower[which(appCatsSensorTower$subcategory == "Games/Music"),]$subcategory = "Games"
appCatsSensorTower[which(appCatsSensorTower$subcategory == "Games/Puzzle"),]$subcategory = "Games"
appCatsSensorTower[which(appCatsSensorTower$subcategory == "Games/Racing"),]$subcategory = "Games"
appCatsSensorTower[which(appCatsSensorTower$subcategory == "Games/Role Playing"),]$subcategory = "Games"
appCatsSensorTower[which(appCatsSensorTower$subcategory == "Games/Simulation"),]$subcategory = "Games"
appCatsSensorTower[which(appCatsSensorTower$subcategory == "Games/Sports"),]$subcategory = "Games"
appCatsSensorTower[which(appCatsSensorTower$subcategory == "Games/Strategy"),]$subcategory = "Games"
appCatsSensorTower[which(appCatsSensorTower$subcategory == "Games/Trivia"),]$subcategory = "Games"
appCatsSensorTower[which(appCatsSensorTower$subcategory == "Games/Word"),]$subcategory = "Games"

appCatsSensorTower = appCatsSensorTower[which(appCatsSensorTower$subcategory != "NoneFound"),]

inputMetaData = merge(inputMetaData, appCatsSensorTower, by = "appid")

#Restrict to only users with at least N reviews:
usersNumReviews = as.data.frame(unique(inputMetaData[c("userid", "unixTimestamp")]))
usersNumReviews$tempCol = 1
usersNumReviews = aggregate(usersNumReviews$tempCol, by = list(usersNumReviews$userid), FUN = sum)
usersWithAtLeastMinReviews = usersNumReviews[which(usersNumReviews$x >= N_minReviews),]
setnames(usersWithAtLeastMinReviews, "Group.1", "userid")
setnames(usersWithAtLeastMinReviews, "x", "numReviews")

inputMetaData = merge(inputMetaData, usersWithAtLeastMinReviews, by = c("userid"))

#####################
#### 1. Calculate histograms, MM's, and lamdbas for the Basic model
#####################

#App generation
#1. N_A()
#2. app lifetimes
#3. category distribution

#Node generation
#1. N_U()
#2. Choose a genre (genre histogram)
#3. Choose app in a genre (PA)

#1. lifetime histograms

userMinTimes = aggregate(inputMetaData$unixTimestamp, by = list(inputMetaData$userid), FUN = min)
userMaxTimes = aggregate(inputMetaData$unixTimestamp, by = list(inputMetaData$userid), FUN = max)
setnames(userMinTimes, "Group.1","userid")
setnames(userMinTimes, "x","minTime")
setnames(userMaxTimes, "Group.1","userid")
setnames(userMaxTimes, "x","maxTime")
userLifetimes = merge(userMinTimes, userMaxTimes, by = "userid")
#userLifetimes$lifetime = ((userLifetimes$maxTime - userLifetimes$minTime)/(24*60*60))
#userLifetimes = merge(userLifetimes, inputMetaData[c("userid", "unixTimestamp", "subcategory")], by.x = c("userid", "minTime"), by.y = c("userid", "unixTimestamp"))
#fit1 <- fitdistr(userLifetimes[which(userLifetimes$subcategory),]$lifetime, "exponential") 

userLifetimes = merge(userLifetimes, inputMetaData[c("userid", "unixTimestamp", "subcategory", "numReviews")], by.x = c("userid", "minTime"), by.y = c("userid", "unixTimestamp"))

#Category distribution

#2. sleep time histogram
usersWithTimestamps = inputMetaData[c("userid", "unixTimestamp")]
usersWithTimestamps = usersWithTimestamps[order(usersWithTimestamps$unixTimestamp),]
usersWithTimestamps = usersWithTimestamps[order(usersWithTimestamps$userid),]
#gen the prev variables:
usersWithTimestamps$prevTimestamp = shift(usersWithTimestamps$unixTimestamp, -1)
usersWithTimestamps$prevUserid = shift(usersWithTimestamps$userid, -1)
#remove starting period observations:
usersWithTimestamps = usersWithTimestamps[which(usersWithTimestamps$userid == usersWithTimestamps$prevUserid),]

usersWithTimestamps$userReviewTimeInterval = (usersWithTimestamps$unixTimestamp - usersWithTimestamps$prevTimestamp)/(24*60*60)

hist(usersWithTimestamps$userReviewTimeInterval[which(usersWithTimestamps$userReviewTimeInterval <= 50)], breaks = 30)









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





userInitial_appCats = c(        "Education"         
                                ,"Entertainment"  ,   "Finance"                 , "Games"             
                                , "Health & Fitness",  "Lifestyle"         
                                , "Medical"       ,    "Music"             , "Navigation"                
                                , "Photo & Video"   ,  "Productivity"      
                                , "Reference"     ,    "Social Networking"         ,  "Travel"            
                                , "Utilities"    )

userLifetimeRates_ByAppCat = as.data.frame(fitdistr(userLifetimes[which(userLifetimes$subcategory == "Books"),]$lifetime, "exponential")$estimate)
colnames(userLifetimeRates_ByAppCat) = c("rate")
userLifetimeRates_ByAppCat$subcategory = "Books"

for (cat in userInitial_appCats) {
  print(cat)
  tempEstimate = as.data.frame(fitdistr(userLifetimes[which(userLifetimes$subcategory == cat),]$lifetime, "exponential")$estimate)
  colnames(tempEstimate) = c("rate")
  tempEstimate$subcategory = cat
  userLifetimeRates_ByAppCat = rbind(userLifetimeRates_ByAppCat, tempEstimate)
  
}

lifetimeHist = hist(userLifetimes$lifetime)

