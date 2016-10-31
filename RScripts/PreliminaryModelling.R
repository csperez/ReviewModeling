
library(data.table)
library(ggplot2)
library(sqldf)
setwd('/Users/Chris/Downloads/iTunesData')

inputMetaData = prepareData("itunes3_reviews_meta.csv")
N_minReviews = 3

#####################
#### 1. Restrict to only users with at least N_minReviews reviews
#####################

usersNumReviews = as.data.frame(unique(inputMetaData[c("userid", "unixTimestamp")]))
usersNumReviews$tempCol = 1
usersNumReviews = aggregate(usersNumReviews$tempCol, by = list(usersNumReviews$userid), FUN = sum)
table(usersNumReviews$x)

#> table(usersNumReviews$x)
#1      2      3      4      5      6      7      8      9     10     11     12     13     14     15 
#500053  34181   5576   1484    465    200     99     51     32     13      9      7      2      4      1 
#16     17     18     23     25     26     33     37 
#2      2      1      1      1      2      1      1 

usersWithAtLeastMinReviews = usersNumReviews[which(usersNumReviews$x >= 6),]
setnames(usersWithAtLeastMinReviews, "Group.1", "userid")
print(paste("Num of rows before min-reviews restriction: ", nrow(inputMetaData)))

inputMetaData = merge(inputMetaData, usersWithAtLeastMinReviews, by = c("userid"))
print(paste("Num of rows after min-reviews restriction: ", nrow(inputMetaData)))


#####################
#### 2. Get app categories
#####################
#https://sensortower.com/ios/us/publisher/app/app_name/507710255

#6b0fd75dd27519a7bbb263fd697917d4bfe44158
#curl -H "Authorization: Bearer 6b0fd75dd27519a7bbb263fd697917d4bfe44158" https://api.appannie.com/v1.2/apps/ios/app/529479190/details
#https://api.appannie.com/v1.2/apps/ios/app/529479190/details
http://blog.scottlogic.com/2014/03/20/app-store-analysis.html
https://42matters.com/pricing
appsWithCategories = read.csv("appCategories.csv")

unknownApps_FirstIter = appsWithCategories[which(appsWithCategories$category == "NoneFound"),]

write.csv(unknownApps_FirstIter, "unknownApps_FirstIter.csv", row.names = F)


#####################
#### 3. Calculate histograms for distributions
#####################
#1. lifetime histogram
#2. sleep time histogram











#####################
#### Iteration sketches
#####################

###
#1. Basic
###

#a. Nodes arrive, choosing a type (genre)





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