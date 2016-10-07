
library(data.table)
library(ggplot2)
setwd('/Users/cperez/Desktop/iTunesData')

#####################
### 1. Read in data 
#####################

#meta schema are:
#appid,review id,userid,username,stars,version,date,number of helpful votes,total votes,unix timestamp

metaColNames = c("appid","reviewId","userid","username","stars","version","dateMonth", "dateYear","numHelpfulVotes","totalVotes","unixTimestamp")
#metaColClasses = c("numeric", "numeric", "numeric", "character", "character", "character", "character", "character", "character", "numeric", "numeric")
metaColClasses = c("character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character")

reviewMetaData = read.csv("itunes3_reviews_meta.csv", header = F, col.names = metaColNames, colClasses = metaColClasses)

reviewMetaData$unixTimestamp = as.numeric(reviewMetaData$unixTimestamp)
reviewMetaData$appid = as.numeric(reviewMetaData$appid)
reviewMetaData$reviewId = as.numeric(reviewMetaData$reviewId)
reviewMetaData$userid = as.numeric(reviewMetaData$userid)



#Note: we may have to filter out some erroneous records.
#Occasionally, character values spill over to numeric columns:


nrow(reviewMetaData[which(is.na(reviewMetaData$unixTimestamp) == T),])
# ^ .004 of the sample
nrow(reviewMetaData[which(reviewMetaData$unixTimestamp <= 10000),])
#^128 records

reviewMetaData = reviewMetaData[which(is.na(reviewMetaData$unixTimestamp) == F),]
reviewMetaData = reviewMetaData[which(reviewMetaData$unixTimestamp > 10000000),]

#####################
## 2. Produce number of Users/Apps over time plot
#####################

#### A. Number of users over time


# get the first timestamp:
#min_Time = min(reviewMetaData$unixTimestamp)
min_Time = 1215648000
# ^ this is July 10, 2008
# ^ calculating manually as there are several erroneous records

#For each node, get the their earliest time stamp:
usersWithMinTimestamps = aggregate(reviewMetaData$unixTimestamp, by = list(reviewMetaData$userid), FUN = min)
setnames(usersWithMinTimestamps, "Group.1", "userid")
setnames(usersWithMinTimestamps, "x", "userMinTime")


#Calculate the number of (fractional) months since the first day:
usersWithMinTimestamps$months = ((usersWithMinTimestamps$userMinTime - min_Time)/(24*60*60*30))

#Calculate the number of users who start each day:
usersWithMinTimestamps = as.data.frame(unique(usersWithMinTimestamps[c("userid", "months")]))
usersWithMinTimestamps$dummyCol = 1
usersWithMinTimestamps = aggregate(usersWithMinTimestamps$dummyCol, by = list(usersWithMinTimestamps$months), FUN = sum)
setnames(usersWithMinTimestamps, "Group.1", "months")
setnames(usersWithMinTimestamps, "x", "numUsers")

usersWithMinTimestamps = usersWithMinTimestamps[order(usersWithMinTimestamps$months),]
usersWithMinTimestamps$cumulativeNumUsers = cumsum(usersWithMinTimestamps$numUsers)

#Remove times prior to the min time:
usersWithMinTimestamps = usersWithMinTimestamps[which(usersWithMinTimestamps$months > 0),]

#Plot the number of users over time:

ggplot(data = usersWithMinTimestamps, aes(x = months , y = cumulativeNumUsers )) + geom_point() + labs(title = "Number of Users over Time, starting at July 10, 2008")
ggsave("ITunesData_numUsersOverTime.png")


#####
### B. number of apps over time:
#####



#For each node, get the their earliest time stamp:
appsWithMinTimestamps = aggregate(reviewMetaData$unixTimestamp, by = list(reviewMetaData$appid), FUN = min)
setnames(appsWithMinTimestamps, "Group.1", "appid")
setnames(appsWithMinTimestamps, "x", "appMinTime")

appsWithMinTimestamps$months = ((appsWithMinTimestamps$appMinTime - min_Time)/(24*60*60))/30


#Calculate the number of apps that start each day:
appsWithMinTimestamps = as.data.frame(unique(appsWithMinTimestamps[c("appid", "months")]))
appsWithMinTimestamps$dummyCol = 1
appsWithMinTimestamps = aggregate(appsWithMinTimestamps$dummyCol, by = list(appsWithMinTimestamps$months), FUN = sum)
setnames(appsWithMinTimestamps, "Group.1", "months")
setnames(appsWithMinTimestamps, "x", "numApps")

appsWithMinTimestamps = appsWithMinTimestamps[order(appsWithMinTimestamps$months),]
appsWithMinTimestamps$cumulativeNumApps = cumsum(appsWithMinTimestamps$numApps)


appsWithMinTimestamps = appsWithMinTimestamps[which(appsWithMinTimestamps$months > 0),]

ggplot(data = appsWithMinTimestamps, aes(x = months , y = cumulativeNumApps )) + geom_point() + labs(title = "Number of Apps over Time, starting at July 10, 2008")
ggsave("ITunesData_numAppsOverTime.png")







