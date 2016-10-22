
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


#####################
## 3. Figure 1: probability that a new edge chooses a node with degree d; i.e. the p_e(d) formula
#####################

#######
### A. First, let's look only at edges formed with apps (rather than other users)
#######

#1. Compute the degree of each app at time t

#Get the earliest review of each (user,app) tuple:
userAppEarliestEdges = aggregate(reviewMetaData$unixTimestamp, by = list(reviewMetaData$appid, reviewMetaData$userid), FUN = min)
setnames(userAppEarliestEdges, "Group.1", "appid")
setnames(userAppEarliestEdges, "Group.2", "userid")
setnames(userAppEarliestEdges, "x", "unixTimestamp")

# Get the degree per app by timestamp value:
userAppEarliestEdges$userTest = 1
appDegreesPerTimestamp = aggregate(userAppEarliestEdges$userTest,by = list(userAppEarliestEdges$appid, userAppEarliestEdges$unixTimestamp), FUN = sum)
setnames(appDegreesPerTimestamp, "Group.1", "appid")
setnames(appDegreesPerTimestamp, "Group.2", "unixTimestamp")
setnames(appDegreesPerTimestamp, "x", "numNewUsersAtTimestamp")

appDegreesPerTimestamp = appDegreesPerTimestamp[order(appDegreesPerTimestamp$unixTimestamp),]
appDegreesPerTimestamp_dt <- data.table(appDegreesPerTimestamp)
appDegreesPerTimestamp_dt = appDegreesPerTimestamp_dt[, degreeAtTimeT := cumsum(numNewUsersAtTimestamp), by=list(appid)]
appDegreesPerTimestamp = as.data.frame(appDegreesPerTimestamp_dt)
appDegreesPerTimestamp = appDegreesPerTimestamp[order(appDegreesPerTimestamp[c("appid", "unixTimestamp")]),]

# To each new edge, attach the degree from the previous timestmap:
appDegreesPerTimestamp$degreeAtPriorTimestamp = shift(appDegreesPerTimestamp$degreeAtTimeT, -1)
userAppEarliestEdges = merge(userAppEarliestEdges, appDegreesPerTimestamp, by = c("appid", "unixTimestamp") )

userAppEarliestEdges$edgeDummyCol = 1
numEdgesWithPriorTimeDegreeD = aggregate(userAppEarliestEdges$edgeDummyCol, by = list(userAppEarliestEdges$unixTimestamp, userAppEarliestEdges$degreeAtPriorTimestamp), FUN = sum)
setnames(numEdgesWithPriorTimeDegreeD, "Group.1", "unixTimestamp")
setnames(numEdgesWithPriorTimeDegreeD, "Group.2", "degreeAtPriorTimestamp")
setnames(numEdgesWithPriorTimeDegreeD, "x", "numEdges")

#These are then the numerators:
numEdgesWithPriorTimeDegreeD = aggregate(numEdgesWithPriorTimeDegreeD$numEdges, by = list(numEdgesWithPriorTimeDegreeD$degreeAtPriorTimestamp), FUN = sum)
setnames(numEdgesWithPriorTimeDegreeD, "Group.1", "degreeAtPriorTimestamp")
setnames(numEdgesWithPriorTimeDegreeD, "x", "numEdges")

#Count the number of apps with degree d at the prior timestmap:
#Need to do a cartesian product of unique timestamps and apps:
possibleTimestamps = as.data.frame(unique(appDegreesPerTimestamp$unixTimestamp))
colnames(possibleTimestamps) = c("unixTimestamp")
#To reduce space used, get apps' min times;
appMinTimes = aggregate(appDegreesPerTimestamp$unixTimestamp, by = list(appDegreesPerTimestamp$appid), FUN = min)
setnames(appMinTimes, "Group.1", "appid")
setnames(appMinTimes, "x", "minTime")

appDegreeEvolution =  merge(appDegreesPerTimestamp, appMinTimes, by = c("appid"))
uniqueApps = as.data.frame(unique(appDegreeEvolution$appid))
colnames(uniqueApps) = c("appid")

possibleAppsTimes = merge(uniqueApps, possibleTimestamps, by = NULL)
possibleAppsTimes = merge(possibleAppsTimes,appDegreeEvolution[c("appid", "unixTimestamp", "numNewUsersAtTimestamp")], by = c("appid", "unixTimestamp"), all.x = T )
possibleAppsTimes = merge(possibleAppsTimes, appMinTimes, by = c("appid"))
possibleAppsTimes = possibleAppsTimes[which(possibleAppsTimes$unixTimestamp >= possibleAppsTimes$minTime),]
possibleAppsTimes[which(is.na(possibleAppsTimes$numNewUsersAtTimestamp) == T),]$numNewUsersAtTimestamp = 0

possibleAppsTimes = possibleAppsTimes[order(possibleAppsTimes$unixTimestamp),]
possibleAppsTimes_dt <- data.table(possibleAppsTimes)
possibleAppsTimes_dt = possibleAppsTimes_dt[, completeDegreeAtTimeT := cumsum(numNewUsersAtTimestamp), by=list(appid)]
possibleAppsTimes = as.data.frame(possibleAppsTimes_dt)
possibleAppsTimes = possibleAppsTimes[order(possibleAppsTimes[c("appid", "unixTimestamp")]),]


possibleAppsTimes$degreeAtPriorTimestamp = shift(possibleAppsTimes$completeDegreeAtTimeT, -1)
possibleAppsTimes$appDummyCol = 1
numAppsWithDegreeDAtPriorTimestamp = aggregate(possibleAppsTimes$appDummyCol, by = list(possibleAppsTimes$unixTimestamp, possibleAppsTimes$degreeAtPriorTimestamp), FUN = sum)
setnames(numAppsWithDegreeDAtPriorTimestamp, "Group.1", "unixTimestamp")
setnames(numAppsWithDegreeDAtPriorTimestamp, "Group.2", "degreeAtPriorTimestamp")
setnames(numAppsWithDegreeDAtPriorTimestamp, "x", "numApps")

numAppsWithDegreeD$appDummyCol = 1
numAppsWithDegreeD = aggregate(possibleAppsTimes$appDummyCol, by = list(possibleAppsTimes$degreeAtPriorTimestamp), FUN = sum)
setnames(numAppsWithDegreeD, "Group.1", "degreeAtPriorTimestamp")
setnames(numAppsWithDegreeD, "x", "numApps")

#Example appid:
#View(possibleAppsTimes[which(possibleAppsTimes$appid == 284075743),])

#example appid:

#
View(prob_PA_a[which(prob_PA_a$degreeAtPriorTimestamp == 3449),])

possibleAppsTimes[which(possibleAppsTimes$degreeAtPriorTimestamp == 3980),]
#290338603
View(possibleAppsTimes[which(possibleAppsTimes$appid == 290338603),])




#Merge the numerators and denominaotrs by degree:
prob_PA_a = merge(numAppsWithDegreeD, numEdgesWithPriorTimeDegreeD, by = c("degreeAtPriorTimestamp"))
prob_PA_a$numEdges = as.double(prob_PA_a$numEdges)
prob_PA_a$prob_e_d = prob_PA_a$numEdges / prob_PA_a$numApps

ggplot() + geom_point(data = prob_PA_a, aes(x =degreeAtPriorTimestamp, y = prob_e_d )) + labs(x = "degree")
ggsave("Figure1_Attempt.jpg")
#ggplot() + geom_point(data = prob_PA_a[which(prob_PA_a$prob_e_d < 1),], aes(x =degreeAtPriorTimestamp, y = prob_e_d )) + labs(x = "degree")
#ggplot() + geom_point(data = prob_PA_a[which(prob_PA_a$prob_e_d < 1 & prob_PA_a$degreeAtPriorTimestamp < 5000),], aes(x =degreeAtPriorTimestamp, y = prob_e_d )) + labs(x = "degree")


#######
### B. Now, look at edges formed with other users (assuming the two-hop (user -> app -> fellow user of same app))
#######




#####################
## 4. Figure 2: Average number of edges created by a node of age a.
#####################







#####################
## 5. Figure 4: Number of edges E_h created to nodes h hops away.
#####################




#####################
## 5. Figure 5: Probability of linking to a random node h hops from source node.
#####################

#####################
## 5. Figure 6: Node lifetimes
#####################


#####################
## Diagnostics:
#####################

#1. see the distribution of timestamps per app:
appsTimestampsUnique = as.data.frame(unique(reviewMetaData[c("appid", "unixTimestamp")]))
appsTimestampsUnique$numTimesTest = 1
appNumTimestamps = aggregate(appsTimestampsUnique$numTimesTest, by = list(appsTimestampsUnique$appid), FUN = sum)


userAppEarliestEdges = as.data.frame(unique(userAppEarliestEdges[c("appid", "unixTimestamp")]))
userAppEarliestEdges$numTimesTest = 1
userAppEarliestEdges = aggregate(userAppEarliestEdges$numTimesTest, by = list(userAppEarliestEdges$appid), FUN = sum)

appDegreesPerTimestamp = as.data.frame(unique(appDegreesPerTimestamp[c("appid", "unixTimestamp")]))
appDegreesPerTimestamp$numTimesTest = 1
appDegreesPerTimestamp = aggregate(appDegreesPerTimestamp$numTimesTest, by = list(appDegreesPerTimestamp$appid), FUN = sum)







#####################
## Aux functions:
#####################


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




