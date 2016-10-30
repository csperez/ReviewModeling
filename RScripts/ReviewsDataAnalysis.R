
library(data.table)
library(ggplot2)
library(sqldf)
setwd('/Users/cperez/Desktop/iTunesData')

## Todo list:
#Polish:

#For app nodes:
#1. p_e(d)
#2. Figure 2

#For users as nodes:


#Create:
#1. app node lifetimes
#2. user node lifetimes

#3. number of app nodes over time
#4. number of user nodes over time



#####################
### Quick stats
#####################

#1. number of userids:
#542k
nrow(as.data.frame(unique(reviewMetaData$userid)))
#2. number of appids:
#8.057k
nrow(as.data.frame(unique(reviewMetaData$appid)))


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
appMaxTimes = aggregate(appDegreesPerTimestamp$unixTimestamp, by = list(appDegreesPerTimestamp$appid), FUN = max)
setnames(appMaxTimes, "Group.1", "appid")
setnames(appMaxTimes, "x", "maxTime")


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




### Instead, averaging point estimates:
#Want to merge on timestamp and unixTimestamp:


possibleAppsTimes$appDummyCol = 1
numAppsWithDegreeD_t = aggregate(possibleAppsTimes$appDummyCol, by = list(possibleAppsTimes$unixTimestamp, possibleAppsTimes$degreeAtPriorTimestamp), FUN = sum)
setnames(numAppsWithDegreeD_t, "Group.1", "unixTimestamp")
setnames(numAppsWithDegreeD_t, "Group.2", "degreeAtPriorTimestamp")
setnames(numAppsWithDegreeD_t, "x", "numApps")


numEdgesWithPriorTimeDegreeD_t = aggregate(possibleAppsTimes$numNewUsersAtTimestamp, by = list(possibleAppsTimes$unixTimestamp, possibleAppsTimes$degreeAtPriorTimestamp), FUN = sum)
setnames(numEdgesWithPriorTimeDegreeD_t, "Group.1", "unixTimestamp")
setnames(numEdgesWithPriorTimeDegreeD_t, "Group.2", "degreeAtPriorTimestamp")
setnames(numEdgesWithPriorTimeDegreeD_t, "x", "numEdges")

totalNumEdges_t = aggregate(possibleAppsTimes$numNewUsersAtTimestamp, by = list(possibleAppsTimes$unixTimestamp), FUN = sum)
setnames(totalNumEdges_t, "Group.1", "unixTimestamp")
setnames(totalNumEdges_t, "x", "totalNumEdges")


prob_PA_t = merge(totalNumEdges_t, numEdgesWithPriorTimeDegreeD_t, by = c( "unixTimestamp"))
prob_PA_t$numEdges = as.double(prob_PA_t$numEdges)
prob_PA_t$totalNumEdges = as.double(prob_PA_t$totalNumEdges)

prob_PA_t$prob_e_d = prob_PA_t$numEdges / prob_PA_t$totalNumEdges

#Adding the denom:
possibleAppsTimes$appDummyCol = 1
numAppsWithDegreeD = aggregate(possibleAppsTimes$appDummyCol, by = list(possibleAppsTimes$degreeAtPriorTimestamp), FUN = sum)
setnames(numAppsWithDegreeD, "Group.1", "degreeAtPriorTimestamp")
setnames(numAppsWithDegreeD, "x", "numApps")
#Calculating the numerator:
prob_PA_SummedOverTime = aggregate(prob_PA_t$prob_e_d, by = list(prob_PA_t$degreeAtPriorTimestamp), FUN = sum)
setnames(prob_PA_SummedOverTime, "Group.1", "degreeAtPriorTimestamp")
setnames(prob_PA_SummedOverTime, "x", "totalProbabilitySum")

prob_PA_SummedOverTime = merge(prob_PA_SummedOverTime, numAppsWithDegreeD, by = c( "degreeAtPriorTimestamp"))
prob_PA_SummedOverTime$prob_e_d = prob_PA_SummedOverTime$totalProbabilitySum / prob_PA_SummedOverTime$numApps

prob_PA_SummedOverTime$log_prob_e_d = log(prob_PA_SummedOverTime$prob_e_d)

ggplot() + geom_point(data = prob_PA_SummedOverTime, aes(x = log(degreeAtPriorTimestamp), y = log_prob_e_d )) + labs(x = "degree") +
  labs(title = "Figure 1: Probability p_e(d) of a new edge e choosing a destination at a node of degree d")

ggsave("Figure1_ProbabilityOfEdgeEChoosingDestinationWithDegreeD.png")


#######
### B. Now, look at edges formed with other users (assuming the two-hop (user -> app -> fellow user of same app))
#######




#####################
## 4. Figures 2 and 7: Average number of edges created by a node of age a, exponential model of node ages.
#####################
#Note: for our context, we should probably do this separately for reviwers and apps

#A. First, do this only for apps
#1. attach the min times per app to get the age of an app:
appNumEdgesByAge = possibleAppsTimes[which(is.na(possibleAppsTimes$appid) == F),]
appNumEdgesByAge = merge(appNumEdgesByAge, appMaxTimes, by = c("appid"))
appNumEdgesByAge$appAgeInWeeks = round(((appNumEdgesByAge$unixTimestamp - appNumEdgesByAge$minTime)/(24*60*60*7)))
appNumEdgesByAge$appLifetime = round(((appNumEdgesByAge$maxTime - appNumEdgesByAge$minTime)/(24*60*60)))


#2. Calculate the numerator:
averageCreatedEdgesPerWeeks = aggregate(appNumEdgesByAge$numNewUsersAtTimestamp, by = list(appNumEdgesByAge$appAgeInWeeks), FUN = sum)
setnames(averageCreatedEdgesPerWeeks, "Group.1", "appAgeInWeeks")
setnames(averageCreatedEdgesPerWeeks, "x", "numNewUsersAtTimestamp")

#3. Calculate the denominator:
#To get all nodes that ever achieved a specific age a, we do a cross join on (all possible ages) x (apps's max ages):
maxAppAges = aggregate(appNumEdgesByAge$appAgeInWeeks, by = list(appNumEdgesByAge$appid), FUN = max)
setnames(maxAppAges, "Group.1", "appid")
setnames(maxAppAges, "x", "maxAppAge")

allPossibleAges = as.data.frame(unique(averageCreatedEdgesPerWeeks$appAgeInWeeks))
colnames(allPossibleAges) = c("possibleAge")

numAppsAchievingAge = merge(allPossibleAges, maxAppAges, by = NULL)
numAppsAchievingAge = numAppsAchievingAge[which(numAppsAchievingAge$maxAppAge >= numAppsAchievingAge$possibleAge),]
numAppsAchievingAge$appDummyCol = 1
numAppsAchievingAge = aggregate(numAppsAchievingAge$appDummyCol, by = list(numAppsAchievingAge$possibleAge), FUN = sum)
setnames(numAppsAchievingAge, "Group.1", "possibleAge")
setnames(numAppsAchievingAge, "x", "numAppsAchievingAge")

#4. merge the numerators and the denoms, and then calculate the avg. number of edges:
avgCreatedEdgesAtAgeA = merge(averageCreatedEdgesPerWeeks, numAppsAchievingAge, by.x = c("appAgeInWeeks"), by.y = c("possibleAge"))
avgCreatedEdgesAtAgeA$avgNumEdgesCreatedAtAgeA = avgCreatedEdgesAtAgeA$numNewUsersAtTimestamp / avgCreatedEdgesAtAgeA$numAppsAchievingAge

ggplot() + geom_point(data = avgCreatedEdgesAtAgeA, aes(x =appAgeInWeeks, y = avgNumEdgesCreatedAtAgeA )) + labs(x = "Node age (weeks)") + 
  labs(title = "Figure 2: Average number of edges created by nodes at age a",  x = "Node age (weeks)")
ggsave("AvgNumEdgesCreatedByNodesAtAgeA_all.png")
#Without outliers:
ggplot() + geom_point(data = avgCreatedEdgesAtAgeA[which(avgCreatedEdgesAtAgeA$appAgeInWeeks < 175),], aes(x =appAgeInWeeks, y = avgNumEdgesCreatedAtAgeA )) + labs(x = "Node age (weeks)") +
  labs(title = "Figure 2: Average number of edges created by nodes at age a, app age < 175 weeks",  x = "Node age (weeks)")
ggsave("AvgNumEdgesCreatedByNodesAtAgeA_lessThan175Weeks.png")

ggplot() + geom_point(data = avgCreatedEdgesAtAgeA[which(avgCreatedEdgesAtAgeA$appAgeInWeeks < 150),], aes(x =appAgeInWeeks, y = avgNumEdgesCreatedAtAgeA )) + 
  labs(title = "Figure 2: Average number of edges created by nodes at age a, app age < 150 weeks",  x = "Node age (weeks)")
ggsave("AvgNumEdgesCreatedByNodesAtAgeA_lessThan150Weeks.png")



### Fitting an exponential model to node ages:
fit1 <- fitdistr(appNumEdgesByAge$appLifetime, "exponential") 
lambda = .0024415

appNumEdgesByAge$lifetimeExponentialFit = lambda*exp(-lambda*appNumEdgesByAge$appLifetime)

avgLifetimeExponentialFits = aggregate(appNumEdgesByAge$lifetimeExponentialFit, by = list(appNumEdgesByAge$appLifetime), FUN = mean)
setnames(avgLifetimeExponentialFits, "Group.1", "appLifetime")
setnames(avgLifetimeExponentialFits, "x", "avgLifetimeExponentialFit")

ggplot() + geom_point(data = avgLifetimeExponentialFits, aes(x =appLifetime, y = log(avgLifetimeExponentialFit) )) + 
  labs(title = "Figure 7: Exponentially-distributed node lifetimes",  x = "Node lifetime (days)")
ggsave("ExponentiallyDistLifetimesByDays.png")





#####################
## 5. Figure 4: Number of edges E_h created to nodes h hops away.
#####################
# Hmm, the notion of hops for users-apps edges seems a bit tricky.
# An app two-hops away requires that the user is connected via: user -> app_1 -> otherUserUsingApp_1 -> app_2





#####################
## 5. Figure 5: Probability of linking to a random node h hops from source node.
#####################



#####################
## 5. Figure 6: Node lifetimes
#####################

#
  



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

#Number of unique users per app:
userAppEarliestEdges$testCol = 1
numUsersPerApp = aggregate(userAppEarliestEdges$testCol, by = list(userAppEarliestEdges$appid), FUN = sum)
setnames(numUsersPerApp, "Group.1", "appid")
setnames(numUsersPerApp, "x", "numUsers")

summary(numUsersPerApp$x)
quants_numUsersApp = quantile(numUsersPerApp$x, probs = c(.8, .9, .95, .975, .99, .995))

#####################
### 6. Creating the user-user graph using app-hops
#####################

#1. add a threshold parameter for edges between users only if both have reviewed at least n apps
# user_a -> app_1 -> user_b, with optional parameter on # of common apps reviewed.

#So, we create a (user,user) edgelist with a attribute on # of apps reviewd.
#We do this using a self-join:

#(appid, userid, (earliest) unixTimestamp)

#userEdgeList = merge(userAppEarliestEdges, userAppEarliestEdges, by = c("appid"))

userAppEarliestEdges = merge(userAppEarliestEdges, numUsersPerApp, by = c("appid"))

#> quants_numUsersApp
#80%    90%    95%  97.5%    99%  99.5% 
#  14.0   42.0  120.0  360.0 1360.2 2639.4 
userAppEarliestEdges_lo = userAppEarliestEdges[which(userAppEarliestEdges$numUsers <= 10000),]
userAppEarliestEdges_hi = userAppEarliestEdges[which(userAppEarliestEdges$numUsers > 10000),]


userEdgeList_lo = sqldf('select A.userid AS userid_A, B.userid AS userid_B, COUNT(*) AS numCommonApps from userAppEarliestEdges_lo AS A INNER JOIN userAppEarliestEdges_lo AS B 
                        ON A.appid = B.appid AND A.userid > B.userid GROUP BY A.userid, B.userid')


userEdgeList_hi = sqldf('select A.userid AS userid_A, B.userid AS userid_B, COUNT(*) AS numCommonApps from userAppEarliestEdges_hi AS A INNER JOIN userAppEarliestEdges_hi AS B 
                        ON A.appid = B.appid AND A.userid > B.userid GROUP BY A.userid, B.userid')




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




