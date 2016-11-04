
#Goal: test a few iterations of a basic generative graph process

library(data.table)
library(ggplot2)
library(sqldf)
library(MASS)
setwd('/Users/cperez/Desktop/iTunesData')

inputMetaData = prepareData("itunes3_reviews_meta.csv")
#appsWithCategories = read.csv("appCategories.csv")
N_minReviews = 2

appCatsSensorTower = read.csv("appCategories_SensorTowerOutput copy.csv")
appCatsSensorTower = as.data.frame(unique(appCatsSensorTower))
appCatsSensorTower[which(appCatsSensorTower$subcategory == "Games/Action"),]$subcategory = "Games"
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
#### 1. Simulate the model
#####################

T_timesteps = 100
min_Time = 1215648000

# At t = 0, create an initial number of nodes.
numNewApps = round(N_A_t(0))
numNewUsers = round(N_U_t(0))

currentApps = instantiateNewApps(0, numNewApps)
currentUsers = instantiateNewUsers(0,numNewUsers)


nextGenreUsers = currentUsers[which(currentUsers$userGenre==1),]
nextGenreApps = currentApps[which(currentApps$appGenre==1),]

nextGenreUsers$newAppIndex = sample(1:nrow(nextGenreApps), nrow(nextGenreUsers), replace = T)
nextGenreUsers$newAppId = lapply(nextGenreUsers$newAppIndex, function(x) nextGenreApps[x,1])
newEdges = nextGenreUsers[c("userid", "newAppId", "userGenre")]
colnames(newEdges) = c("userid", "appid", "edgeGenre")
newEdges$timestep = 0
currentEdges = newEdges


for (k in 2:6) {
  nextGenreUsers = currentUsers[which(currentUsers$userGenre==k),]
  nextGenreApps = currentApps[which(currentApps$appGenre==k),]
  
  nextGenreUsers$newAppIndex = sample(1:nrow(nextGenreApps), nrow(nextGenreUsers), replace = T)
  nextGenreUsers$newAppId = lapply(nextGenreUsers$newAppIndex, function(x) nextGenreApps[x,1])
  newEdges = nextGenreUsers[c("userid", "newAppId", "userGenre")]
  colnames(newEdges) = c("userid", "appid", "edgeGenre")
  newEdges$timestep = 0
  currentEdges = rbind(currentEdges, newEdges)
}



#At further timesteps, we sample new users and apps and add them to the current collection.

lastAppIndexUsed = 0
lastUserIndexUsed = 0

  
#Run the simulation for T_timesteps:
for (t in 1:T_timesteps) {
  print("timestep: ")
  print(t)
  #1. Create the new apps and users
  lastAppIndexUsed = numNewApps + lastAppIndexUsed
  lastUserIndexUsed = numNewUsers + lastUserIndexUsed
  
  numNewApps = round(N_A_t(t))
  numNewUsers = round(N_U_t(t))
  print("num new users:")
  print(numNewUsers)
  newApps = instantiateNewApps(lastAppIndexUsed, numNewApps)
  newUsers = instantiateNewUsers(lastUserIndexUsed,numNewUsers)

  currentApps = rbind(currentApps, newApps)
  currentUsers = rbind(currentUsers, newUsers)
  
  
  #2. Create edges based on PA within a genre, conditional on the users being alive.
  #a. sample users' chosen next genre based on 1-MM
  aliveUsers = currentUsers[which(currentUsers$aliveStatus == 1),]
  print("new alive users:")
  print(nrow(aliveUsers))
  aliveUsers$nextGenre = lapply(aliveUsers$userGenre, function(x) sample(c(1,2,3,4,5,6),1,prob=genreMarkovMatrix[,x])[1])

  aliveApps = currentApps[which(currentApps$aliveStatus == 1),]
  print("new alive apps:")
  print(nrow(aliveApps))
  
  #b. assign an edge according to PA within that genre
  
  #For now, join at random:
  #(implement PA later)
  for (k in 1:6) {
    nextGenreUsers = aliveUsers[which(aliveUsers$nextGenre==k),]
    nextGenreApps = aliveApps[which(aliveApps$appGenre==k),]
    
    nextGenreUsers$newAppIndex = sample(1:nrow(nextGenreApps), nrow(nextGenreUsers), replace = T)
    nextGenreUsers$newAppId = lapply(nextGenreUsers$newAppIndex, function(x) nextGenreApps[x,1])
    newEdges = nextGenreUsers[c("userid", "newAppId", "nextGenre")]
    colnames(newEdges) = c("userid", "appid", "edgeGenre")
    newEdges$timestep = t
    currentEdges = rbind(currentEdges, newEdges)
  }

  
  #c. sample a rating based on users' rating histograms.
  

  #3. Update everyone's lifespan
  currentApps$appLifetime = currentApps$appLifetime - 1
  currentUsers$userNumReviews = currentUsers$userNumReviews - 1
  
  currentApps[which(currentApps$appLifetime <= 0),]$aliveStatus = 0
  currentUsers[which(currentUsers$userNumReviews <= 0),]$aliveStatus = 0
  
}

currentEdges$edgeGenre = as.numeric(currentEdges$edgeGenre)
currentEdges$appid = as.numeric(currentEdges$appid)

write.csv(currentEdges, "SimulatedEdges_BasicModel.csv", row.names = F)


#sample ratings per edge:
#currentEdges$rating = lapply(currentEdges$edgeGenre, function(x) sample(c(1,2,3,4,5,6),1,prob=genreMarkovMatrix[,x])[1])


#Compare time series of number of edges:
inputMetaData$weeks = round(((inputMetaData$unixTimestamp - min_Time)/(24*60*60*7)))
inputMetaData$edgeDummyCol = 1
realEdgesPerWeek = aggregate(inputMetaData$edgeDummyCol, by = list(inputMetaData$weeks), FUN = sum)
setnames(realEdgesPerWeek, "Group.1", "weekIndex")
setnames(realEdgesPerWeek, "x", "numEdges")


currentEdges$edgeDummyCol = 1
simulatedEdgesPerWeek = aggregate(currentEdges$edgeDummyCol, by = list(currentEdges$timestep), FUN = sum)
setnames(simulatedEdgesPerWeek, "Group.1", "weekIndex")
setnames(simulatedEdgesPerWeek, "x", "numEdges")

ggplot() + 
  geom_line(data = realEdgesPerWeek[which(realEdgesPerWeek$weekIndex >= 50),], aes(x =weekIndex, y = numEdges ))  + 
  geom_point(data = simulatedEdgesPerWeek, aes(x =weekIndex, y = numEdges )) 
  labs(x = "weekIndex", title = "Number of edges per week")


#####################
#### 2. Calculate histograms, MM's, and lamdbas for the Basic model
#####################


#N_U(t):
#Call:
#lm(formula = usersWithMinTimestamps$cumulativeNumUsers ~ usersWithMinTimestamps$weeks + 
#    usersWithMinTimestamps$weeks_sq)
#Coefficients:
#                    (Intercept)     usersWithMinTimestamps$weeks  usersWithMinTimestamps$weeks_sq  
#                       39058.61                         -1593.13                            19.46  


#N_A(t):
#Call:
#lm(formula = appsWithMinTimestamps$cumulativeNumApps ~ appsWithMinTimestamps$weeks + 
#    appsWithMinTimestamps$weeks_sq)
#Coefficients:
#                   (Intercept)     appsWithMinTimestamps$weeks  appsWithMinTimestamps$weeks_sq  
#                      232.7242                         -6.4042                          0.2307  


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



## Overall genre histogram:

 #            Books           Business           Catalogs          Education      Entertainment            Finance       Food & Drink              Games 
 #               32                 12                  1                 94                 31                  6                  4                406 
 #     Games/Trivia         Games/Word   Health & Fitness          Lifestyle            Medical              Music         Navigation               News 
 #                0                  0                 13                545                  3                128                  7                 43 
 #        Newsstand          NoneFound      Photo & Video       Productivity          Reference  Social Networking             Sports             Travel 
 #                1                  0                 86                 27                 55                 98                 22                 27 
 #        Utilities            Weather 
 #              232                  1 





 #            Books           Business           Catalogs          Education      Entertainment            Finance       Food & Drink              Games 
 #              
 #     Games/Trivia         Games/Word   Health & Fitness          Lifestyle            Medical              Music         Navigation               News 
 #                0                  0                                                
 #        Newsstand          NoneFound      Photo & Video       Productivity          Reference  Social Networking             Sports             Travel 
 #                1                  0                                                                               
 #        Utilities            Weather 
 #             


### Users:


#             Books           Business           Catalogs          Education      Entertainment            Finance       Food & Drink              Games 
#                90                  4                  0                499                109                166                  5               3495 
##      Games/Action    Games/Adventure       Games/Arcade        Games/Board         Games/Card       Games/Casino         Games/Dice  Games/Educational 
 #                                0                  0                  0                  0                  0                  0                  0 
 #     Games/Family        Games/Music       Games/Puzzle       Games/Racing Games/Role Playing   Games/Simulation       Games/Sports     Games/Strategy 
 ##                0                  0                  0                  0                  0                  0                  0                  0 
  #    Games/Trivia         Games/Word   Health & Fitness          Lifestyle            Medical              Music         Navigation               News 
  ##               0                  0                 57               4516                  6                562                  3                121 
   #      Newsstand          NoneFound      Photo & Video       Productivity          Reference  Social Networking             Sports             Travel 
   #              0                  0               1546                106                163                642                  4                 29 
   #      Utilities            Weather 
   #           1464                 19 


#             Books           Business           Catalogs          Education      Entertainment            Finance       Food & Drink              Games 
#                90                  4                  0                499                109                166                  5               3495 
##      Games/Action    Games/Adventure       Games/Arcade        Games/Board         Games/Card       Games/Casino         Games/Dice  Games/Educational 
 #                                0                  0                  0                  0                  0                  0                  0 
 #     Games/Family        Games/Music       Games/Puzzle       Games/Racing Games/Role Playing   Games/Simulation       Games/Sports     Games/Strategy 
 ##                0                  0                  0                  0                  0                  0                  0                  0 
  #    Games/Trivia         Games/Word   Health & Fitness          Lifestyle            Medical              Music         Navigation               News 
  ##               0                  0                 57               4516                  6                562                  3                121 
   #      Newsstand          NoneFound      Photo & Video       Productivity          Reference  Social Networking             Sports             Travel 
   #              0                  0               1546                106                163                642                  4                 29 
   #      Utilities            Weather 
   #           1464                 19 



#####################
#### 3. Auxiliary functions
#####################

### Create 1-MM matrix between genres:

createGenreMMMatrix <- function() {
  numGenres = 6
  genreMarkovMatrix = matrix(0,numGenres,numGenres)

  inputMetaData$coarseSubcat = 0
  inputMetaData[which(inputMetaData$subcategory == "Games"),]$coarseSubcat = 1
  inputMetaData[which(inputMetaData$subcategory == "Entertainment" | inputMetaData$subcategory == "Music"),]$coarseSubcat = 2
  inputMetaData[which(inputMetaData$subcategory == "Reference" | inputMetaData$subcategory == "Books" | inputMetaData$subcategory == "Business" | inputMetaData$subcategory == "Education" | inputMetaData$subcategory == "News"),]$coarseSubcat = 3
  inputMetaData[which(inputMetaData$subcategory == "Lifestyle" | inputMetaData$subcategory == "Socal Networking" | inputMetaData$subcategory == "Photo & Video"),]$coarseSubcat = 4
  inputMetaData[which(inputMetaData$subcategory == "Health & Fitness" | inputMetaData$subcategory == "Navigation" | inputMetaData$subcategory == "Medical" | inputMetaData$subcategory == "Travel" ),]$coarseSubcat = 5
  inputMetaData[which(inputMetaData$subcategory == "Utilities" | inputMetaData$subcategory == "Travel"),]$coarseSubcat = 6
  
  genreTransitionData = inputMetaData[which(inputMetaData$coarseSubcat > 0),]
  
  genreTransitions = merge(genreTransitionData[c("userid", "coarseSubcat", "unixTimestamp")], genreTransitionData[c("userid", "coarseSubcat", "unixTimestamp")], by = "userid")
  
  genreTransitions = genreTransitions[which(genreTransitions$unixTimestamp.y > genreTransitions$unixTimestamp.x),]
  
  genreTransitions$dummyCol = 1
  genreTransitions = aggregate(genreTransitions$dummyCol, by = list(genreTransitions$coarseSubcat.x, genreTransitions$coarseSubcat.y), FUN = sum)

return(genreMarkovMatrix)
}

for (i in 1:nrow(genreTransitions)) {
  originEntry = genreTransitions[i,1]
  destEntry = genreTransitions[i,2]
  numTransitions = genreTransitions[i,3]
  genreMarkovMatrix[destEntry, originEntry] = numTransitions
}
for (i in 1:numGenres) {
  colSum = sum(genreMarkovMatrix[,i])
  genreMarkovMatrix[,i] = genreMarkovMatrix[,i]/colSum
}


## Create numNewUsers new users, with user ids starting at lastIndexUsed

instantiateNewUsers <- function(lastIndexUsed, numNewUsers) {
  newUserData = as.data.frame(matrix(lastIndexUsed,numNewUsers,2 ))
  newUserData$rownumber = 1:nrow(newUserData)
  newUserData$userid = newUserData$V1 + newUserData$rownumber
  #Sample their lifetimes:
  #(lambda was initially estimated at the daily level. It needs to be reestimated at the weekly level.)
  
  numReviewsProbVector = c(131073  , 5164  ,  673  ,  156  ,   37    ,15 )
  numReviewsProbVector = numReviewsProbVector/sum(numReviewsProbVector)
  cutoffs = numReviewsProbVector*nrow(newUserData)
  cutoffs[2] = cutoffs[2] + cutoffs[1]
  cutoffs[3] = cutoffs[3] + cutoffs[2]
  cutoffs[4] = cutoffs[4] + cutoffs[3] 
  cutoffs[5] = cutoffs[5] + cutoffs[4]
  
  newUserData$userNumReviews = 1
  newUserData[which(newUserData$rownumber >= cutoffs[1] & newUserData$rownumber < cutoffs[2] ),]$userNumReviews = 2
  newUserData[which(newUserData$rownumber >= cutoffs[2] & newUserData$rownumber < cutoffs[3] ),]$userNumReviews = 3
  newUserData[which(newUserData$rownumber >= cutoffs[3] & newUserData$rownumber < cutoffs[4] ),]$userNumReviews = 4
  newUserData[which(newUserData$rownumber >= cutoffs[4] & newUserData$rownumber < cutoffs[5] ),]$userNumReviews = 5
  newUserData[which(newUserData$rownumber >= cutoffs[5]  ),]$userNumReviews = 6
  
  
  #Sample their genre:
  #Coalesced cats: Games, Entertainment (Ent, Music), Reference (Ref, Books, Biz, Educ, News), Lifestyle (Lifestyle, Social Net, Photo), Health/Travel (Health, Nav, Medical, Trav), Utilities (Ut. and prod)
  
  newUserData <- newUserData[sample(1:nrow(newUserData)), ]
  
  genreProbVector = c(3495, 109+562, 163+90+4+499+121, 4516+642+1546, 57+3+6+29, 1464+106 )

  genreProbVector = genreProbVector/sum(genreProbVector)
  cutoffs = genreProbVector*nrow(newUserData)
  cutoffs[2] = cutoffs[2] + cutoffs[1]
  cutoffs[3] = cutoffs[3] + cutoffs[2]
  cutoffs[4] = cutoffs[4] + cutoffs[3] 
  cutoffs[5] = cutoffs[5] + cutoffs[4]
  
  newUserData$userGenre = 1
  newUserData[which(newUserData$rownumber >= cutoffs[1] & newUserData$rownumber < cutoffs[2] ),]$userGenre = 2
  newUserData[which(newUserData$rownumber >= cutoffs[2] & newUserData$rownumber < cutoffs[3] ),]$userGenre = 3
  newUserData[which(newUserData$rownumber >= cutoffs[3] & newUserData$rownumber < cutoffs[4] ),]$userGenre = 4
  newUserData[which(newUserData$rownumber >= cutoffs[4] & newUserData$rownumber < cutoffs[5] ),]$userGenre = 5
  newUserData[which(newUserData$rownumber >= cutoffs[5]  ),]$userGenre = 6

  newUserData$aliveStatus = 1

  return(newUserData[c("userid", "userNumReviews", "aliveStatus", "userGenre")])
}







#Create numNewApps new apps, with app ids starting at lastIndexUsed
#Final output has three columns: app id, their sampled lifetimes, and their alive status
instantiateNewApps <- function(lastIndexUsed, numNewApps) {
  newAppData = as.data.frame(matrix(lastIndexUsed,numNewApps,2 ))
  newAppData$rownumber = 1:nrow(newAppData)
  newAppData$appid = newAppData$V1 + newAppData$rownumber
  #Sample their lifetimes:
  #(lambda was initially estimated at the daily level. It needs to be reestimated at the weekly level.)
  lambda_appLifetime = 7*.0024415
  newAppData$appLifetime = rexp(nrow(newAppData), rate = lambda_appLifetime)
  newAppData$aliveStatus = 1
  #Sample their genre:
  #Coalesced cats: Games, Entertainment (Ent, Music), Reference (Ref, Books, Biz, Educ, News), Lifestyle (Lifestyle, Social Net, Photo), Health/Travel (Health, Nav, Medical, Trav), Utilities (Ut. and prod)
  genreProbVector = c(406, 159, 236, 729, 65, 259)

  genreProbVector = genreProbVector/sum(genreProbVector)
  cutoffs = genreProbVector*nrow(newAppData)
  cutoffs[2] = cutoffs[2] + cutoffs[1]
  cutoffs[3] = cutoffs[3] + cutoffs[2]
  cutoffs[4] = cutoffs[4] + cutoffs[3] 
  cutoffs[5] = cutoffs[5] + cutoffs[4]
  
  newAppData$appGenre = 1
  newAppData[which(newAppData$rownumber >= cutoffs[1] & newAppData$rownumber < cutoffs[2] ),]$appGenre = 2
  newAppData[which(newAppData$rownumber >= cutoffs[2] & newAppData$rownumber < cutoffs[3] ),]$appGenre = 3
  newAppData[which(newAppData$rownumber >= cutoffs[3] & newAppData$rownumber < cutoffs[4] ),]$appGenre = 4
  newAppData[which(newAppData$rownumber >= cutoffs[4] & newAppData$rownumber < cutoffs[5] ),]$appGenre = 5
  newAppData[which(newAppData$rownumber >= cutoffs[5]  ),]$appGenre = 6


  return(newAppData[c("appid", "appLifetime", "aliveStatus", "appGenre")])
}



N_U_t <- function(t) {
  newNumUsers = 19.46*(t^2) + -1593.13*(t) + 39058.61
  return(newNumUsers)
}

N_A_t <- function(t) {
  newNumApps = 0.2307*(t^2) + -6.4042*(t) + 232.7242
  return(newNumApps)
}


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

sampleDist_genreTransition = function(probVector) { 
    sampledGenre = sample(x = c(1,2,3,4,5,6), 1, replace = T, prob = probVector) 
    return(sampledGenre)
}



#####################
#### Scrap
#####################


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

