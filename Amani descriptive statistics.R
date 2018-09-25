# IST687 – Introduction to Data Science
# Group Project - September 10th
# Descriptive Statistics
# ----------------------------------------------------------------------------------------------------------------------
# Step 1: Load packages, call libraries, and load dataset
Install.packages("magrittr")
install.packages("dplyr")
library("reshape2")
library("readxl")
library("ggplot2")
library("moments")
library("sqldf")
library("magrittr")
library("dplyr")
library("eeptools") #for the age calculation

# Load the dataset: Read the excel file IST687
prem1516 <- read.csv("http://www.football-data.co.uk/mmz4281/1516/E0.csv")
prem1617 <- read.csv("http://www.football-data.co.uk/mmz4281/1617/E0.csv")
prem1718 <- read.csv("http://www.football-data.co.uk/mmz4281/1718/E0.csv")

prem <- prem1516 %>% rbind(prem1617) %>% rbind (prem1718)

# Inspect the data using the str(), summary(), and View() functions
str(prem)
summary(prem)
View(prem)

# Data transformation Select Home Team and Home team data.
premHomeTeams <- prem %>% mutate(HomeAway = "Home") %>% select(Div, Date, Team1 = HomeTeam, 
                                                               Team2 = AwayTeam, HomeAway, GS = FTHG, GA = FTAG, FTR, HTGS = HTHG, HTGA = HTAG, 
                                                               HTR, Referee, ShotsTaken = HS, ShotsAllowed = AS, ShotsOnTarget = HST, OppShotsOnTarget = AST, 
                                                               FoulsCommitted = HF, FoulsReceived = AF, CornerKicks = HC, OppCornerKicks = AC, 
                                                               YellowCards = HY, OppYellowCards = AY, RedCards = HR, OppRedCards = AR)

# Select Away Team and Away team data.
premAwayTeams <- prem %>% mutate(HomeAway = "Away") %>% select(Div, Date, Team1 = AwayTeam, 
                                                               Team2 = HomeTeam, HomeAway, GS = FTAG, GA = FTHG, FTR, HTGS = HTAG, HTGA = HTHG, 
                                                               HTR, Referee, ShotsTaken = AS, ShotsAllowed = HS, ShotsOnTarget = AST, OppShotsOnTarget = HST, 
                                                               FoulsCommitted = AF, FoulsReceived = HF, CornerKicks = AC, OppCornerKicks = HC, 
                                                               YellowCards = AY, OppYellowCards = HY, RedCards = AR, OppRedCards = HR)

# Removing spaces from character  cols
premHomeTeams$Team1    <- gsub(" ", "", premHomeTeams$Team1)
premHomeTeams$Team2    <- gsub(" ", "", premHomeTeams$Team2)
premHomeTeams$HomeAway <- gsub(" ", "", premHomeTeams$HomeAway)

premAwayTeams$Team1    <- gsub(" ", "", premAwayTeams$Team1)
premAwayTeams$Team2    <- gsub(" ", "", premAwayTeams$Team2)
premAwayTeams$HomeAway <- gsub(" ", "", premAwayTeams$HomeAway)

# Bind datasets into a new dataset.
premTransformed <- premHomeTeams %>% rbind(premAwayTeams)

# Check the structure of the data, head, and see that there are no NAs.
str(premTransformed)
head(premTransformed)
summary(premTransformed)
View(premTransformed)

# Column names need to be changed to better understand the data.
colnames(premTransformed)

# ------------------------------------------------------------------------------------------
# The function performanceGoalsHomeAway computes totals and average of goals scored for and 
# against Home and Away teams
# 	GF: Goal For, GA: Goal Against,	AVG

performanceGoalsHomeAway <- function(x) {
  
  df <- x
  MeanShotsTaken        <-mean(df$ShotsTaken)
  MeanShotsOnTarget     <-mean(df$ShotsOnTarget)
  MeanCornerKicks       <-mean(df$CornerKicks)
  MeanFTGoals           <-mean(df$GS)
  MeanHTGoals           <-mean(df$HTGS)
  
  mStats <- data.frame(MeanShotsTaken , MeanShotsOnTarget, MeanCornerKicks, MeanFTGoals, MeanHTGoals)
  return(mStats)
}

# ------------------------------------------------------------------------------------------
# The function performanceCardsHomeAway computes average of cards received and fouls committed and 
# against Home and Away teams
# 	GF: Goal For, GA: Goal Against,	AVG

performanceCardsHomeAway <- function(x) {
  
  df <- x
  TotalGamesPlayed        <- nrow(df)
  TotalFoulsCommitted     <-sum(df$HF)
  TotalFoulsReceived      <-sum(df$AF)
  TotalYellowCards        <-sum(df$HY)
  TotalRedCards           <-sum(df$HR)
  TotalOppYellowCards     <-sum(df$AY)
  TotalOppRedCards        <-sum(df$AR)
  MeanFoulsCommitted      <-round(mean(df$HF), digits = 1)
  MeanFoulsReceived       <-round(mean(df$AF), digits = 1)
  MeanYellowCards         <-round(mean(df$HY), digits = 1)
  MeanRedCards            <-round(mean(df$HR), digits = 1) 
  MeanOppYellowCards      <-round(mean(df$AY), digits = 1) 
  MeanOppRedCards         <-round(mean(df$AR), digits = 1)
  
  
  mStats       <- data.frame(TotalGamesPlayed, TotalFoulsCommitted, TotalFoulsReceived, TotalYellowCards, TotalRedCards,
                    + TotalOppYellowCards, TotalOppRedCards, MeanFoulsCommitted, MeanFoulsReceived, MeanYellowCards,
                    + MeanOppYellowCards, MeanRedCards, MeanOppRedCards)
  return(mStats)
}

# ---------------------------------------------------------------------------------------------------------- 
# 1. The function descriptiveStatsInfo’ takes a dataframe as input, and calculates the followings:
#  a. Mean
#  b. Median
#  c. Min & max
#  d. Standard deviation
#  e. Quantiles (at 0.05 and 0.95)
#  f. Skewness
# ----------------------------------------------------------------------------------------------------------
descriptiveStatsInfo <- function(x) {
  #Computing statistical measurements for teams
  
  df <- x
  MeanFTWin         <- mean(df$TotalFTWin)
  MeanFTLoss	      <- mean(df$TotalFTLoss)
  MeanFTDraw        <- mean(df$TotalFTDraw)
  STDFTWin          <- sd(df$TotalFTWin, na.rm=TRUE)
  STDFTLoss	        <- sd(df$TotalFTLoss, na.rm=TRUE)
  STDFTDraw         <- sd(df$TotalFTDraw, na.rm=TRUE)
  MedianFTWin       <- median(df$TotalFTWin)
  MedianFTLoss	    <- median(df$TotalFTLoss)
  MedianFTDraw      <- median(df$TotalFTDraw)
  MinFTWin          <- min(df$TotalFTWin)
  MaxFTWin          <- max(df$TotalFTWin)
  MinFTLoss         <- min(df$TotalFTLoss)
  MaxFTLoss         <- max(df$TotalFTLoss)
  MinFTDraw         <- min(df$TotalFTDraw)
  MaxFTDraw         <- max(df$TotalFTDraw)
  
  QuantilesFTWinInfo     <- quantile(df$TotalFTWin, probs = c(0.05, 0.95))
  SkewnessFTWinInfo      <- skewness(df$TotalFTWin)
  QuantilesFTLossInfo    <- quantile(df$TotalFTLoss, probs = c(0.05, 0.95))
  SkewnessFTLossInfo     <- skewness(df$TotalFTLoss)
  QuantilesFTDrawInfo    <- quantile(df$TotalFTDraw, probs = c(0.05, 0.95))
  SkewnessFTDrawInfo     <- skewness(df$TotalFTDraw)
  
  mStats       <- data.frame(MeanFTWin, MeanFTLoss, MeanFTDraw, STDFTWin, STDFTLoss, STDFTDraw, MedianFTWin, MedianFTLoss, MedianFTDraw, 
                             +   MinFTWin, MaxFTWin, MinFTLoss, MaxFTLoss, MinFTDraw, MaxFTDraw, QuantilesFTWinInfo, SkewnessFTWinInfo, 
                             +   QuantilesFTLossInfo, SkewnessFTLossInfo, QuantilesFTDrawInfo, SkewnessFTDrawInfo)
  return(mStats)
  
}
# ------------------------------------------------------------------------------------------
# The function summaryStatsHomeAway computes totals for Home and Away teams
summaryStatsHomeAway <- function(x, mTeam1, mHomeAway) {
  
  df <- x
  HomeAway               <-mHomeAway 
  Team1                  <-mTeam1
  TotalGamesPlayed       <-nrow(df)
  TotalShotsTaken        <-sum(df$ShotsTaken)
  TotalShotsOnTarget     <-sum(df$ShotsOnTarget)
  TotalFoulsCommitted    <-sum(df$FoulsCommitted)
  TotalCornerKicks  <-sum(df$CornerKicks)
  TotalYellowCards  <-sum(df$YellowCards)
  TotalRedCards     <-sum(df$RedCards)
  TotalFTGoals      <-sum(df$GS)
  TotalHTGoals      <-sum(df$HTGS)
  df$FTR            <- gsub(" ", "", df$FTR)
  df$HTR            <- gsub(" ", "", df$HTR)
  TotalHTWin        <- length(which(df$HTR=="H" ))
  TotalHTLoss	      <- length(which(df$HTR=="A" ))
  TotalHTDraw       <- length(which(df$HTR=="D" ))
  TotalFTWin        <- length(which(df$FTR=="H" ))
  TotalFTLoss	      <- length(which(df$FTR=="A" ))
  TotalFTDraw       <- length(which(df$FTR=="D" ))
  mStats       <- data.frame(Team1, HomeAway, TotalShotsTaken , TotalShotsOnTarget, TotalFoulsCommitted, TotalCornerKicks, TotalYellowCards, TotalRedCards, 
                             +   TotalFTGoals,TotalHTGoals,TotalHTWin, TotalHTLoss, TotalHTDraw, TotalFTWin, TotalFTLoss, TotalFTDraw, TotalGamesPlayed)
  return(mStats)
}
# --------------------------------------------------------------------------------------------------------------------
# The function performanceHomeAdvantage computes the Home Field Advantage
performanceHomeAdvantage <- function(x) {
  # Home field advantage will be calculated as follows:
  # The team gets 3 points for a win, 1 point for a draw, and zero for a loss.
  # Calculate the ratio of total points earned from all matches/games played at home vs. away.
  # If the ratio is 50%, it indicates that success has been achieved equally at home and away i.e.there is no home field advantage.
  # If the ratio is above 50% then it indicates that playing at home field increases the winning probability.
  
  
  df <- x
  mMax = nrow(df)
  mCounter = 1
  HomeAdvantagedf <- data.frame()
  while (mCounter <= (mMax-1)) {
    TotalHomePoints <- 0
    TotalAwayPoints <- 0
    if(df$HomeAway[mCounter] == "Home") {
      TotalHomePoints <- df$TotalFTWin[mCounter] * 3 + df$TotalFTDraw[mCounter] * 1 
      TotalAwayPoints <- df$TotalFTWin[mCounter+1] * 3 + df$TotalFTDraw[mCounter] * 1
    }
    HomeAdvantage = round(TotalHomePoints / (df$TotalGamesPlayed[mCounter] * 3) * 100, digits = 1)
    AwayAdvantage = round(TotalAwayPoints / (df$TotalGamesPlayed[mCounter] * 3) * 100, digits = 1)
    
    mStats       <- data.frame(df$Team1[mCounter], HomeAdvantage, AwayAdvantage, df$TotalGamesPlayed[mCounter])
    HomeAdvantagedf  <-  HomeAdvantagedf %>% rbind(mStats)
    
    mCounter <- mCounter + 2
    print(mCounter)
  }
  
  return(HomeAdvantagedf)
}
# --------------------------------------------------------------------------------------------------------------------
#
## Step 2: Create a dataframe with all teams (Home and Away) participating in the UK premiere league and their totals.
premTeams <- gsub(" ", "", premTransformed$Team1)
premTeams <- unique(premTeams)
premStat <- premStat[order(premStat$Team1),]
premTeams

premHome <- premHomeTeams %>% mutate(HomeAway = "Home") %>% select(Date, Team1, Team2, HomeAway, GS, FTR, HTGS, HTR, ShotsTaken, ShotsOnTarget, FoulsCommitted, CornerKicks, YellowCards, RedCards)
premAway <- premAwayTeams %>% mutate(HomeAway = "Away") %>% select(Date, Team1, Team2, HomeAway, GS, FTR, HTGS, HTR, ShotsTaken, ShotsOnTarget, FoulsCommitted, CornerKicks, YellowCards, RedCards)

mCounter <- 1
mMax <-  length(premTeams)
premHomeTeamStat <- data.frame()
premAwayTeamStat <- data.frame()
premStat  <- data.frame()
while (mCounter <= mMax) {
  mTeam <- premTeams[mCounter]
  premHomeTeam <- premHome[ which(premHome$Team1 == mTeam),]
  premAwayTeam <- premAway[ which(premAway$Team1 == mTeam),]
  #premHomeTeamStat <- premHomeTeam[mCounter,2:4]  # to get the Team1, Team2, and HomeAway values
  #premAwayTeamStat <- premAwayTeam[mCounter,2:4]
  #premHomeTeamStat  <- premTeams[mCounter] 
  #premHomeTeamStat  <- premHomeTeamStat  
  #premAwayTeamStat  <- premTeams[mCounter]
  premHomeStatTemp  <- summaryStatsHomeAway(premHomeTeam, mTeam,"Home" )
  premAwayStatTemp  <- summaryStatsHomeAway(premAwayTeam, mTeam,"Away") 
  premHomeTeamStat  <- cbind(premHomeStatTemp)  
  premAwayTeamStat  <- cbind(premAwayStatTemp)
  premStat <- premStat  %>% rbind(premHomeTeamStat) 
  premStat <- premStat  %>% rbind(premAwayTeamStat)  
  mCounter <- mCounter + 1
  
}


# Renumber/reindex the row numbers
rownames(premStat) <- NULL
#premStat <- premStat[,-2:-2]   # removing Team2


# --------------------------------------------------------------------------------------------------------------------
## Step 3: calculate the descriptive & performance statistics for all Home and Away teams.

#  1- calculate average goals scorded for or against for Home and Away teams
premHomeTeamGoals <- performanceGoalsHomeAway(premHome)
premAwayTeamGoals <- performanceGoalsHomeAway(premAway)  

#  2- calculate average cards received and fouls committed for Home and Away teams
premTeamCards  <- performanceCardsHomeAway(prem)


#  3- calculate descriptive statistics for Home and Away teams
premHomedescriptiveStats <- data.frame()
premAwaydescriptiveStats <- data.frame()
premHomedescriptiveStats <- descriptiveStatsInfo(premStat[which(premStat$HomeAway == "Home"),])
premAwaydescriptiveStats <- descriptiveStatsInfo(premStat[which(premStat$HomeAway == "Away"),])


#  4- calculate Home Advantage
premHomeAdvantage  <- data.frame()
premStat <- premStat[order(premStat$Team1,premStat$HomeAway),]
premHomeAdvantage  <- performanceHomeAdvantage(premStat)
premHomeAdvantage %>% arrange(desc(HomeAdvantage), desc(AwayAdvantage))
