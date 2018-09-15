require(dplyr)

prem1516 <- read.csv("http://www.football-data.co.uk/mmz4281/1516/E0.csv")
prem1617 <- read.csv("http://www.football-data.co.uk/mmz4281/1617/E0.csv")
prem1718 <- read.csv("http://www.football-data.co.uk/mmz4281/1718/E0.csv")

prem <- prem1516 %>% bind_rows(prem1617) %>% bind_rows(prem1718)

premHomeTeams <- prem %>% 
  mutate(HomeAway = 'Home') %>% 
  select(Div, Date, Team1 = HomeTeam, Team2 = AwayTeam, HomeAway, GS = FTHG, GA = FTAG, FTR, 
         HTGS = HTHG, HTGA = HTAG, HTR, Referee, ShotsTaken = HS, ShotsAllowed = AS, ShotsOnTarget = HST, 
         ShotsOnTargetAllowed = AST, FoulsCommited = HF, FoulsReceived = AF, CornerKicks = HC, 
         CornerKicksAllowed = AC, YellowCards = HY, YellowCardsOpp = AY, RedCards = HR, RedCardsOpp = AR)
premAwayTeams <- prem %>% 
  mutate(HomeAway = 'Away') %>% 
  select(Div, Date, Team1 = AwayTeam, Team2 = HomeTeam, HomeAway, GS = FTAG, GA = FTHG, FTR, 
         HTGS = HTAG, HTGA = HTHG, HTR, Referee, ShotsTaken = AS, ShotsAllowed = HS, ShotsOnTarget = AST, 
         ShotsOnTargetAllowed = HST, FoulsCommited = AF, FoulsReceived = HF, CornerKicks = AC, 
         CornerKicksAllowed = HC, YellowCards = AY, YellowCardsOpp = HY, RedCards = AR, RedCardsOpp = HR)

premUpdated <- premHomeTeams %>% bind_rows(premAwayTeams)
write.csv(premUpdated, 'PremierLeague 2015-18.csv')
