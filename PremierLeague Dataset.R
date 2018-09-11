require(dplyr)

prem <- read.csv("http://www.football-data.co.uk/mmz4281/1718/E0.csv")

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
write.csv(premUpdated, 'PremierLeague 2017-18.csv')
