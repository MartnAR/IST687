readStates <- function(){
  url <- "http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"
  states_csv <- read.csv(url)
  return(states_csv)
}

census <- readStates()

census <- census[c(-1:-8, -60:-66), ]
census <- census[, -6:-10]

colnames(census) <- c('stateName','base2010','base2011','Jul2010','Jul2011')

dim(census)
head(census)

numberize <- function(dataset){
  cols <- colnames(dataset[, -1])
  for (i in cols){
    dataset[[i]] <- gsub(',','',dataset[[i]])
    dataset[[i]] <- gsub(' ','',dataset[[i]])
    dataset[[i]] <- as.numeric(as.character(dataset[[i]]))
  }
  return(dataset)
}

census <- numberize(census)

dfStates <- census
mean(dfStates$Jul2011)

perc_below <- function(vector, number){
  dat <- data.frame(dfStates[,vector][order(dfStates[, vector])])
  colnames(dat) <- vector
  tot <- nrow(dat)
  under <- sum(dat[, vector] <= number)
  perc_under <- under/tot
  return(perc_under)
}
