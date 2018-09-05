## Historical Odds

library(readxl)
library(tidyverse)
library(lubridate)

## Read in historical odds data

odds_archives <- bind_rows(
          read_excel("./data/odds_archives/nfl odds 2007-08.xlsx",col_types = "text") %>% mutate("Season" = "2007"),
          read_excel("./data/odds_archives/nfl odds 2008-09.xlsx",col_types = "text") %>% mutate("Season" = "2008"),
          read_excel("./data/odds_archives/nfl odds 2009-10.xlsx",col_types = "text") %>% mutate("Season" = "2009"),
          read_excel("./data/odds_archives/nfl odds 2010-11.xlsx",col_types = "text") %>% mutate("Season" = "2010"),
          read_excel("./data/odds_archives/nfl odds 2011-12.xlsx",col_types = "text") %>% mutate("Season" = "2011"),
          read_excel("./data/odds_archives/nfl odds 2012-13.xlsx",col_types = "text") %>% mutate("Season" = "2012"),
          read_excel("./data/odds_archives/nfl odds 2013-14.xlsx",col_types = "text") %>% mutate("Season" = "2013"),
          read_excel("./data/odds_archives/nfl odds 2014-15.xlsx",col_types = "text") %>% mutate("Season" = "2014"),
          read_excel("./data/odds_archives/nfl odds 2015-16.xlsx",col_types = "text") %>% mutate("Season" = "2015"),
          read_excel("./data/odds_archives/nfl odds 2016-17.xlsx",col_types = "text") %>% mutate("Season" = "2016"),
          read_excel("./data/odds_archives/nfl odds 2017-18.xlsx",col_types = "text") %>% mutate("Season" = "2017")
          )

## Rename teams to link to the nflscrapr abbreviations
odds_archives <- odds_archives %>% 
  mutate(Team = case_when(Team == "NewOrleans"~"NO",
                          Team == "Indianapolis"~"IND",
                          Team == "KansasCity"~"KC",
                          Team == "HoustonTexans"~"HOU", 
                          Team == "Denver"~ "DEN",   
                          Team == "Buffalo"~ "BUF",
                          Team == "Pittsburgh"~ "PIT",
                          Team == "Cleveland"~ "CLE",  
                          Team == "Tennessee"~"TEN",
                          Team == "Jacksonville"~ "JAC", 
                          Team ==  "Carolina"~ "CAR",  
                          Team == "St.Louis"~ "LA",   
                          Team == "Philadelphia"~"PHI", 
                          Team == "GreenBay"~"GB",  
                          Team == "Atlanta"~"ATL",
                          Team == "Minnesota"~"MIN",    
                          Team == "Miami"~"MIA",
                          Team == "Washington"~"WAS",
                          Team == "NewEngland"~"NE",  
                          Team == "NYJets"~"NYJ",
                          Team == "TampaBay"~"TB",
                          Team == "Seattle"~"SEA",
                          Team == "Chicago"~"CHI",
                          Team == "SanDiego"~"LAC",
                          Team == "Detroit"  ~"DET",
                          Team == "Oakland" ~"OAK",
                          Team == "NYGiants"~"NYG",
                          Team == "Dallas"~"DAL",
                          Team == "Baltimore"~"BAL",
                          Team == "Cincinnati" ~"CIN", 
                          Team == "Arizona"~"ARI",
                          Team == "SanFrancisco"~"SF",
                          Team == "Houston"~"HOU",
                          Team == "BuffaloBills"~"BUF", 
                          Team == "NewYork"  ~ "NYG",
                          Team == "LosAngeles" ~ "LA", 
                          Team == "LARams"~ "LA",
                          Team == "LAChargers" ~ "LAC",
                          TRUE ~ "Unknown"))

## Format Dates
odds_archives <- odds_archives %>% 
  mutate("new_date" = lubridate::mdy(paste0(str_pad(Date,pad="0",4),Season)),
         "month" = month(new_date),
         "Date" = case_when(month %in% c(1,2,3) ~ new_date+years(1),
                             month %in% c(4,5,6,7,8,9,10,11,12) ~ new_date)) 


## Replace Pickem values (pk) with zeroes.
odds_archives <- odds_archives %>% 
  mutate("Open" = ifelse(Open == "pk","0",Open),
         "Close" = ifelse(Close == "pk","0",Close))

## Update numeric data types that are currently character vectors
odds_archives <- odds_archives %>% 
  mutate(Open = as.numeric(Open),
         Close = as.numeric(Close),
         ML = as.numeric(ML))


## Add a game_id column
odds_archives <- odds_archives %>% 
  mutate("game_id" = paste0("G-",rep(seq(0,(nrow(odds_archives)/2-1)),each = 2))) 
  

## Pare down the dataframe to include the core fields
odds_summary <- odds_archives %>% 
  select(game_id,Date,Team,Open,Close,ML)

# Create a wide format version of the dataset where each row is a single contest
game_subset_one <- slice(odds_summary, seq(1, nrow(odds_summary), 2))
game_subset_two <- slice(odds_summary, seq(2, nrow(odds_summary), 2))
hist_results_wide <- left_join(game_subset_one, game_subset_two, by = c("game_id","Date"), suffix = c("1","2"))

rm(list=c("game_subset_one","game_subset_two","odds_archives"))

# Determine which team was favored
hist_results_wide <- hist_results_wide %>% 
  mutate("OpenFav" = case_when(Open1 < Open2 ~ Team1,
                                  Open1 > Open2 ~ Team2),
         "OpenSpread" = case_when(Team1 == OpenFav ~ Open1 * -1,
                                  Team2 == OpenFav ~ Open2 * -1),
         "CloseFav" = case_when(Close1 < Close2 ~ Team1,
                                Close1 > Close2 ~ Team2),
         "CloseSpread" = case_when(Team1 == CloseFav ~ Close1 * -1,
                                   Team2 == CloseFav ~ Close2 * -1),
         "Team1Open" = case_when(Team1 == OpenFav ~ OpenSpread,
                               !Team1 == OpenFav ~ OpenSpread * -1),
         "Team1Close" = case_when(Team1 == CloseFav ~ CloseSpread,
                                !Team1 == CloseFav ~ CloseSpread *-1),
         "Team2Open" = case_when(Team2 == OpenFav ~ OpenSpread,
                               !Team2 == OpenFav ~ OpenSpread * -1),
         "Team2Close" = case_when(Team2 == CloseFav ~ CloseSpread,
                                !Team2 == CloseFav ~ CloseSpread *-1)) %>% 
  select(game_id, Date, Team1, Team1Open, Team1Close, Team2, Team2Open, Team2Close) 

# Reshape the dataframe where each row is a single team-game with opening and closing handicaps
spread_subset_one <- hist_results_wide %>% 
  select(game_id,Date,"Team"=Team1,"Open" = Team1Open, "Close" = Team1Close)

spread_subset_two <- hist_results_wide %>% 
  select(game_id,Date,"Team"=Team2,"Open" = Team2Open, "Close" = Team2Close)

# Bind the two subsets 
spreads <- bind_rows(spread_subset_one,spread_subset_two)

# Clean up the workspace
rm(
  list = c(
    "hist_results_wide",
    "odds_archives",
    "odds_summary",
    "spread_subset_one",
    "spread_subset_two"
  )
)

# Save data to repo
saveRDS(spreads,"./data/odds_archives/spreads.rds")
