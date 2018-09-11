getMarketRatings <- function(week = 1){

# Store week number
  week_num <- week

# Query Team Ratings from Inpredictable
source("Inpredictable.R")
  

# Load NFL Schedule
schedule <- readRDS("nfl_schedule.rds")

# Focus on GPF, oGPF, and dGPF
ratings <- nfl_inpredictable %>% 
  select(Team,GPF,oGPF,dGPF)


# Schedule Ratings: Add the market-based ratings to the nfl schedule
ratings_schedule <- schedule %>% 
  left_join(ratings, by = c("Away" = "Team")) %>% 
  left_join(ratings, by = c("Home" = "Team"), suffix = c("_away","_home")) %>% 
  mutate("deltaGPF" = GPF_away - GPF_home,
         "deltaOGPF" = oGPF_away - oGPF_home,
         "deltaDGPF" = dGPF_away - dGPF_home,
         "Favorite" = case_when(GPF_away > GPF_home ~ Away,
                                GPF_away < GPF_home ~ Home,
                                GPF_away == GPF_home ~ "Tie"),
         "Margin" = abs(deltaGPF),
         "Offense" = case_when(oGPF_away > oGPF_home ~ Away,
                               oGPF_away < oGPF_home ~ Home,
                               oGPF_away == oGPF_home ~ "Tie"),
         "OffMargin" = abs(deltaOGPF),
         "Defense" = case_when(dGPF_away > dGPF_home ~ Away,
                               dGPF_away < dGPF_home ~ Home,
                               dGPF_away == dGPF_home ~ "Tie"),
         "DefMargin" = abs(deltaDGPF)) 

# Plot matchups
ratings_schedule %>% 
  filter(Week == week_num) %>% 
  mutate("Matchup" = paste(Away,Home,sep = "-")) %>% 
  ggplot(aes(x=reorder(Matchup,Margin),y=Margin))+
  geom_text(aes(label=Favorite))+
  coord_flip() +
  xlab("")+
  ggtitle(paste0("NFL Matchups for Week ",week_num),
          subtitle = paste0("Run at ",now()))+
  scale_y_continuous(breaks=seq(1,12,.5))

# Print Table
ratings_schedule %>% 
  filter(Week==week_num) %>% 
  select(Date,Away,Home,Favorite,Margin)


}

getMarketRatings(2)


