
# Margin of Victory Analysis ----------------------------------------------

library(tidyverse)

# Load all game results from seasons 2009-2017
game_results <- bind_rows(readRDS("./data/season_games/games_2009.rds"),
                          readRDS("./data/season_games/games_2010.rds"),
                          readRDS("./data/season_games/games_2011.rds"),
                          readRDS("./data/season_games/games_2012.rds"),
                          readRDS("./data/season_games/games_2013.rds"),
                          readRDS("./data/season_games/games_2014.rds"),
                          readRDS("./data/season_games/games_2015.rds"),
                          readRDS("./data/season_games/games_2016.rds"),
                          readRDS("./data/season_games/games_2017.rds"))

# Update team names that have relocated
game_results <- game_results %>% 
  mutate(home = case_when(home == "JAX"~ "JAC",
                        home == "SD" ~ "LAC",
                        home == "STL" ~ "LA",
                        TRUE ~ home),
       away = case_when(away == "JAX"~ "JAC",
                        away == "SD" ~ "LAC",
                        away == "STL" ~ "LA",
                        TRUE ~ away))

# Absolute Margin of Victory by Year
game_results %>% 
  group_by(Season) %>% 
  summarise("avg_abs_mov" = mean(abs(homescore-awayscore)))

game_results %>% 
  ggplot(aes(x=factor(Season), y = abs(homescore-awayscore)))+
  geom_boxplot()+
  xlab("")+
  ylab("Abs. MOV")+
  ggtitle("Absolute MOV by Season")

# Margin of Victory and Defeat by Team (NEed to rename LAC/JAX)
game_results <- game_results %>% 
  mutate("winner" = case_when(homescore>awayscore ~ home,
                              homescore<awayscore ~ away,
                              TRUE ~ "tie")) 

game_results %>% 
  group_by(winner) %>% 
  summarise("wins" = n(),
            "avg_mov" = mean(homescore-awayscore)) %>% 
  arrange(desc(avg_mov))

game_results %>% 
  group_by(winner) %>% 
  summarise("wins" = n(),
            "avg_mov" = mean(homescore-awayscore)) %>% 
  ggplot(aes(x=reorder(winner,avg_mov),y=avg_mov)) +
  geom_point() +
  coord_flip()



# Future Cleaning task: Identify teams that need to be renamed
game_results %>% 
  count(home) %>% 
  arrange(n)

game_results %>% 
  mutate(home = case_when(home == "JAX"~ "JAC",
                          home == "SD" ~ "LAC",
                          home == "STL" ~ "LA",
                          TRUE ~ home),
         away = case_when(away == "JAX"~ "JAC",
                          away == "SD" ~ "LAC",
                          away == "STL" ~ "LA",
                          TRUE ~ away))

odds = readLines("http://www.vegasinsider.com/nfl/odds/las-vegas/")
         