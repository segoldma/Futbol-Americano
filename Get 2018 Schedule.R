# Get NFL Schedule

# Scrape from pfr
schedule <- read_html("https://www.pro-football-reference.com/years/2018/games.htm") %>% 
  html_nodes("#games") %>% 
  html_table() %>% 
  as.data.frame() %>% 
  filter(!Week == "Week")

# Formatted team abbrevs
team_abbrevs <- read.csv("team_abbrevs.csv") 
colnames(team_abbrevs) <- c("Team","Abbrev")

# Add abbrevs to the schedule file and save as RDS
schedule %>% 
  left_join(team_abbrevs, by = c("VisTm" = "Team")) %>% 
  left_join(team_abbrevs, by = c("HomeTm" = "Team")) %>% 
  select(Week, "Date" = Var.3, Time, "Away" = Abbrev.x,"Home" = Abbrev.y) %>% 
  filter(!str_detect(Week,"Pre")) %>% 
  mutate(Away = as.character(Away),
         Home = as.character(Home)) %>% 
  saveRDS("nfl_schedule.rds")

# Remove team abbreviation mapping
rm(team_abbrevs)

