
# Implied NFL Power Ratings from Betting Market  ------------------
# Mike Beouy's website, Inpredictable (http://inpredictable.com)

library(rvest)
library(dplyr)
library(stringr)

# Query NFL Rankings
nfl_inpredictable <- read_html("http://stats.inpredictable.com/rankings/nfl.php") %>% 
  html_nodes("table") %>% 
  html_table(fill = TRUE) %>% 
  as.data.frame() %>%  # Drop Ranking Columns
  select(2,5,7,10,13,15,17,19)

# Rename headers with data from row 2
colnames(nfl_inpredictable) <- as.character(unlist(nfl_inpredictable[1,]))

# Update column datatypes. Drop extra header row.
nfl_inpredictable <- nfl_inpredictable %>% 
  slice(2:nrow(nfl_inpredictable)) %>% 
  mutate("Team" = str_replace(`Team`,"&nbsp",""),
         "GPF" = as.numeric(GPF),
         "oGPF" = as.numeric(oGPF),
         "dGPF" = as.numeric(dGPF),
         "Projected Seed" = as.numeric(str_replace(`Projected Seed`,"%",""))/100,
         "pSOS" = as.numeric(pSOS),
         "fSOS" = as.numeric(fSOS))
