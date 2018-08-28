
# Implied NFL Power Ratings Implied from Betting Market  ------------------
# Mike Beouy's website, Inpredictable (http://inpredictable.com)

library(rvest)
library(dplyr)
library(stringr)

# Query NFL Rankings
inpredict <- read_html("http://stats.inpredictable.com/rankings/nfl.php") %>% 
  html_nodes("table") %>% 
  html_table(fill = TRUE) %>% 
  as.data.frame() %>%  # Drop Ranking Columns
  select(2,5,7,10,13,15,17,19)

# Rename headers with data from row 2
colnames(inpredict) <- as.character(unlist(inpredict[1,]))

# Update column datatypes. Drop extra header row.
inpredict <- inpredict %>% 
  slice(2:nrow(inpredict)) %>% 
  mutate("Team" = str_replace(`Team`,"&nbsp",""),
         "GPF" = as.numeric(GPF),
         "oGPF" = as.numeric(oGPF),
         "dGPF" = as.numeric(dGPF),
         "Projected Seed" = as.numeric(str_replace(`Projected Seed`,"%",""))/100,
         "pSOS" = as.numeric(pSOS),
         "fSOS" = as.numeric(fSOS))
