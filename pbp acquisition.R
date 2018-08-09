# Load Pkg
library(nflscrapR)
library(dplyr)

# Query play-by-play data for past seasons
system.time(pbp_2015 <- season_play_by_play(2015))
system.time(pbp_2016 <- season_play_by_play(2016))
system.time(pbp_2017 <- season_play_by_play(2017))

# Save results locally as RDS
saveRDS(pbp_2015, "pbp_2015")
saveRDS(pbp_2016,"pbp_2016")
saveRDS(pbp_2017, "pbp_2017")


glimpse(pbp_2017)
