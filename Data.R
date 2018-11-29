library(tidyverse)

pff_qb <- read_csv("PFF_Ratings_2008-2012_QB.csv") 
pff_qb$TeamYear <- paste(pff_qb$Team, pff_qb$Year, sep = "")


pff_rb <- read_csv("PFF_Ratings_2008-2012_RBs.csv")
pff_rb$TeamYear <- paste(pff_rb$Team, pff_rb$Year, sep = "")

pff_wr <- read_csv("PFF_Ratings_2008-2012_WR.csv")
pff_wr$TeamYear <- paste(pff_wr$Team, pff_wr$Year, sep = "")

pff_te <- read_csv("PFF_Ratings_2008-2012_TE.csv")
pff_te$TeamYear <- paste(pff_te$Team, pff_te$Year, sep = "")

pff_ol <- read_csv("PFF_Ratings_2008-2012_OL.csv")
pff_ol$TeamYear <- paste(pff_ol$Team, pff_ol$Year, sep = "")

pff_dl <- read_csv("PFF_Ratings_2008-2012_DL.csv")
pff_dl$TeamYear <- paste(pff_dl$Team, pff_dl$Year, sep = "")

pff_db <- read_csv("PFF_Ratings_2008-2012_DB.csv")
names(pff_db)[4]="Year"
pff_db$TeamYear <- paste(pff_db$Team, pff_db$Year, sep = "")


pff_lb <- read_csv("PFF_Ratings_2008-2012_LB.csv")
pff_lb$TeamYear <- paste(pff_lb$Team, pff_lb$Year, sep = "")

pff_team <- read_csv("PFF_Ratings_2008-2012_Team.csv")

pff_team_total <- pff_team %>% group_by(Team) %>% summarize(team_total_offense = sum(`Overall Offense`), team_total_defense = sum(`Overall Defense`))

nfl_records <- read_csv("NFL_Records.csv")                    

records_team_ratings <- pff_team %>% left_join(nfl_records, by = "TeamYear" )




ggplot(records_team, aes(x = `Overall Offense`, y = Pct)) + geom_point() + geom_smooth(method = "lm")
ggplot(records_team, aes(x = `Overall Defense`, y = Pct)) + geom_point() + geom_smooth(method = "lm")

