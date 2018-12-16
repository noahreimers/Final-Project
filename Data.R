library(tidyverse)

# First I need to read in all of the individual position group ratings. Then I am creating the TeamYear variable to create a
# unique variable for each year and team that I can then group by to create a summation of each of the position groups for 
# each of the teams. Several of the position groups had slightly different names for their rating variable so there is slight
# differences in the code. The db data had a different name for the Year variable so I made a small alteration in that 
# part of the data.

pff_qb <- read_csv("PFF_Ratings_2008-2012_QB.csv") 
pff_qb$TeamYear <- paste(pff_qb$Team, pff_qb$Year, sep = "")
pff_qbs <- pff_qb %>% 
  group_by(TeamYear) %>% 
  summarize(qb_sum = sum(Overall))

pff_rb <- read_csv("PFF_Ratings_2008-2012_RBs.csv")
pff_rb$TeamYear <- paste(pff_rb$Team, pff_rb$Year, sep = "")
pff_rbs <- pff_rb %>% 
  group_by(TeamYear) %>% 
  summarize(rb_sum = sum(Overall))

pff_wr <- read_csv("PFF_Ratings_2008-2012_WR.csv")
pff_wr$TeamYear <- paste(pff_wr$Team, pff_wr$Year, sep = "")
pff_wrs <- pff_wr %>% 
  group_by(TeamYear) %>% 
  summarize(wr_sum = sum(`Overall Grade`))

pff_te <- read_csv("PFF_Ratings_2008-2012_TE.csv")
pff_te$TeamYear <- paste(pff_te$Team, pff_te$Year, sep = "")
pff_tes <- pff_te %>% 
  group_by(TeamYear) %>% 
  summarize(te_sum = sum(Overall))

pff_ol <- read_csv("PFF_Ratings_2008-2012_OL.csv")
pff_ol$TeamYear <- paste(pff_ol$Team, pff_ol$Year, sep = "")
pff_ols <- pff_ol %>% 
  group_by(TeamYear) %>% 
  summarize(ol_sum = sum(`Overall Rating`))

pff_dl <- read_csv("PFF_Ratings_2008-2012_DL.csv")
pff_dl$TeamYear <- paste(pff_dl$Team, pff_dl$Year, sep = "")
pff_dls <- pff_dl %>% 
  group_by(TeamYear) %>% 
  summarize(dl_sum = sum(Overall))

pff_db <- read_csv("PFF_Ratings_2008-2012_DB.csv")
names(pff_db)[4]="Year"
pff_db$TeamYear <- paste(pff_db$Team, pff_db$Year, sep = "")
pff_dbs <- pff_db %>% 
  group_by(TeamYear) %>% 
  summarize(db_sum = sum(Overall))

pff_lb <- read_csv("PFF_Ratings_2008-2012_LB.csv")
pff_lb$TeamYear <- paste(pff_lb$Team, pff_lb$Year, sep = "")
pff_lbs <- pff_lb %>% 
  group_by(TeamYear) %>% 
  summarize(lb_sum = sum(Overall))


# The data that I'm loading in here is the aggregated team ratings that include things like "Run offense", "Pass defense", etc. 
# and they also have overall ratings for both offense and defense.


pff_team <- read_csv("PFF_Ratings_2008-2012_Team.csv")

# This line of code is aggregating the overall ratings for each team over the 5 years to give a summation of their ratings for
# offense and defense.

pff_team_total <- pff_team %>% 
  group_by(Team) %>% 
  summarize(team_total_offense = sum(`Overall Offense`), team_total_defense = sum(`Overall Defense`))

# This code is reading in each team's record for each of the 5 years observed in the pff data. I found and altered this data on 
# my own.

nfl_records <- read_csv("NFL_Records.csv")

# Here I am joining the overall rating data for each year with the team's record and joining by the unique TeamYear ID that will 
# match each of the years that the team has ratings with their record that year.

records_team_ratings <- pff_team %>% left_join(nfl_records, by = "TeamYear")

# In this chunk of code I am aggregating the overall team ratings and the records with the summation of each of the position groups
# I can then analyze which position groups are most indicative of winning. 

positions_team_records <- records_team_ratings %>% 
  left_join(pff_qbs, by = "TeamYear") %>% 
  left_join(pff_rbs, by = "TeamYear") %>% 
  left_join(pff_wrs, by = "TeamYear") %>% 
  left_join(pff_tes, by = "TeamYear") %>% 
  left_join(pff_ols, by = "TeamYear") %>% 
  left_join(pff_dls, by = "TeamYear") %>% 
  left_join(pff_dbs, by = "TeamYear") %>% 
  left_join(pff_lbs, by = "TeamYear")

positions_team_records <- positions_team_records %>% 
  mutate(Overall_Offense = `Overall Offense`, Overall_Defense = `Overall Defense`)
positions_team_records <- positions_team_records %>% 
  select(TeamsYear, Overall_Offense, `Pass Offense`, `Rush Offense`, `Pass Block`, `Run Block`, Overall_Defense, 
         `Run Defense`,`Pass Rush`, `Pass Coverage`, `Special Teams`, Pct, Pyt_Exp, qb_sum, rb_sum, wr_sum, te_sum, ol_sum, 
         dl_sum, db_sum, lb_sum) 

# Here I am writing the first portion of the data to an rds.

write_rds(positions_team_records, "final_app/p_groups_records.rds", compress = "none")





## I am now reading in the NFL Draft data that I have from Pro Football Reference. This includes the team, round, pick, position,
## and the value (measured in AV) that the team got from this player (as of 2018). So there is clearly some bias from players 
## that were drafted in 2008 vs. 2011. I have also taken the personal liberty to lag the NFL Draft data by one year. So in this
## data the 2009 NFL Draft will be listed as 2008, because it followed the 2008 NFL season. I thought this made the most sense
## in terms of what I am looking at.

draft<- read_csv("Draft Data.csv") 
colnames(draft)[6] <- "TeamYear"

## The first chunk is creating a variable in each of the positional datasets that gives me the ability to match up the other data
## with each Team, Year and Position, and is consolidating all the different positions within "DL", "OL" and "DB" that are listed.
## This is not ideal, because needing to draft a defensive end is much different than needing to draft a defensive tackle, but 
## limitations in the data call for it.
## The second chunk is creating a variable that will allow me to match based upon Team and Position, so this will allow me to 
## match with data that is not looking at each year individually, but instead at each team over all the years at each position
## group.

pff_qb$TeamYearPos <- paste(pff_qb$Team, pff_qb$Year, "QB", sep = "")
pff_rb$TeamYearPos <- paste(pff_rb$Team, pff_rb$Year, "RB" ,sep = "")
pff_wr$TeamYearPos <- paste(pff_wr$Team, pff_wr$Year, "WR" ,sep = "")
pff_te$TeamYearPos <- paste(pff_te$Team, pff_te$Year, "TE" ,sep = "")
pff_ol$TeamYearPos <- paste(pff_ol$Team, pff_ol$Year, pff_ol$Position ,sep = "")
pff_dl$TeamYearPos <- paste(pff_dl$Team, pff_dl$Year, pff_dl$Position ,sep = "")
pff_lb$TeamYearPos <- paste(pff_lb$Team, pff_lb$Year, "LB" ,sep = "")
pff_db$TeamYearPos <- paste(pff_db$Team, pff_db$Year, "DB" ,sep = "")

pff_qb$TeamPos <- paste(pff_qb$Team, "QB", sep = "")
pff_rb$TeamPos <- paste(pff_rb$Team, "RB" ,sep = "")
pff_wr$TeamPos <- paste(pff_wr$Team, "WR" ,sep = "")
pff_te$TeamPos <- paste(pff_te$Team, "TE" ,sep = "")
pff_ol$TeamPos <- paste(pff_ol$Team, "OL" ,sep = "")
pff_dl$TeamPos <- paste(pff_dl$Team, "DL" ,sep = "")
pff_lb$TeamPos <- paste(pff_lb$Team, "LB" ,sep = "")
pff_db$TeamPos <- paste(pff_db$Team, "DB" ,sep = "")


## The draft data I have pulled in does not have specific positions for every single player, instead there are sometimes where it
## will place them within "OL" instead of delineating if they are a guard, tackle or center. Because of this I am going to change
## all of the offensive linemen to "OL" and do the same for defensive line. The last two lines of code are a little workaround I 
## am using. gsub takes all of the observations with "T" in them and changes them to "OL" so it altered my "TE" positions to "OLE"
## Likewise for my one observation that is "NG" it changed this portion to "NOL" when a nose guard is really part of the DL.


draft$Pos <- gsub("DT", "DL", draft$Pos)
draft$Pos <- gsub("DE", "DL", draft$Pos)
draft$Pos <- gsub("G", "OL", draft$Pos)
draft$Pos <- gsub("T", "OL", draft$Pos)
draft$Pos <- gsub("C", "OL", draft$Pos)
draft$Pos <- gsub("OLE", "TE", draft$Pos)
draft$Pos <- gsub("NOL", "DL", draft$Pos)

## In this small chunk I am making all of the value observations that are NA equal to 0 so I can sum up the value from each pick
## The second line is creating a strength of round variable. A 1st round pick is worth much more than a 7th round pick, so I have
## used a simple 8 - round equation to make the 1st round worth the most. This is a very simplistic way of valuing the draft
## picks, but one that I think makes sense.

draft[is.na(draft)] <- 0
draft$Rnd_str <- 8 - draft$Rnd
draft$TeamPos <- paste(draft$Team, draft$Pos, sep = "")

## This first chunk is collecting the total amount of picks and their strength spent on each position by each team. I can then use
## This data to join with the ratings data to look at the overall ratings and the corresponding usage on draft picks.

draft_str <- draft %>% 
  group_by(TeamPos) %>% 
  summarize(draft_picks = sum(Rnd_str))

## In this chunk I'm creating a dataset that is finding the draft round strength and the value that the team derived from those 
## selections. AV is a metric that Pro Football Reference uses to estimate the total value a player brings to a team, and that
## value is as of 2018.

draft_str_value <- draft %>% group_by(Team, Pos) %>% 
  summarize(draft_picks = sum(Rnd_str), value = sum(DrAV))

## Now I am going to sum the total ratings for each of the position groups for each team individually. Some of them have slightly
## different names for their grades.

pff_qb_group <- pff_qb %>% group_by(TeamPos) %>% summarize(team_total = sum(Overall))
pff_rb_group <- pff_rb %>% group_by(TeamPos) %>% summarize(team_total = sum(Overall))
pff_wr_group <- pff_wr %>% group_by(TeamPos) %>% summarize(team_total = sum(`Overall Grade`))
pff_te_group <- pff_te %>% group_by(TeamPos) %>% summarize(team_total = sum(Overall))
pff_ol_group <- pff_ol %>% group_by(TeamPos) %>% summarize(team_total = sum(`Overall Rating`))
pff_dl_group <- pff_dl %>% group_by(TeamPos) %>% summarize(team_total = sum(Overall))
pff_lb_group <- pff_lb %>% group_by(TeamPos) %>% summarize(team_total = sum(Overall))
pff_db_group <- pff_db %>% group_by(TeamPos) %>% summarize(team_total = sum(Overall))


## This line is creating a TeamPos variable that I can then use to join with other datasets.

draft_str_value$TeamPos <- paste(draft_str_value$Team, draft_str_value$Pos, sep = "")

## Here I am binding all of the individual position group ratings with each other and then using a left join to bring in the 
## Draft and value data. I made all of the NAs 0. I made a value_per variable that just simply divides the value derived from
## the picks by the strength in the draft picks they used. 

ratings_draft_strength <- rbind(pff_qb_group, pff_rb_group, pff_wr_group, pff_te_group, pff_ol_group, pff_dl_group, pff_db_group,
                                pff_lb_group) %>% left_join(draft_str_value, by = "TeamPos")
ratings_draft_strength[is.na(ratings_draft_strength)] <- 0
ratings_draft_strength <- ratings_draft_strength %>% 
  mutate(value_per= ratings_draft_strength$value/ratings_draft_strength$draft_picks)

## This code chunk is taking the individual year records I already have in the data and transforming them into full 5 year 
## performance numbers, and then using a left join to combine that data with the ratings and draft data.

total_records <- nfl_records %>% group_by(Team) %>% 
  summarize(Win_Pct = sum(Pct)/5)

ratings_draft_strength <- ratings_draft_strength %>% 
  left_join(total_records, by = "Team")
ratings_draft_strength <- na.omit(ratings_draft_strength)

ratings_draft_strength <- ratings_draft_strength %>% mutate(all_positions = "All") %>% mutate(all_teams = "All")

write_rds(ratings_draft_strength, "final_app/draft_value_ratings.rds", compress = "none")
