# This is for the 2019 season. This code is run for every season for 2015-2019

library(data.table)
library(plyr)
library(RcppRoll)


saveLocation <- "C:/Users/adutt/Desktop/CS229/"
dataFileName <- "pl_19.csv"
pl_19_d_raw <- fread(paste0(saveLocation, dataFileName))

match_no <- as.numeric(rownames(pl_19_d_raw))
pl_19_d_raw_2 <- cbind(match_no=match_no, pl_19_d_raw)


pl_19_d_raw3 <- pl_19_d_raw_2[,c(1,3:11,13:24)]

pl_19_d_home <- pl_19_d_raw3[,c(1,2,3,6,12,14,18)]
pl_19_d_away <- pl_19_d_raw3[,c(1,2,4,5,11,13,17)]

# This is the opposite, since we are going to join it to the attacking stats to get the defensive stats of the team they are playing
pl_19_d_home$H_or_A <- "A"
pl_19_d_away$H_or_A <- "H"
pl_19_d_home$H_or_A_true <- "H"
pl_19_d_away$H_or_A_true <- "A"

#head(pl_19_d_raw3)
#head(pl_19_d_home)
#head(pl_19_d_away)

pl_19_d_home <- rename(pl_19_d_home,c("HomeTeam"="opp_team","FTAG"="goals_c","AS"="shots_c","AST"="shots_ot_c","AC"="corners_c","Date"="date"))
pl_19_d_away <- rename(pl_19_d_away,c("AwayTeam"="opp_team","FTHG"="goals_c","HS"="shots_c","HST"="shots_ot_c","HC"="corners_c","Date"="date"))
                                  
pl_19_d <- merge(pl_19_d_home,pl_19_d_away,all=TRUE)
pl_19_d <- pl_19_d[order(match_no)]
#head(pl_19_d, 10)

#nrow(pl_19_d_home)
#nrow(pl_19_d_away)
#nrow(pl_19_d)

setkeyv(pl_19_d, c("opp_team","match_no"))

# lagged goals_c
pl_19_d[, goals_c_l1:=shift(roll_sumr(goals_c, n=1)), by=opp_team][, goals_c_l5:=shift(roll_sumr(goals_c, n=5)), by=opp_team][, goals_c_ha_l3:=shift(roll_sumr(goals_c, n=3)), by=c("opp_team","H_or_A_true")]
pl_19_d$goals_c_h_l3 <- ifelse(pl_19_d$H_or_A_true == "H",pl_19_d$goals_c_ha_l3,NA)
pl_19_d$goals_c_a_l3 <- ifelse(pl_19_d$H_or_A_true == "A",pl_19_d$goals_c_ha_l3,NA)
pl_19_d[, goals_c_ha_l3 := NULL]

# lagged shots_c
pl_19_d[, shots_c_l1:=shift(roll_sumr(shots_c, n=1)), by=opp_team][, shots_c_l5:=shift(roll_sumr(shots_c, n=5)), by=opp_team][, shots_c_ha_l3:=shift(roll_sumr(shots_c, n=3)), by=c("opp_team","H_or_A_true")]
pl_19_d$shots_c_h_l3 <- ifelse(pl_19_d$H_or_A_true == "H",pl_19_d$shots_c_ha_l3,NA)
pl_19_d$shots_c_a_l3 <- ifelse(pl_19_d$H_or_A_true == "A",pl_19_d$shots_c_ha_l3,NA)
pl_19_d[, shots_c_ha_l3 := NULL]

# lagged shots_ot_c
pl_19_d[, shots_ot_c_l1:=shift(roll_sumr(shots_ot_c, n=1)), by=opp_team][, shots_ot_c_l5:=shift(roll_sumr(shots_ot_c, n=5)), by=opp_team][, shots_ot_c_ha_l3:=shift(roll_sumr(shots_ot_c, n=3)), by=c("opp_team","H_or_A_true")]
pl_19_d$shots_ot_c_h_l3 <- ifelse(pl_19_d$H_or_A_true == "H",pl_19_d$shots_ot_c_ha_l3,NA)
pl_19_d$shots_ot_c_a_l3 <- ifelse(pl_19_d$H_or_A_true == "A",pl_19_d$shots_ot_c_ha_l3,NA)
pl_19_d[, shots_ot_c_ha_l3 := NULL]

# lagged corners_c
pl_19_d[, corners_c_l1:=shift(roll_sumr(corners_c, n=1)), by=opp_team][, corners_c_l5:=shift(roll_sumr(corners_c, n=5)), by=opp_team][, corners_c_ha_l3:=shift(roll_sumr(corners_c, n=3)), by=c("opp_team","H_or_A_true")]
pl_19_d$corners_c_h_l3 <- ifelse(pl_19_d$H_or_A_true == "H",pl_19_d$corners_c_ha_l3,NA)
pl_19_d$corners_c_a_l3 <- ifelse(pl_19_d$H_or_A_true == "A",pl_19_d$corners_c_ha_l3,NA)
pl_19_d[, corners_c_ha_l3 := NULL]


#head(pl_19_d[pl_19_d$opp_team == "Man City"],15)

pl_19_d <- pl_19_d[,c("match_no","opp_team","H_or_A","goals_c_l1", "goals_c_l5", "goals_c_h_l3", "goals_c_a_l3", 
                      "shots_c_l1", "shots_c_l5", "shots_c_h_l3", "shots_c_a_l3",
                      "shots_ot_c_l1", "shots_ot_c_l5", "shots_ot_c_h_l3", "shots_ot_c_a_l3",
                      "corners_c_l1", "corners_c_l5", "corners_c_h_l3", "corners_c_a_l3")]

#head(pl_19_d)

# This gives us the what each team conceded
# The other piece of code gives us what each team scored
# So we need to join these conceded stats to the team they are playing. 
# This means when we are predicting goals scored, we take into account the attacking stats of the team we're predicting and the defensive stats of the team they are playing





