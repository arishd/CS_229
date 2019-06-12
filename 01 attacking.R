# This is for the 2019 season. This code is run for every season for 2015-2019

library(data.table)
library(plyr)
library(RcppRoll)


saveLocation <- "C:/Users/adutt/Desktop/CS229/"
dataFileName <- "pl_19.csv"
pl_19_raw <- fread(paste0(saveLocation, dataFileName))

match_no <- as.numeric(rownames(pl_19_raw))
pl_19_raw_2 <- cbind(match_no=match_no, pl_19_raw)

pl_19_raw3 <- pl_19_raw_2[,c(1,3:11,13:24)]

pl_19_home <- pl_19_raw3[,c(1,2,3,5,7,11,13,15,17,19,21)]
pl_19_away <- pl_19_raw3[,c(1,2,4,6,7,12,14,16,18,20,22)]
pl_19_home$H_or_A <- "H"
pl_19_away$H_or_A <- "A"

pl_19_home <- rename(pl_19_home,c("HomeTeam"="team","FTHG"="goals","FTR"="result","HS"="shots","HST"="shots_ot","HF"="fouls","HC"="corners","HY"="yellows","HR"="reds","Date"="date"))
pl_19_away <- rename(pl_19_away,c("AwayTeam"="team","FTAG"="goals","FTR"="result","AS"="shots","AST"="shots_ot","AF"="fouls","AC"="corners","AY"="yellows","AR"="reds","Date"="date"))

pl_19 <- merge(pl_19_home,pl_19_away,all=TRUE)
pl_19 <- pl_19[order(match_no)]
#head(pl_19, 10)


setkeyv(pl_19, c("team","match_no"))
 
# win flag
pl_19$win <- ifelse(pl_19$result == pl_19$H_or_A,1,0)

# lagged goals
pl_19[, goals_l1:=shift(roll_sumr(goals, n=1)), by=team][, goals_l5:=shift(roll_sumr(goals, n=5)), by=team][, goals_ha_l3:=shift(roll_sumr(goals, n=3)), by=c("team","H_or_A")]
pl_19$goals_h_l3 <- ifelse(pl_19$H_or_A == "H",pl_19$goals_ha_l3,NA)
pl_19$goals_a_l3 <- ifelse(pl_19$H_or_A == "A",pl_19$goals_ha_l3,NA)
pl_19[, goals_ha_l3 := NULL]

# lagged shots
pl_19[, shots_l1:=shift(roll_sumr(shots, n=1)), by=team][, shots_l5:=shift(roll_sumr(shots, n=5)), by=team][, shots_ha_l3:=shift(roll_sumr(shots, n=3)), by=c("team","H_or_A")]
pl_19$shots_h_l3 <- ifelse(pl_19$H_or_A == "H",pl_19$shots_ha_l3,NA)
pl_19$shots_a_l3 <- ifelse(pl_19$H_or_A == "A",pl_19$shots_ha_l3,NA)
pl_19[, shots_ha_l3 := NULL]

# lagged shots_ot
pl_19[, shots_ot_l1:=shift(roll_sumr(shots_ot, n=1)), by=team][, shots_ot_l5:=shift(roll_sumr(shots_ot, n=5)), by=team][, shots_ot_ha_l3:=shift(roll_sumr(shots_ot, n=3)), by=c("team","H_or_A")]
pl_19$shots_ot_h_l3 <- ifelse(pl_19$H_or_A == "H",pl_19$shots_ot_ha_l3,NA)
pl_19$shots_ot_a_l3 <- ifelse(pl_19$H_or_A == "A",pl_19$shots_ot_ha_l3,NA)
pl_19[, shots_ot_ha_l3 := NULL]

# lagged corners
pl_19[, corners_l1:=shift(roll_sumr(corners, n=1)), by=team][, corners_l5:=shift(roll_sumr(corners, n=5)), by=team][, corners_ha_l3:=shift(roll_sumr(corners, n=3)), by=c("team","H_or_A")]
pl_19$corners_h_l3 <- ifelse(pl_19$H_or_A == "H",pl_19$corners_ha_l3,NA)
pl_19$corners_a_l3 <- ifelse(pl_19$H_or_A == "A",pl_19$corners_ha_l3,NA)
pl_19[, corners_ha_l3 := NULL]

# lagged win
pl_19[, wins_l1:=shift(roll_sumr(win, n=1)), by=team][, wins_l5:=shift(roll_sumr(win, n=5)), by=team]



#head(pl_19[pl_19$team == "Man City"],15)

pl_19 <- pl_19[,c("date","match_no","H_or_A","team", "goals","goals_l1", "goals_l5", "goals_h_l3", "goals_a_l3",
                  "shots_l1", "shots_l5", "shots_h_l3", "shots_a_l3",
                  "shots_ot_l1", "shots_ot_l5", "shots_ot_h_l3", "shots_ot_a_l3",
                  "corners_l1", "corners_l5", "corners_h_l3", "corners_a_l3")]

#head(pl_19)


