library(data.table)
library(stringr)
library(RPostgres)
library(matrixStats)
library(caret)
library(xgboost)

source(".hidden.R")

con <- dbConnect(RPostgres::Postgres(), 
                 dbname = dbname,
                 host = dbhost,
                 port = pgport,
                 user = username,
                 password = dbpw)


qry1 <- "select * from full_results
		      where season_id = 1
		      order by match_id;"
qry2 <- "select * from full_results
		      where season_id = 2
		      order by match_id;"

s1 <- dbGetQuery(con, qry1)
s2 <- dbGetQuery(con, qry2)

dbDisconnect(con)

str(s1)

setDT(s1)

setorder(s1, season_id, game_week, match_id)

str(s1)
summary(s1)
colSums(is.na(s1))


s1[, result := fifelse(home_score > away_score, "H", 
                       fifelse(home_score < away_score, "A", "D"))]

s1[, `:=` (home_pts = fifelse(result == "H", 3,
                              fifelse(result == "D", 1, 0)),
           away_pts = fifelse(result == "H", 0, 
                              fifelse(result == "D", 1, 3)),
           home_gd = home_score - away_score,
           away_gd = away_score - home_score,
           home_shot_adv = home_shots - away_shots,
           away_shot_adv = away_shots - home_shots,
           odds_home_prob = round(1 / home_odds * 100, 2),
           odds_draw_prob = round(1 / draw_odds * 100, 2),
           odds_away_prob = round(1 / away_odds * 100, 2))]
           
s1[, book_pct := odds_home_prob + odds_draw_prob + odds_away_prob]

s1[, `:=` (odds_home_prob_adj = round(odds_home_prob / book_pct * 100, 2),
           odds_draw_prob_adj = round(odds_draw_prob / book_pct * 100, 2),
           odds_away_prob_adj = round(odds_away_prob / book_pct * 100, 2))]

s1[, result_prob_adj := fifelse(result == "H", odds_home_prob_adj,
                                fifelse(result == "A", odds_away_prob_adj, odds_draw_prob_adj))]

s1

## Create table

allTeams <- unique(s1$home_team)

lgeTable <- data.table(Team = allTeams,
                       P = 0L, W = 0L, D = 0L, L = 0L, GF = 0L, GA = 0L, GD = 0L, Pts = 0L)

setorder(lgeTable, -Pts, -GD, -GF, Team)
lgeTable

weeklyTables <- list()

title_won <-  0
## Update Table by match week

for(wk in 1:max(s1$game_week)){
  games <- s1[game_week == wk,]
  for(i in seq_along(games$match_id)){
    lgeTable[Team == games$home_team[i], `:=` (
      P = P + 1,
      W = fifelse(games$result[i] == "H", W + 1, W),
      D = fifelse(games$result[i] == "D", D + 1, D),
      L = fifelse(games$result[i] == "A", L + 1, L),
      GF = games$home_score[i] + GF,
      GA = games$away_score[i] + GA,
      GD = games$home_gd[i] + GD,
      Pts = games$home_pts[i] + Pts)]
  }
  for(i in seq_along(games$match_id)){
    lgeTable[Team == games$away_team[i], `:=` (
      P = P + 1,
      W = fifelse(games$result[i] == "A", W + 1, W),
      D = fifelse(games$result[i] == "D", D + 1, D),
      L = fifelse(games$result[i] == "H", L + 1, L),
      GF = games$away_score[i] + GF,
      GA = games$home_score[i] + GA,
      GD = games$away_gd[i] + GD,
      Pts = games$away_pts[i] + Pts)]
  }
  
  
  setorder(lgeTable, -Pts, -GD, -GF, Team)
  
  df <- as.data.frame(lgeTable)
  weeklyTables[[wk]] <- df
  
  max_points <- (max(s1$game_week) - wk) * 3
  
  if((lgeTable$Pts[[2]] + max_points < lgeTable$Pts[[1]]) & title_won == 0){
    title_won <- 1
    wkWon <- wk
  }
  
  #Sys.sleep(1)
  
}

cat(paste0(lgeTable$Team[[1]], " won the league after game week ", wkWon))

names(weeklyTables) <- paste0("Week_", 1:54, "_Table")

weeklyTables$Week_54_Table


# Questions season 1 ------------------------------------------------------


#Q1 Miami won the league in season 1.

#Q2 Miami secured the title in week 50

## Find Biggest Upset

# Unlikeliest result

s1[order(result_prob_adj)][1]

#Q3 Match ID 168, game week 12 Miami 3-3 Milwaukee, draw probability (adjusted to 100% book) of 4.17%

# Biggest upset win

s1[result != "D"][order(result_prob_adj)][1]

# Match ID 715, game week 52 Arlington 0-1 Houston, away win probability (adjusted to 100% book) of 5.58%


# season 2 add features-------------------------------------------------------------------

setDT(s2)

colSums(is.na(s2))


s2[, result := fifelse(home_score > away_score, "H", 
                       fifelse(home_score < away_score, "A", "D"))]

s2[, `:=` (home_pts = fifelse(result == "H", 3,
                              fifelse(result == "D", 1, 0)),
           away_pts = fifelse(result == "H", 0, 
                              fifelse(result == "D", 1, 3)),
           home_gd = home_score - away_score,
           away_gd = away_score - home_score,
           home_shot_adv = home_shots - away_shots,
           away_shot_adv = away_shots - home_shots,
           odds_home_prob = round(1 / home_odds * 100, 2),
           odds_draw_prob = round(1 / draw_odds * 100, 2),
           odds_away_prob = round(1 / away_odds * 100, 2))]

s2[, book_pct := odds_home_prob + odds_draw_prob + odds_away_prob]

s2[, `:=` (odds_home_prob_adj = round(odds_home_prob / book_pct * 100, 2),
           odds_draw_prob_adj = round(odds_draw_prob / book_pct * 100, 2),
           odds_away_prob_adj = round(odds_away_prob / book_pct * 100, 2))]

s2[, result_prob_adj := fifelse(result == "H", odds_home_prob_adj,
                                fifelse(result == "A", odds_away_prob_adj, odds_draw_prob_adj))]

s2

# Season 2 table

allTeams <- unique(s2$home_team)

lgeTable <- data.table(Team = allTeams,
                       P = 0L, W = 0L, D = 0L, L = 0L, GF = 0L, GA = 0L, GD = 0L, Pts = 0L)

setorder(lgeTable, -Pts, -GD, -GF, Team)
lgeTable

weeklyTablesS2 <- list()

title_won <-  0

## Update season 2 Table by match week

for(wk in 1:max(s2$game_week)){
  games <- s2[game_week == wk,]
  for(i in seq_along(games$match_id)){
    lgeTable[Team == games$home_team[i], `:=` (
      P = P + 1,
      W = fifelse(games$result[i] == "H", W + 1, W),
      D = fifelse(games$result[i] == "D", D + 1, D),
      L = fifelse(games$result[i] == "A", L + 1, L),
      GF = games$home_score[i] + GF,
      GA = games$away_score[i] + GA,
      GD = games$home_gd[i] + GD,
      Pts = games$home_pts[i] + Pts)]
  }
  for(i in seq_along(games$match_id)){
    lgeTable[Team == games$away_team[i], `:=` (
      P = P + 1,
      W = fifelse(games$result[i] == "A", W + 1, W),
      D = fifelse(games$result[i] == "D", D + 1, D),
      L = fifelse(games$result[i] == "H", L + 1, L),
      GF = games$away_score[i] + GF,
      GA = games$home_score[i] + GA,
      GD = games$away_gd[i] + GD,
      Pts = games$away_pts[i] + Pts)]
  }
  
  
  setorder(lgeTable, -Pts, -GD, -GF, Team)
  
  df <- as.data.frame(lgeTable)
  weeklyTablesS2[[wk]] <- df
  
  max_points <- (max(s2$game_week) - wk) * 3
  
  if((lgeTable$Pts[[2]] + max_points < lgeTable$Pts[[1]]) & title_won == 0){
    title_won <- 1
    wkWon <- wk
  }
  
  #Sys.sleep(1)
  
}

cat(paste0(lgeTable$Team[[1]], " won the league after game week ", wkWon))

names(weeklyTablesS2) <- paste0("Week_", 1:54, "_Table")

weeklyTablesS2$Week_54_Table

# join seasons 1 and 2

slst <- list(s1, s2)
allDF <- rbindlist(slst, use.names = TRUE, fill = TRUE)

setorder(allDF, home_team, season_id, game_week)

allDF[, `:=` (hshots_adv = list(home_shot_adv),
              hgdiff = list(home_gd)),
      by = home_team]

setorder(allDF, away_team, season_id, game_week)

allDF[, `:=` (ashots_adv = list(away_shot_adv),
              agdiff = list(away_gd)),
      by = away_team]

allDF[, `:=` (HmShotsAdv = sapply(hshots_adv, toString),
              HmGlDiff = sapply(hgdiff, toString),
              AwShotsAdv = sapply(ashots_adv, toString),
              AwGlDiff = sapply(agdiff, toString))]

allDF[, c("HSAdv_L1", "HSAdv_L2", "HSAdv_L3", "HSAdv_L4", "HSAdv_L5", "HSAdv_L6") := tstrsplit(HmShotsAdv,
                                                                                               split = ",",
                                                                                               type.convert = TRUE,
                                                                                               keep = c(1,2,3,4,5,6))]

allDF[, c("HGDiff_L1", "HGDiff_L2", "HGDiff_L3", "HGDiff_L4", "HGDiff_L5", "HGDiff_L6") := tstrsplit(HmGlDiff,
                                                                                               split = ",",
                                                                                               type.convert = TRUE,
                                                                                               keep = c(1,2,3,4,5,6))]

allDF[, c("ASAdv_L1", "ASAdv_L2", "ASAdv_L3", "ASAdv_L4", "ASAdv_L5", "ASAdv_L6") := tstrsplit(AwShotsAdv,
                                                                                               split = ",",
                                                                                               type.convert = TRUE,
                                                                                               keep = c(1,2,3,4,5,6))]

allDF[, c("AGDiff_L1", "AGDiff_L2", "AGDiff_L3", "AGDiff_L4", "AGDiff_L5", "AGDiff_L6") := tstrsplit(AwGlDiff,
                                                                                                     split = ",",
                                                                                                     type.convert = TRUE,
                                                                                                     keep = c(1,2,3,4,5,6))]

setorder(allDF, season_id, game_week, match_id)

allDF[, HSAdvL6_Avg := rowMeans2(as.matrix(.SD), na.rm = TRUE),
      .SDcols = c("HSAdv_L1", "HSAdv_L2", "HSAdv_L3", "HSAdv_L4", "HSAdv_L5", "HSAdv_L6")]

allDF[, HGDiffL6_Avg := rowMeans2(as.matrix(.SD), na.rm = TRUE),
      .SDcols = c("HGDiff_L1", "HGDiff_L2", "HGDiff_L3", "HGDiff_L4", "HGDiff_L5", "HGDiff_L6")]

allDF[, ASAdvL6_Avg := rowMeans2(as.matrix(.SD), na.rm = TRUE),
      .SDcols = c("ASAdv_L1", "ASAdv_L2", "ASAdv_L3", "ASAdv_L4", "ASAdv_L5", "ASAdv_L6")]

allDF[, AGDiffL6_Avg := rowMeans2(as.matrix(.SD), na.rm = TRUE),
      .SDcols = c("AGDiff_L1", "AGDiff_L2", "AGDiff_L3", "AGDiff_L4", "AGDiff_L5", "AGDiff_L6")]

allDF[, `:=` (HmShotsAdv = NULL,
              HmGlDiff = NULL,
              AwShotsAdv = NULL,
              AwGlDiff = NULL)]

allDF[, `:=` (hshots_adv = NULL,
              hgdiff = NULL,
              ashots_adv = NULL,
              agdiff = NULL)]
      
allDF[, result := factor(result, levels = c("H", "D", "A"))]


# predict model on season 2 -----------------------------------------------

xgbTreeModel <- readRDS("xgbTreeModel.RDS")

ssn2 <- allDF[season_id == 2]

ssn2[, pred_xgb := predict(xgbTreeModel, newdata = ssn2, type = "raw")]

ssn2[, .(result, pred_xgb)]

table(ssn2$result)
table(ssn2$pred_xgb)

confusionMatrix(ssn2$pred_xgb, ssn2$result)

prob_xgb <- predict(xgbTreeModel, newdata = ssn2, type = "prob")

colnames(prob_xgb) <- c("HW_XGB_Prob", "Draw_XGB_Prob", "AW_XGB_Prob")

ssn2Preds <- cbind(ssn2, prob_xgb)

# correlations between model predictions and odds probabilities

cor(ssn2Preds[, .(odds_home_prob_adj, HW_XGB_Prob, odds_draw_prob, Draw_XGB_Prob, odds_away_prob, AW_XGB_Prob)])

# Odds based predictions

ssn2Preds[, odds_pred_result := fifelse((odds_home_prob_adj > odds_draw_prob_adj) & 
                                          (odds_home_prob_adj > odds_away_prob_adj),"H",
          fifelse((odds_away_prob_adj > odds_draw_prob_adj) & 
                         (odds_away_prob_adj > odds_home_prob_adj), "A", "D"))]

ssn2Preds$odds_pred_result <- factor(ssn2Preds$odds_pred_result, levels = c("H", "D", "A"))

confusionMatrix(ssn2Preds$odds_pred_result, ssn2Preds$result)


# simulate season 2 -------------------------------------------------------

simS2 <- ssn2Preds[, .(season_id, game_week, match_id, home_team_id, home_team, away_team_id, away_team,
                       HW_XGB_Prob, Draw_XGB_Prob, AW_XGB_Prob)]

xgbSimHGMod <- readRDS("xgb_Sim_HG_Model.RDS")
xgbSimAGMod <- readRDS("xgb_Sim_AG_Model.RDS")

# set number of seasons to simulate
ns <- 200

simRuns <- list()

for(k in 1:ns) {
  
  simres <- c()
  
  for(i in seq_along(ssn2Preds$result)){
    simres[i] <- sample(c("H", "D", "A"), 
                        size = 1, 
                        prob = c(ssn2Preds$HW_XGB_Prob[[i]], ssn2Preds$Draw_XGB_Prob[[i]], ssn2Preds$AW_XGB_Prob[[i]]))
  }
  
  simRuns[[k]] <- factor(simres, levels = c("H", "D", "A"))
}



# Create data.table to hold result of simulations

finalTables <- setNames(data.table(matrix(nrow = 0, ncol = 6)), c("Team", "P", "Pts", "GD", "GF", "simID"))

allTeams <- unique(ssn2Preds$home_team)

# Iterate through sims

for(s in 1:ns){
  
  lgeTable <- data.table(Team = allTeams,
                         P = 0L, W = 0L, D = 0L, L = 0L, GF = 0, GA = 0, GD = 0, Pts = 0L)
  
  #setorder(lgeTable, -Pts, -GD, -GF, Team)
  
  simS2[, result := simRuns[[s]]]
  
  simS2[, `:=` (home_score = predict(xgbSimHGMod, newdata = simS2, type = "raw"),
                away_score = predict(xgbSimAGMod, newdata = simS2, type = "raw"))]
  
  simS2[, `:=` (home_gd = home_score - away_score,
                away_gd = away_score - home_score,
                home_pts = fifelse(result == "H", 3L,
                                   fifelse(result == "D", 1L, 0)),
                away_pts = fifelse(result == "H", 0, 
                                   fifelse(result == "D", 1L, 3L)))]
  
  #fwrite(ssn2Preds, paste0("sim_", s, "_results.csv"))
  #cat(paste0("Pre sim run: ", s, " first result ", ssn2Preds$result[[1]], "\n"))
             
  
  for(wk in 1:max(simS2$game_week)){
    games <- simS2[game_week == wk]
    
    for(i in seq_along(games$match_id)){
      lgeTable[Team == games$home_team[i], `:=` (
        P = P + 1,
        W = fifelse(games$result[i] == "H", W + 1, W),
        D = fifelse(games$result[i] == "D", D + 1, D),
        L = fifelse(games$result[i] == "A", L + 1, L),
        GF = games$home_score[i] + GF,
        GA = games$away_score[i] + GA,
        GD = games$home_gd[i] + GD,
        Pts = games$home_pts[i] + Pts)]
    }
    for(i in seq_along(games$match_id)){
      lgeTable[Team == games$away_team[i], `:=` (
        P = P + 1,
        W = fifelse(games$result[i] == "A", W + 1, W),
        D = fifelse(games$result[i] == "D", D + 1, D),
        L = fifelse(games$result[i] == "H", L + 1, L),
        GF = games$away_score[i] + GF,
        GA = games$home_score[i] + GA,
        GD = games$away_gd[i] + GD,
        Pts = games$away_pts[i] + Pts)]
    }
    
    
  }
  
  rankTable <- lgeTable[, .(Team, P, Pts, GD, GF)]
  rankTable[, simID := s]
  
  cat(paste0("Completed Simulation ", s, " of ", ns, "\n"))
  
  finalTables <- rbindlist(list(finalTables, rankTable), use.names = TRUE, fill = TRUE)
}

finalTables[, Pos := frank(-Pts, ties.method = "min"), by = simID]

#finalTables[order(Team, simID)]

setorder(finalTables, Team, simID)

saveRDS(finalTables, "final_sim_tables.RDS")


# Final Position Probabilities --------------------------------------------

finalTables <- readRDS("final_sim_tables.RDS")

predFinalTable <- finalTables[, .(Avg_Pts = sum(Pts) / ns), by = Team][order(-Avg_Pts)]

predFinalTable[, Pred_Rank := frank(-Avg_Pts, ties.method = "min")]

predFinalTable

# Compare Predicted Rank with Actual Rank of Season 2


setDT(weeklyTablesS2$Week_54_Table)
weeklyTablesS2$Week_54_Table$Pos <- frank(weeklyTablesS2$Week_54_Table, -Pts, -GD, -GF, Team, ties.method = "min")
setorder(weeklyTablesS2$Week_54_Table, Pos)
dfCheck <- merge.data.table(weeklyTablesS2$Week_54_Table, predFinalTable, by = "Team")
setorder(dfCheck, Pos)

dfCheck


# Get team finish position counts

posCounts <- finalTables[, .(Count = .N,
                             Sim_Runs = ns), by = .(Team, Pos)][order(Pos, -Count)]

posCounts[, Pos_Prob := round(Count / Sim_Runs * 100, 3)]
posCounts

library(ggplot2)

ggplot(posCounts) +
  geom_col(aes(factor(Pos), Pos_Prob, fill = Pos_Prob)) +
  labs(title = "Probability of Team finishing in each position", fill = 'Probability') +
  facet_wrap(~ Team, ncol = 4) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Final League Position") +
  ylab("Probability of finishing in nth position")


# title winning probabilities of teams with at least one title season in simulations

contenders <- posCounts[Pos == 1 & Count > 0]

contenders

ggplot(contenders) +
  geom_col(aes(Team, Pos_Prob, fill = Pos_Prob)) +
  labs(title = "Title Contenders Win Probability", fill = 'Probability') +
  geom_label(aes(Team, Pos_Prob, label = paste0(Pos_Prob, "%"))) +
  #facet_wrap(~ Team, ncol = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Team") +
  ylab("Probability of Winning League")



# expected goals season 1----------------------------------------------------------

# Create variables for match shots/goals

s1[, `:=` (match_shots = home_shots + away_shots,
           match_goals = home_score + away_score)]

# calculate season 1 shots per goal and home/away match XG

shots_per_goal <- round(sum(s1$match_shots / sum(s1$match_goals)),2)

s1[, `:=` (homeXG = round(home_shots / shots_per_goal, 2),
           awayXG = round(away_shots / shots_per_goal, 2))]

s1[, `:=` (homeXG_diff = round(home_score - homeXG, 2),
           awayXG_diff = round(away_score - awayXG, 2))]

s1_homeXG <- s1[, .(s1_hXGF_diff = sum(homeXG_diff),
       s1_hXGA_diff = sum(awayXG_diff)),
   by = home_team][order(s1_hXGF_diff)]

s1_awayXG <- s1[, .(s1_aXGF_diff = sum(awayXG_diff),
       s1_aXGA_diff = sum(homeXG_diff)),
   by = away_team][order(s1_aXGF_diff)]

s1_XG <- merge.data.table(s1_homeXG, s1_awayXG, by.x = "home_team", by.y = "away_team")

setnames(s1_XG, c("home_team", "s1_hXGF_diff", "s1_hXGA_diff", "s1_aXGF_diff", "s1_aXGA_diff" ),
         c("Team", "homeXG_for_diff", "homeXG_against_diff", "awayXG_for_diff", "awayXG_against_diff"))

s1_XG[, Net_GoalDiff_v_XGDiff := round((homeXG_for_diff - homeXG_against_diff) + (awayXG_for_diff - awayXG_against_diff), 2)][
  order(Net_GoalDiff_v_XGDiff)
]


ggplot(s1_XG) +
  geom_col(aes(reorder(Team, Net_GoalDiff_v_XGDiff), Net_GoalDiff_v_XGDiff, fill = Net_GoalDiff_v_XGDiff)) +
  labs(title = "Season 1 Net Goal Difference vs Expected Net Goal Difference", fill = 'Probability') +
  geom_label(aes(Team, Net_GoalDiff_v_XGDiff, label = Net_GoalDiff_v_XGDiff)) +
  xlab("Team") +
  ylab("Net Goal Diff vs XG Net Goal Diff") +
  coord_flip()

