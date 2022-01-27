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


# train models ------------------------------------------------------------

ssn1 <- allDF[season_id == 1]
ssn2 <- allDF[season_id == 2]

train.control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 5,
                              verboseIter = TRUE,
                              classProbs = TRUE, 
                              summaryFunction = mnLogLoss)


tune.grid <- expand.grid(eta = c(0.001, 0.01, 0.1),
                         nrounds = c(100, 150),
                         max_depth = 4:6,
                         min_child_weight = c(2.0, 2.5, 3.0),
                         colsample_bytree = c(0.75, 1.0),
                         gamma = c(0, 1),
                         subsample = 1.0)

set.seed(101)

xgbTreeModel <- train(result ~ 
                         odds_home_prob_adj +
                         odds_away_prob_adj +
                         (HSAdvL6_Avg * ASAdvL6_Avg) +
                         (HGDiffL6_Avg * AGDiffL6_Avg),
                       data = ssn1,
                       method = "xgbTree",
                       metric = "logLoss",
                       tuneGrid = tune.grid,
                       trControl = train.control)

saveRDS(xgbTreeModel, "xgbTreeModel.RDS")

varImp(xgbTreeModel)

print(xgbTreeModel)

xgbTreeModel <- readRDS("xgbTreeModel.RDS")

# The final values used for the model were nrounds = 150, max_depth = 4, eta = 0.01, gamma = 1, 
# colsample_bytree = 0.75, min_child_weight = 3 and subsample = 1.

xgbTreeModel


# predict model on season 2 -----------------------------------------------

ssn2[, pred_xgb := predict(xgbTreeModel, newdata = ssn2, type = "raw")]

ssn2[, .(result, pred_xgb)]

table(ssn2$result)
table(ssn2$pred_xgb)

confusionMatrix(ssn2$result, ssn2$pred_xgb)

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



# goals models for sim tables ------------------------------------------------------------

set.seed(101)

xgbSimHGMod <- train(home_score ~ 
                        HW_XGB_Prob +
                        AW_XGB_Prob +
                        result,
                      data = ssn2Preds,
                      method = "xgbTree",
                      metric = "RMSE",
                      tuneGrid = expand.grid(eta = c(0.01),
                                             nrounds = c(150),
                                             max_depth = 4,
                                             min_child_weight = c(2.0),
                                             colsample_bytree = c(0.75),
                                             gamma = 1.0,
                                             subsample = 1.0),
                      trControl = trainControl(method = "repeatedcv",
                                               number = 5,
                                               repeats = 3,
                                               verboseIter = TRUE))

saveRDS(xgbSimHGMod, "xgb_Sim_HG_Model.RDS")


set.seed(101)

xgbSimAGMod <- train(away_score ~ 
                       HW_XGB_Prob +
                       AW_XGB_Prob +
                       result,
                     data = ssn2Preds,
                     method = "xgbTree",
                     metric = "RMSE",
                     tuneGrid = expand.grid(eta = c(0.01),
                                            nrounds = c(150),
                                            max_depth = 4,
                                            min_child_weight = c(2.0),
                                            colsample_bytree = c(0.75),
                                            gamma = 1.0,
                                            subsample = 1.0),
                     trControl = trainControl(method = "repeatedcv",
                                              number = 5,
                                              repeats = 3,
                                              verboseIter = TRUE))


saveRDS(xgbSimAGMod, "xgb_Sim_AG_Model.RDS")

simS2 <- ssn2Preds[, .(season_id, game_week, match_id, home_team_id, home_team, away_team_id, away_team,
                       HW_XGB_Prob, Draw_XGB_Prob, AW_XGB_Prob)]

# simulate season 2 -------------------------------------------------------

simS2 <- ssn2Preds[, .(season_id, game_week, match_id, home_team_id, home_team, away_team_id, away_team,
                       HW_XGB_Prob, Draw_XGB_Prob, AW_XGB_Prob)]

# set number of seasons to simulate
ns <- 100

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
    
    # setorder(lgeTable, -Pts, -GD, -GF, Team)
    # 
    # df <- as.data.frame(lgeTable)
    # weeklyTables[[wk]] <- df
    
    
  }
  
  #fwrite(lgeTable, paste0("sim_", s, "_table.csv"))
  rankTable <- lgeTable[, .(Team, P, Pts, GD, GF)]
  rankTable[, simID := s]
  
  #cat(paste0("First team this simulation ", rankTable[1, .(Team, Pts)], "\n", "\n"))
  
  finalTables <- rbindlist(list(finalTables, rankTable), use.names = TRUE, fill = TRUE)
}

finalTables[, Pos := frank(-Pts, ties.method = "min"), by = simID]

finalTables[order(Team, simID)]


# Final Position Probabilities --------------------------------------------

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
## try tibbles

# library(tidyverse)
# 
# finalTables <- tibble(finalTables)
# finalTables$Team <- as.character(finalTables$Team)
# 
# for(s in 1:ns){
#   
#   ssn2Preds <- as_tibble(ssn2Preds)
#   
#   lgeTable <- tibble(Team = allTeams,
#                          P = 0, W = 0, D = 0, L = 0, GF = 0, GA = 0, GD = 0, Pts = 0)
#   
#   ssn2Preds$result <- simRuns[[s]]
#   
#   for(wk in 1:max(ssn2Preds$game_week)){
#     games <- ssn2Preds %>% filter(game_week == wk)
#     
#     for(i in seq_along(games$match_id)){
#       lgeTable %>% 
#         filter(Team == games$home_team[i]) %>% 
#         mutate(P = P + 1,
#                W = if_else(games$result[i] == "H", W + 1, W),
#                D = if_else(games$result[i] == "D", D + 1, D),
#                L = if_else(games$result[i] == "A", L + 1, L),
#                GF = games$home_score[i] + GF,
#                GA = games$away_score[i] + GA,
#                GD = games$home_gd[i] + GD,
#                Pts = games$home_pts[i] + Pts)
#         
#     }
#     
#     for(i in seq_along(games$match_id)){
#       lgeTable %>% 
#         filter(Team == games$away_team[i]) %>% 
#         mutate(P = P + 1,
#                W = if_else(games$result[i] == "A", W + 1, W),
#                D = if_else(games$result[i] == "D", D + 1, D),
#                L = if_else(games$result[i] == "H", L + 1, L),
#                GF = games$away_score[i] + GF,
#                GA = games$home_score[i] + GA,
#                GD = games$away_gd[i] + GD,
#                Pts = games$away_pts[i] + Pts)
#       
#       }
#     
#     
#   }
#   
#   rankTable <- lgeTable %>% select(Team, P, Pts, GD, GF)
#   rankTable$simID <- s
#   
#   finalTables <- bind_rows(finalTables, rankTable)
# }
# 
# setDT(finalTables)
# 
# finalTables
# 
