library(data.table)
library(stringr)
library(RPostgres)
library(matrixStats)

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


# model season 2-------------------------------------------------------------------

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


