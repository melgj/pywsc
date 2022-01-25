library(data.table)
library(stringr)
library(RPostgres)
library(rlist)

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
  
  Sys.sleep(1)
  
}

cat(paste0(lgeTable$Team[[1]], " won the league after game week ", wkWon))

names(weeklyTables) <- paste0("Week_", 1:54, "_Table")

weeklyTables$Week_1_Table

## Biggest Upset

s1[order(result_prob_adj)][1]

