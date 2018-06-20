# Required packages
# library(readr)
# library(dplyr)
# library(elo)
# library(lubridate)
# library(MLmetrics)

# Set up package loading facilitator
if (!require("pacman")) {install.packages("pacman");library(pacman)}

# Use pacman to load required packges
pacman::p_load("readr","dplyr","elo","lubridate","MLmetrics")

# Read in the world cup CSV data
rawdata = read_csv("wc_datathon_dataset.csv")

# Convert the match date to an R date
rawdata$date = ymd(rawdata$date)

# We're going to use all matches (qualifiers / friendlies etc) AFTER the 2010 world cup to predict the 2014 world cup matches
training = rawdata %>%
    filter(
      date > rawdata %>% filter(tournament == "World Cup 2010") %>% pull(date) %>% max(), 
      is_neutral == TRUE
     )

training$team_1 = tolower(training$team_1)
training$team_2 = tolower(training$team_2)

wc_2018 = read_csv("john_smith_numbersman1-1.csv")

#nations_inc <- union(unique(training$team_1),unique(training$team_2))

#match_2017 = rawdata %>% filter(year(date) == 2017,team_1 %in% nations_inc, team_2 %in% nations_inc,is_neutral == TRUE)

elo_output_home_prob <- function(k_factor,pred_match){
  pred_match_home_prob <- predict(elo.run(
    score(team_1_goals, team_2_goals) ~ team_1 + team_2,
    data = training,
    k = k_factor), newdata = pred_match %>% select(team_1, team_2))
  return (pred_match_home_prob)
}

elo_output_draw_rates <- function(k_factor){
  runelo = elo.run(score(team_1_goals,team_2_goals) ~ team_1 + team_2,
                    data = training,
                    k = k_factor)
  draw_rates =
    data.frame(
      win_prob = runelo$elos[,3],
      win_loss_draw = runelo$elos[,4]
    ) %>%
    mutate (prob_bucket = round(abs((win_prob-(1-win_prob))*5)) / 5
    ) %>%
    group_by(prob_bucket) %>%
    summarise(
      draw_prob = sum(ifelse(win_loss_draw==0.5,1,0)) / n()
    )
}

draw_rates = sum(ifelse(training$team_1_goals==training$team_2_goals,1,0)) / nrow(training)

elo_output_final <- function(k_factor,pred_match){
  fianl= pred_match %>%
    mutate(
      home_prob = elo_output_home_prob(k_factor,pred_match),
      away_prob = 1 - home_prob,
      prob_bucket = round(5*abs((home_prob - away_prob))) / 5
    ) %>%
    left_join(elo_output_draw_rates(k_factor)) %>%
    mutate(
      prob_team_1_draw = ifelse(draw_prob == 0 | is.na(draw_prob),2e-10,draw_prob)
      # draw_prob = draw_rates
    ) %>%
    mutate(
      prob_team_1_win = home_prob * (1-draw_prob),
      prob_team_1_lose = away_prob * (1-draw_prob)
    ) %>%
    # select(date,team_1,team_2,home_prob,draw_prob,away_prob,team_1_goals,team_2_goals) %>%
    # mutate(
    #   home_actual = ifelse(team_1_goals > team_2_goals,1,0),
    #   draw_actual = ifelse(team_1_goals == team_2_goals,1,0),
    #   away_actual = ifelse(team_2_goals > team_1_goals,1,0)
    # ) %>%
    # select(home_prob,draw_prob,away_prob,home_actual,draw_actual,away_actual)
    select(prob_team_1_win,prob_team_1_draw,prob_team_1_lose)
  return (fianl)
}

# Logloss: 1.14

# temp_home_prob_0 <- elo_output_home_prob(0,match_2017)
# 
# temp_draw_rates_0 <- elo_output_draw_rates(0)
# 
# temp_final_0 <- elo_output_final(0,match_2017)
# 
# temp_home_prob_20 <- elo_output_home_prob(20,match_2017)
# 
# temp_draw_rates_20 <- elo_output_draw_rates(20)
# 
# temp_final_20 <- elo_output_final(20,match_2017)
# 
# logloss <- double()
# for (i in (0:50)){
#   cat (i,
#       MultiLogLoss(y_pred = elo_output_final(i,match_2017)[,c("home_prob", "draw_prob", "away_prob")],
#       y_true = elo_output_final(i,match_2017)[,c("home_actual", "draw_actual", "away_actual")]),
#       "\n")
#   logloss <- c(logloss,MultiLogLoss(y_pred = elo_output_final(i,match_2017)[,c("home_prob", "draw_prob", "away_prob")],
#                                     y_true = elo_output_final(i,match_2017)[,c("home_actual", "draw_actual", "away_actual")]))
# }
# 
# 
# plot(0:50,logloss,type = "b")
# bestlogloss <- c(which.min(logloss),logloss[which.min(logloss)])


wc_2018_20 = elo_output_final(20,wc_2018)

wc_2018_25 = elo_output_final(25,wc_2018)

wc_2018_30 = elo_output_final(30,wc_2018)

wc_2018_35 = elo_output_final(35,wc_2018)

wc_2018_40 = elo_output_final(40,wc_2018)

wc_2018_matches <- wc_2018[,1:4]



wc_2018_20 <- cbind(wc_2018_matches,wc_2018_20)

wc_2018_25 <- cbind(wc_2018_matches,wc_2018_25)

wc_2018_30 <- cbind(wc_2018_matches,wc_2018_30)

wc_2018_35 <- cbind(wc_2018_matches,wc_2018_35)

wc_2018_40 <- cbind(wc_2018_matches,wc_2018_40)


write_csv(wc_2018_20, "Ming_Ma_ELOk20.csv")

write_csv(wc_2018_25, "Ming_Ma_ELOk25.csv")

write_csv(wc_2018_30, "Ming_Ma_ELOk30.csv")

write_csv(wc_2018_35, "Ming_Ma_ELOk35.csv")

write_csv(wc_2018_40, "Ming_Ma_ELOk40.csv")



