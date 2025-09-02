#Pull my excel file over from excel
full_year_conf_champ_game_stats <- read_excel("full year conf champ game stats.xlsx")
library(dplyr)
#change the name to make it easier to type
nfl <- full_year_conf_champ_game_stats
#gather stats for each team
Commanders_stats <- nfl %>% filter(nfl$`Team games` == "Commanders")
Eagles_stats <- nfl %>% filter(nfl$`Team games` == "Eagles")
Cheifs_stats <- nfl %>% filter(nfl$`Team games` == "Chiefs")
Bills_stats <- nfl %>% filter(nfl$`Team games` == "Bills")
#substitute the column names from space to underscores so it's easier to type
colnames(nfl) <- gsub(" ", "_", colnames(nfl))
#add in an average opponent rank 
avg_opponent_rank <- nfl %>% group_by(Team_games) %>% summarise(avg_opponent_rank = mean(opponent_rank, na.rm = TRUE))
#left join due to the average opponent rank being another dataset
nfl <- nfl %>% left_join(avg_opponent_rank, by = c("Team_games" = "Team_games"))
#mutate new columns for adjusted rankings per team based on opponent ranks
rank_adjusted <- nfl %>% mutate(across(c(points_for, points_allowed, passing__yards, passing_tds, rushing_yards, rushing_tds, pass_yard_allowed, pass_td_allowed, rush_yard_allowed, rush_td_allowed), ~ . * avg_opponent_rank, .names = "adjusted_{.col}"))
#create a model for the adjusted points 
model <- lm(adjusted_points_for ~ adjusted_passing__yards + adjusted_passing_tds + adjusted_rushing_yards + adjusted_rushing_tds + passing_ints + adjusted_pass_yard_allowed + adjusted_pass_td_allowed + adjusted_rush_yard_allowed + adjusted_rush_td_allowed), data = rank_adjusted)
#team average which takes the average for each teams adjusted column
team_avg <- rank_adjusted %>% group_by(Team_games) %>% summarise( average_adjusted_passing_yards = mean(adjusted_passing__yards, na.rm = TRUE) / 10, average_adjusted_passing_tds = mean(adjusted_passing_tds, na.rm = TRUE) / 10, average_adjusted_rushing_yards = mean(adjusted_rushing_yards, na.rm = TRUE) / 10, average_adjusted_rushing_tds = mean(adjusted_rushing_tds, na.rm = TRUE) / 10, average_adjusted_pass_yard_allowed = mean(adjusted_pass_yard_allowed, na.rm = TRUE) / 10, average_adjusted_pass_td_allowed = mean(adjusted_pass_td_allowed, na.rm = TRUE) / 10, average_adjusted_rush_yard_allowed = mean(adjusted_rush_yard_allowed, na.rm = TRUE) / 10, average_adjusted_rush_td_allowed = mean(adjusted_rush_td_allowed, na.rm = TRUE) / 10, average_ints = mean(passing_ints, na.rm = TRUE))
#game shows us an individual game result based on averages
game <- data.frame(
  adjusted_passing__yards = team_avg$average_adjusted_passing_yards,
  adjusted_passing_tds = team_avg$average_adjusted_passing_tds,
  adjusted_rushing_yards = team_avg$average_adjusted_rushing_yards,
  adjusted_rushing_tds = team_avg$average_adjusted_rushing_tds,
  adjusted_pass_yard_allowed = team_avg$average_adjusted_pass_yard_allowed,
  adjusted_pass_td_allowed = team_avg$average_adjusted_pass_td_allowed,
  adjusted_rush_yard_allowed = team_avg$average_adjusted_rush_yard_allowed,
  adjusted_rush_td_allowed = team_avg$average_adjusted_rush_td_allowed, average_ints = team_avg$average_ints,
  team = team_avg$Team_games
  )
#rename the passing_ints column to average_ints to allow for no errors in "game"
game <- game %>%
  rename(passing_ints = average_ints)
#find game stats for each team with the team stats using subset for a specific condition
game_bills <- subset(game, team == "Bills")
game_chiefs <- subset (game, team == "Chiefs")
game_commanders <- subset(game, team == "Commanders")
game_eagles <- subset (game, team == "Eagles")
#using the predict function we want to predict the "score" for each team
predict_team1 <- predict(model, newdata = game_bills)
predict_team2 <- predict(model, newdata = game_chiefs)
predict_team3 <- predict(model, newdata = game_commanders)
predict_team3 <- predict(model, newdata = game_commanders)
predict_team4 <- predict(model, newdata = game_eagles)
#Using the Bills vs Chiefs we want to find the AFC
AFC <- predict_team1 - predict_team2
#Using the Commanders vs Eagles we want to find the NFC
NFC <- predict_team3 - predict_team4
#Now that we have our matchups we can find the models predictions
if (AFC > 0 ) { winner <- "Bills" ; margin <- AFC} else if (AFC < 0) { winner <- "Chiefs" ; margin <- abs(AFC)}
#then print.. we are not using decimals and then we are going to divide by 10 to allow for the margin differential to be more reasonable
print(paste(winner, "win by", round(margin,0) / 10 , "points"))
#do the same for the NFC
if (NFC > 0) { winner <- "Commanders" ; margin <- NFC} else if (NFC < 0) { winner <- "Eagles" ; margin <- abs(NFC)}
print(paste(winner, "win by", round(margin, 0) / 10, "points"))
#Due to the scores of the NFC and AFC championship games we can predict the Super Bowl
Superbowl <- predict_team1 - predict_team3
if (Superbowl > 0) { winner <- "Bills" ; margin <- Superbowl} else if (Superbowl < 0) { winner <- "Commanders" ; margin <- abs(Superbowl)}
print(paste("The", winner, "win the Super Bowl by", round(margin,0), "points"))






