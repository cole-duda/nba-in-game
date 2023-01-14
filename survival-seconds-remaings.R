#install.packages("hoopR")
library(hoopR)
library(tidyverse)
nba_pbp = hoopR::load_nba_pbp(seasons = c(2022,2021,2020,2019,2018,2017))
nba_pbp = nba_pbp%>%select(period_display_value, period_number, home_score,away_score,season, away_team_name,home_team_name,home_team_spread,game_spread,home_favorite,game_id,qtr,time,clock_minutes,clock_seconds,
                           end_game_seconds_remaining)
###Start doing some feature engineering
winners = nba_pbp %>%
  group_by(game_id) %>%
  summarise_all(last)%>%mutate(winner = case_when(
    home_score > away_score ~ home_team_name,
    TRUE ~ away_team_name
  ))%>%select(game_id, winner)
nba_pbp = nba_pbp%>%left_join(winners)
nba_pbp = nba_pbp%>%mutate(away_lead = away_score - home_score, home_lead = home_score - away_score)
nba_pbp = nba_pbp%>%mutate(current_leader = case_when(
  home_score > away_score ~ home_team_name,
  home_score == away_score ~ 'Tie',
  TRUE ~ away_team_name
), current_leader_won = case_when(
  current_leader == winner ~ 1,
  TRUE ~ 0
), #lead = abs(away_lead), time_remaining = ceiling(end_game_seconds_remaining/10)*10)
lead = abs(away_lead), time_remaining = round(end_game_seconds_remaining, digits = -1))
nba_pbp = nba_pbp%>%mutate(home_pregame_odds = case_when(
  home_team_spread <= 1.5 & home_team_spread >= -1 ~ "Neutral",
  home_team_spread > 1.5 & home_team_spread <= 3 ~ "Slight Favorite",
  home_team_spread > 3 & home_team_spread <= 7 ~ "Favorite",
  home_team_spread > 7 ~ "Heavy Favorite",
  home_team_spread < 1.5 & home_team_spread >= -3 ~ "Slight Dog",
  home_team_spread < -3 & home_team_spread >= -5.5 ~ "Dog",
  TRUE ~ "Heavy Dog"
))
nba_pbp = nba_pbp%>%mutate(away_pregame_odds = case_when(
  home_team_spread <= 1.5 & home_team_spread >= -1 ~ "Neutral",
  home_team_spread > 1.5 & home_team_spread <= 3 ~ "Slight Dog",
  home_team_spread > 3 & home_team_spread <= 7 ~ "Dog",
  home_team_spread > 7 ~ "Heavy Dog",
  home_team_spread < 1.5 & home_team_spread >= -3 ~ "Slight Favorite",
  home_team_spread < -3 & home_team_spread >= -5.5 ~ "Favorite",
  TRUE ~ "Heavy Favorite"
))
nba_pbp = nba_pbp%>%mutate(pregame_odds = case_when(
  current_leader == home_team_name ~ home_pregame_odds,
  current_leader == away_team_name ~ away_pregame_odds,
  TRUE ~ "Tie"

))


current_leader_by_second = nba_pbp%>%filter(time_remaining>0)%>%group_by(time_remaining,lead)%>%summarise(pct_won = mean(current_leader_won,na.rm = TRUE), n = n())
current_leader_by_second_dogs = nba_pbp%>%filter(time_remaining>0)%>%group_by(time_remaining,lead, pregame_odds)%>%summarise(pct_won = mean(current_leader_won,na.rm = TRUE), n = n())







