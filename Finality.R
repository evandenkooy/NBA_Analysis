#loading packages
library('tidyverse')
library('ggplot2')

#reading in data
df <- read_csv('NBA_2023_SHOTS.csv')

#dropping excess columns
df <- subset(df, select = -c(SEASON_1, SEASON_2, TEAM_ID, TEAM_NAME, POSITION_GROUP, HOME_TEAM, AWAY_TEAM, EVENT_TYPE, ZONE_RANGE))

#creating an additional column to define shot distance
df$SHOT_FAR <- ifelse(df$SHOT_DISTANCE >= 30, "Deep",
                      ifelse(df$SHOT_DISTANCE >= 24, "Three", 
                             ifelse(df$SHOT_DISTANCE >= 18, "Deep_2", 
                                    ifelse(df$SHOT_DISTANCE >= 12, "Midrange",
                                           ifelse(df$SHOT_DISTANCE >= 6, "Short",
                                                  "Hoop")))))



#Creating a players dataframe and adding attempt totals
players <- df %>%
  group_by(PLAYER_NAME, SHOT_FAR) %>% #grouping by the player's name and by the shot distance category
  summarize(Total_Attempts = n(), Successful_Attempts = sum(SHOT_MADE)) %>% #creating sums of attempts and made attempts
  arrange(desc(Total_Attempts)) #arranging by total attempts (Giannis at the 'HOOP' was the most common occurrence in the NBA of any shot type by any 2023 player)
players <- as.data.frame(players)



