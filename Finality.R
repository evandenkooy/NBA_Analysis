#loading packages
library('tidyverse')
library('ggplot2')
library('shiny')

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

players$Field_Goal_Percentage <- players$Successful_Attempts / players$Total_Attempts #Calculating field goal percentage for each player under each distance and adding it as a column
players$Efficiency <- players$Total_Attempts * (players$Field_Goal_Percentage ^ 3) #Calculating efficiency for each player under each distance and adding it as a column

maxes <- aggregate(Efficiency ~ SHOT_FAR, data = players, FUN = max) #creating a dataframe to find the max efficiency value for each shot distance


player_data <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(player_data) <- c("Name", "Hoop", "Short", "Midrange", "Deep_2", "Three", "Deep")

#HELPPPPPPPP
for (name in unique(players$PLAYER_NAME)){
  player_data <- bind_rows(player_data, tibble(
    Name = name,
    Hoop = players[players$PLAYER_NAME == name & players$SHOT_FAR == "Hoop", "Hoop"],
    Short = players[players$PLAYER_NAME == name & players$SHOT_FAR == "Short", "Short"],
    Midrange = players[players$PLAYER_NAME == name & players$SHOT_FAR == "Midrange", "Midrange"],
    Deep_2 = players[players$PLAYER_NAME == name & players$SHOT_FAR == "Deep_2", "Deep_2"],
    Three = players[players$PLAYER_NAME == name & players$SHOT_FAR == "Three", "Three"],
    Deep = players[players$PLAYER_NAME == name & players$SHOT_FAR == "Deep", "Deep"]
  ))
}
player_data <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(player_data) <- c("Name", "Hoop", "Short", "Midrange", "Deep_2", "Three", "Deep")

for (name in unique(players$PLAYER_NAME)){
  player_data[nrow(player_data) + 1,][1] = name
  for (shottype in unique(players$SHOT_FAR)) {
    ind <- which(players$PLAYER_NAME == name & players$SHOT_FAR == shottype)
    ind2 <- which(player_data$PLAYER_NAME == name)
    player_data[ind2, shottype] <- players[ind, shottype]
    print(ind)
  }
}
