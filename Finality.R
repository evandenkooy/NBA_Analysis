#loading packages
library('tidyverse')
library('ggplot2')
library('shiny')
library('tidyr')
library('babynames')
baby <- babynames
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

normalize <- function(x){
  typeof <- x["SHOT_FAR"]
  eff <- maxes[maxes$SHOT_FAR == typeof, "Efficiency"]
  part1 <- (as.numeric(x["Efficiency"]) / eff) * (100 / 266.349584)
  x["Efficiency"] <- part1
  return(x)
}

rezzy <- apply(players, 1, normalize)

players <- as.data.frame(t(rezzy))


player_data <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(player_data) <- c("Name", "Hoop", "Short", "Midrange", "Deep_2", "Three", "Deep")


for (name in unique(players$PLAYER_NAME)){
  player_data[nrow(player_data) + 1, 1] = name
  for (shottype in unique(players$SHOT_FAR)) {
    ind <- which(players$PLAYER_NAME == name & players$SHOT_FAR == shottype)
    ind2 <- which(player_data$Name == name)
    if (length(ind) == 0){
      player_data[ind2, shottype] <- 0
    } else {
      player_data[ind2, shottype] <- players[ind, "Efficiency"]
    }
  }
}

# #
# #
# #
# ##
# # Define the UI
# ui <- fluidPage(
#   titlePanel("Player Shot Data"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("playerInput", "Select Player:", choices = player_data$Name, selected = player_data$Name[1]),
#     ),
#     mainPanel(
#       plotOutput("playerPlot")
#     )
#   )
# )
# 
# server <- function(input, output) {
#   
#   # Reactive expression to filter player data based on the selected player
#   selected_player <- reactive({
#     filter(player_data, Name == input$playerInput)
#   })
#   
#   # Manually set the order of shot categories
#   category_order <- c("Hoop", "Short", "Midrange", "Deep_2", "Three", "Deep")
#   
#   # Create a ggplot object
#   output$playerPlot <- renderPlot({
#     ggplot() +
#       geom_line(data = gather(player_data, key = "ShotCategory", value = "Value", -Name), 
#                 aes(x = factor(ShotCategory, levels = category_order), y = Value, color = Name, group = Name),
#                 size = 0.8, alpha = 0.5, color = "gray") +
#       labs(x = "Shot Category", y = NULL, title = "Curved Line Graph by Player") +
#       theme_minimal() +
#       theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#       theme(axis.text.y = element_blank()) +
#       geom_smooth(data = gather(player_data, key = "ShotCategory", value = "Value", -Name), 
#                   aes(x = factor(ShotCategory, levels = category_order), y = Value, color = Name, group = Name),
#                   method = "loess", se = FALSE, size = 0.5, linetype = "solid", color = "gray") +
#       geom_smooth(data = gather(selected_player(), key = "ShotCategory", value = "Value", -Name), 
#                   aes(x = factor(ShotCategory, levels = category_order), y = Value, color = Name, group = Name),
#                   method = "loess", se = FALSE, size = 0.8, linetype = "dashed", color = "red") +
#       theme(legend.position = "none", axis.title.y = element_blank(), axis.ticks.y = element_blank())
#   })
# }
# 
# shinyApp(ui, server)
# #
# #
# #
#
pts <- players %>% 
  select(PLAYER_NAME) %>% 
  distinct()

pts$Three_Pts_Made <- 0

for (row in 1:nrow(df)){
  if ((df$SHOT_TYPE[row] == "3PT Field Goal") & (df$SHOT_MADE[row] == TRUE)){
    pts$Three_Pts_Made[pts$PLAYER_NAME == df$PLAYER_NAME[row]] <- 
      pts$Three_Pts_Made[pts$PLAYER_NAME == df$PLAYER_NAME[row]] + 1
  } 
}

pts <- subset(pts, Three_Pts_Made >= 169)

withDate <- df %>%
  arrange(PLAYER_NAME, GAME_DATE) %>%
  group_by(PLAYER_NAME) %>%
  mutate(Three_Pts_Made_Total = cumsum(SHOT_TYPE == "3PT Field Goal" & SHOT_MADE))




