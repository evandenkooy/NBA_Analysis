library('tidyverse')
library('ggplot2')

df <- read_csv('NBA_2023_SHOTS.csv')

df$SHOT_FAR <- ifelse(df$SHOT_DISTANCE >= 30, "Deep",
                      ifelse(df$SHOT_DISTANCE >= 24, "Three", 
                             ifelse(df$SHOT_DISTANCE >= 18, "Deep_2", 
                                    ifelse(df$SHOT_DISTANCE >= 12, "Midrange",
                                           ifelse(df$SHOT_DISTANCE >= 6, "Short",
                                                  "Hoop")))))
