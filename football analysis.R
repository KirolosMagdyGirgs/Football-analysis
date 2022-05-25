#package installation
# remotes::install_version("SDMTools", "1.1-221")
# devtools::install_github("statsbomb/StatsBombR",force = T)
#=====================================================
library(StatsBombR)
comp <- FreeCompetitions() %>%
filter(competition_id==37& season_name=="2020/2021")
comp

matches<-FreeMatches(comp)
matches
first <- matches %>%
filter(match_id==3775648)
first
# class(matches) # data frame 
# colnames(matches)   column names " we could use names()"

# events of matches
StatsBombData <- StatsBombFreeEvents(MatchesDF = matches, Parallel = T) 
StatsBombData = allclean(StatsBombData) 
StatsBombData

# shots and goals table 
shots_goals = StatsBombData %>%
group_by(team.name) %>%
summarise(shots = sum(type.name=="Shot", na.rm = TRUE), #Summarise takes whatever operation we give it and produces a new, separate table out of it. The vast majority of summarise uses come after group_by.
goals = sum(shot.outcome.name=="Goal", na.rm = TRUE))#na.rm = TRUE tells it to ignore any NAs within that column

#==============================================================
# shots and goals table per game 
shots_goals_per_game = StatsBombData %>%
group_by(team.name) %>%
summarise(shots = sum(type.name == "Shot" , na.rm = T) / n_distinct(match_id) , #Adding in the 'n_distinct(match_id)' means we are dividing the number of shots/goals by each distinct (or unique) instance of a match, for every team. I.e, we are dividing the numbers per game.
          goals = sum(shot.outcome.name == "Goal" , na.rm = T) / n_distinct(match_id) ) %>%
arrange(desc(shots))  # order teams by most shots   , ascending -> arrange(shots)


#?n_distinct   Efficiently count the number of unique values in a set of vectors


# shots per game bar plot  ggplot docu : https://ggplot2.tidyverse.org/reference/
library(ggplot2)
ggplot(data = shots_goals_per_game, aes( x = reorder(team.name, shots), y = shots)) +   # fill = goals for stacked bar chart

geom_bar(stat = "identity", width = 0.5 , fill='#ff4d4d'  ) +

labs( title = "Shots Per Game" ,subtitle = "FA Women's Super League 20/21" , y="Shots") +
  
theme( title = element_text(colour = 'white'), axis.text  = element_text(colour = "white" , size = 10 ),
    axis.title.x = element_text(colour = "white"),axis.title.y = element_blank(), 
    plot.background  = element_rect(fill = "#333333")) +  #axis is not flipped
  
#scale_y_continuous( expand = c(0,0)) +       #Here we cut down on the space between the bars and the edge of the plot
coord_flip()

#=======================================================
#Player shots per 90
player_shots = StatsBombData %>% group_by(player.name, player.id , team.name  ) %>% 
summarise(shots = sum(type.name=="Shot", na.rm = TRUE)) 

player_minutes = get.minutesplayed(StatsBombData) #This function gives us the minutes played in each match by ever player in the dataset.

player_minutes = player_minutes %>% group_by(player.id) %>% summarise(minutes = sum(MinutesPlayed)) #Now we group that by player and sum it altogether to get their total minutes played

player_shots = left_join(player_shots, player_minutes) #left_join allows us to combine our shots table and our minutes table, with the the player.id acting as a reference point.

player_shots = player_shots %>% mutate(nineties = minutes/90) #mutate is a dplyr function that creates a new column. In this instance we are creating a column that divides the minutes totals by 90, giving us each player??s number of 90s played for the season.

player_shots = player_shots %>% mutate(shots_per90 = shots/nineties) %>% #Finally we divide our shots totals by our number of 90s to get our shots per 90s column.

arrange(desc(shots_per90)) 

top10 <- head(player_shots , 10) 


ggplot(data = top10, aes( x = reorder(player.name, shots_per90), y = shots_per90)) +   # fill = goals for stacked bar chart
  
  geom_bar(stat = "identity", width = 0.5 , fill='#ff4d4d'  ) +
  
  labs( title = "Players shot per 90 " ,subtitle = "FA Women's Super League 20/21" , y="Shots/90") +
  
  theme( title = element_text(colour = 'white'), axis.text  = element_text(colour = "white" , size = 10 ),
         axis.title.x = element_text(colour = "white"),axis.title.y = element_blank(), 
         plot.background  = element_rect(fill = "#333333")) +  #axis is not flipped
  
  #scale_y_continuous( expand = c(0,0)) +       #Here we cut down on the space between the bars and the edge of the plot
  coord_flip()
#==========================================================================================





