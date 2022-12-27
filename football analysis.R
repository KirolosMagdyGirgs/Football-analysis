#package installation
install.packages("devtools") 
install.packages("remotes") 
remotes::install_version("SDMTools", "1.1-221")
devtools::install_github("statsbomb/StatsBombR",force = T)
#=====================================================
library(StatsBombR)
comp <- FreeCompetitions() 
#filter(competition_id==37& season_name=="2020/2021")
comp

matches<-FreeMatches(comp)
matches
first <- matches %>%
filter(match_id==3775648)
first
# class(matches) # data frame 
# colnames(matches)   column names " we could use names()"

# events of matches
StatsBombData <- StatsBombFreeEvents(MatchesDF = first, Parallel = T) 
StatsBombData = allclean(StatsBombData) 
StatsBombData
colnames(StatsBombData)

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
#Plotting passes
devtools::install_github("FCrSTATS/SBpitch")
library("SBpitch")
passes = StatsBombData %>% 
filter(type.name=="Pass" & is.na(pass.outcome.name) & player.id==4641) %>% #is.na(delete passes that weren't received by another player)
filter(pass.end_location.x>=102 & pass.end_location.y<=62 & pass.end_location.y>=18) #passes into the box

create_Pitch() + 
geom_segment(data = passes, aes(x = location.x, y = location.y, xend = pass.end_location.x, yend = pass.end_location.y),lineend = "round", size = 0.5, colour = "#000000",
arrow =arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) + 
labs(title = "Fran Kirby, Completed Box Passes", subtitle = "WSL, 2020-21") + 
scale_y_reverse() + 
coord_fixed(ratio = 105/100) 
#-----------------------
#Vini pass vs Korea  id:337782

library(readxl)
data.source.xls <- read_excel("C:/Users/kokom/Desktop/Book3.xlsx")
vpass = data.source.xls %>% 
  filter(`type/displayName`=="Pass" & playerId==337782 & `outcomeType/displayName`=="Successful")

#+++++++++++++++++++++++++++++++++++++++++++++


# create_Pitch() + 
#   geom_segment(data = vpass, aes(x = x, y = y, xend = endX , yend = endY) 
#                ,lineend = "round", size = 0.5, colour = "#000000",
#                arrow =arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) + 
#   labs(title = "Fran Kirby, Completed Box Passes", subtitle = "WSL, 2020-21") + 
#   #scale_y_reverse() + 
#   coord_fixed(ratio = 100/100)
#====================================
# Using ggsoccer package 
install.packages("ggsoccer")
library(ggsoccer) 
ggplot() +
  annotate_pitch(fill = "#333333", colour = "white") +
  geom_segment(data = vpass,colour="#ff4d4d",size = 1,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  # geom_segment(size=1 , colour = "white",data = passes, aes(x = location.x, y = location.y, xend = pass.end_location.x, yend = pass.end_location.y),lineend = "round", size = 0.5, colour = "#000000",
  #              arrow =arrow(length = unit(0.1, "inches"), ends = "last", type = "open")) +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "black") , plot.background = element_rect(fill = "black"), title = element_text(colour = "white"))+
  direction_label(colour = "white") +
  # ggtitle("VINI JR. vs South Korea", 
  #         "Successful passes" )
  labs( title = "VINI JR. vs South Korea" ,subtitle = "Successful passes") 

#==========================================================================
#Neymar vs Korea
library(ggplot2)
library(ggsoccer)
library(readxl)
#all game events 
data.source.xls <- read_excel("C:/Users/kokom/Desktop/book2.xlsx")

#get neymar passes
neypass = data.source.xls %>% 
  filter(`type/displayName`=="Pass" & playerId==50835 & `outcomeType/displayName`=="Successful")
#get neymar KEY passes and big chances 
library(dplyr)
KeyPass=data.source.xls%>%filter(playerId==50835) %>% filter_all(any_vars(. %in% c('KeyPass')))
BigChance=data.source.xls%>%filter(playerId==50835) %>% filter_all(any_vars(. %in% c('BigChanceCreated')))

ggplot() +
  annotate_pitch(fill = "#333333", colour = "white") +
  geom_segment(data = neypass,colour="#ff4d4d",size = 1,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data = KeyPass,colour="#002fa7",size = 1,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data = BigChance,colour="#3fcf5d",size = 1,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  
 
   #coord_cartesian(xlim = c(40, 100))+   #crop the field 
  
  # scale_y_reverse()+
  # 
  # coord_flip()+
  # to make vertical pitch

  theme_pitch() +
  theme(panel.background = element_rect(fill = "black") , plot.background = element_rect(fill = "black"), title = element_text(colour = "white"))+
  direction_label(colour = "white")+
  # ggtitle("VINI JR. vs South Korea", 
  #         "Successful passes" )
  labs( title = "Neymar JR. vs South Korea" ,subtitle = "Successful passes") + 
  annotate("text", x = 15, y = 8, family = "Trebuchet MS",colour="#ff4d4d",size=3,label = "Successful passes")+
  annotate("text", x = 15, y = 13, family = "Trebuchet MS",colour="#002fa7",size=3,label = "Key passes")+
  annotate("text", x = 15, y = 18, family = "Trebuchet MS",colour="#3fcf5d",size=3,label = " Big chances created")
#=======================================================================================================
#Bruno VS SUI  ID=123761
######## Key pass can be both success and unsuccess passes 
install.packages("ggtext")
library(ggtext)
library(ggplot2)
library(ggsoccer)
library(readxl)
#all game events 
data.source.xls <- read_excel("D:/Projects/football analysis/BrunoVSwz/bruno.xlsx")

#get Bruno passes
SuccPasses = data.source.xls %>% 
  filter(`type/displayName`=="Pass" &`outcomeType/displayName`=="Successful"& playerId==123761 )
#get Bruno KEY passes and Assists 
library(dplyr)
KeyPass=data.source.xls%>%filter(playerId==123761) %>% filter_all(any_vars(. %in% c('KeyPass')))
Assists=data.source.xls%>%filter(playerId==123761) %>% filter_all(any_vars(. %in% c('IntentionalGoalAssist')))

ggplot() +
  annotate_pitch(fill = "#333333", colour = "white") +
  geom_segment(data = SuccPasses,colour="#ff4d4d",size = 1,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data = KeyPass,colour="#002fa7",size = 1,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data =Assists,colour="#3fcf5d",size = 1,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  
  
  #coord_cartesian(xlim = c(40, 100))+   #crop the field 
  
  # scale_y_reverse()+
  # 
  # coord_flip()+
  # to make vertical pitch
  
  theme_pitch() +
  theme(panel.background = element_rect(fill = "black") , plot.background = element_rect(fill = "black"), title = element_text(colour = "white"))+
  direction_label(colour = "white")+
  # ggtitle("VINI JR. vs South Korea", 
  #         "Successful passes" )
 
  labs( title = "Bruno Fernandes VS SUI" ,subtitle = "Successful passes" , caption ='@SaidMeaza') + 
  

  
  annotate("text", x = 8, y = 42, family = "Trebuchet MS",colour="#ff4d4d",size=4,label = "Successful passes")+
  annotate("text", x = 8, y = 47, family = "Trebuchet MS",colour="#002fa7",size=4,label = "Key passes")+
  annotate("text", x = 8, y = 52, family = "Trebuchet MS",colour="#3fcf5d",size=4,label = " Assists")

#==============================================================================
#Attiat-Allah vs POR   PlayerID = 447645
library(ggplot2)
library(ggsoccer)
library(readxl)

#all game events 
data.source.xls <- read_excel('D:/Projects/football analysis/Attiyat-Allah vs POR/Morco vs port.xlsx')

#get Attiat passes
SuccPasses = data.source.xls %>% 
  filter(`type/displayName`=="Pass" &`outcomeType/displayName`=="Successful"& playerId==447645 )
#get Atttiat KEY passes and Assists 
library(dplyr)
KeyPass=data.source.xls%>%filter(playerId==447645) %>% filter_all(any_vars(. %in% c('KeyPass')))
Assists=data.source.xls%>%filter(playerId==447645) %>% filter_all(any_vars(. %in% c('IntentionalGoalAssist')))



ggplot() +
  annotate_pitch(fill = "#333333", colour = "white") +
  geom_segment(data = SuccPasses,colour="#ff4d4d",size =1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data = KeyPass,colour="#3d85c6",size = 1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data =Assists,colour="#3fcf5d",size = 1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  
  
  #coord_cartesian(xlim = c(40, 100))+   #crop the field 
  
  
  #Vertical field 
  scale_y_reverse()+
  coord_flip()+

  theme_pitch(aspect_ratio = 105/68) +
  theme(panel.background = element_rect(fill = "black") , plot.background = element_rect(fill = "black"), title = element_text(colour = "white",size=15))+
  direction_label(colour = "white")+
  # ggtitle("VINI JR. vs South Korea", 
  #         "Successful passes" )
  
  labs( title = "Yahia Attiat-Allah vs Portugal" ,title.size=50,subtitle = "Successful passes" , caption ='@SaidMeaza') + 
  
  
  
  annotate("text", x = 15, y = 8, family = "Trebuchet MS",colour="#ff4d4d",size=5,label = "Successful passes")+
  annotate("text", x = 15, y = 13, family = "Trebuchet MS",colour="#3d85c6",size=5,label = "Key passes")+
  annotate("text", x = 15, y = 18, family = "Trebuchet MS",colour="#3fcf5d",size=5,label = " Assists")


#=======================================================
#Messi and Enzo VS COR   MessiID = 11119     EnzoID=369430
install.packages("png")
install.packages("patchwork")
install.packages("gridExtra")
library("patchwork")                
library(magick)
library(ggplot2)
library(ggsoccer)
library(readxl)
library(dplyr)
library("png")                      
library("gridExtra")
#my_image = readPNG("C:/Users/kokom/Desktop/Screenshot__340_-removebg-preview.png", native = FALSE)

#all game events 
data.source.xls <- read_excel("D:/Projects/football analysis/Messi & Enzo vs CRO/New Microsoft Excel Worksheet.xlsx")

#get Messi and Enzo passes
SuccPassesMessi = data.source.xls %>% 
  filter(`type/displayName`=="Pass" &`outcomeType/displayName`=="Successful"& playerId==11119 )



ppbegin = sqrt( (100-SuccPassesMessi["x"])**2 + (50 - SuccPassesMessi["y"])**2 )
ppend = sqrt( (100-SuccPassesMessi["endX"])**2 + (50 - SuccPassesMessi["endY"])**2 )
ratio = ppend / ppbegin 
progressive= ratio %>% filter(ratio < 0.75)

SuccPassesMessi["ppbegin"]= sqrt( (100-SuccPassesMessi["x"])**2 + (50 - SuccPassesMessi["y"])**2 )
SuccPassesMessi["ppend"]= sqrt( (100-SuccPassesMessi["endX"])**2 + (50 - SuccPassesMessi["endY"])**2 )
SuccPassesMessi["Ratio"]= SuccPassesMessi["ppend"]/SuccPassesMessi["ppbegin"]


ProgressivePassesMessi=SuccPassesMessi %>% filter( Ratio < 0.75)



SuccPassesEnzo = data.source.xls %>% 
  filter(`type/displayName`=="Pass" &`outcomeType/displayName`=="Successful"& playerId==369430 )
#get Messi and Enzo KEY passes and Assists 

KeyPassMessi=data.source.xls%>%filter(playerId==11119) %>% filter_all(any_vars(. %in% c('KeyPass')))
AssistsMessi=data.source.xls%>%filter(playerId==11119) %>% filter_all(any_vars(. %in% c('IntentionalGoalAssist')))
KeyPassEnzo=data.source.xls%>%filter(playerId==369430) %>% filter_all(any_vars(. %in% c('KeyPass')))
AssistsEnzo=data.source.xls%>%filter(playerId==369430) %>% filter_all(any_vars(. %in% c('IntentionalGoalAssist')))

ggp1=ggplot() +
  annotate_pitch(fill = "#333333", colour = "white") +
  geom_segment(data = SuccPassesMessi,colour="#ff4d4d",size =1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data = ProgressivePassesMessi,colour="white",size =1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data = KeyPassMessi,colour="#3d85c6",size = 1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data =AssistsMessi,colour="#3fcf5d",size = 1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  
  
  
  theme_pitch() +
  theme(panel.background = element_rect(fill = "black") , plot.background = element_rect(fill = "black"), title = element_text(colour = "white",size=15))+
  direction_label(colour = "white")+
  labs( title = "L.Messi passes vs Croatia" ,title.size=50) + 

  
  
  annotate("text", x = 20, y = 95, family = "Trebuchet MS",colour="#ff4d4d",size=5,label = "35 Successful passes (85% accuracy)")+
  annotate("text", x = 15, y = 90, family = "Trebuchet MS",colour="white",size=5,label = "4 Progressive Passes")+
  annotate("text", x = 15, y = 85, family = "Trebuchet MS",colour="#3d85c6",size=5,label = "2 Key passes")+
  annotate("text", x = 15, y = 80, family = "Trebuchet MS",colour="#3fcf5d",size=5,label = "1 Assist")

ggp1

ggp2=ggplot() +
  annotate_pitch(fill = "#333333", colour = "white") +
  geom_segment(data = SuccPassesEnzo,colour="#ff4d4d",size =1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data = KeyPassEnzo,colour="#3d85c6",size = 1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data =AssistsEnzo,colour="#3fcf5d",size = 1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +


  #coord_cartesian(xlim = c(40, 100))+   #crop the field

  # scale_y_reverse()+
  #
  # coord_flip()+
  # to make vertical pitch

  theme_pitch() +
  theme(panel.background = element_rect(fill = "black") , plot.background = element_rect(fill = "black"), title = element_text(colour = "white",size=15))+
  direction_label(colour = "white")+
 

  labs( title = "Enzo Fernández passes vs Croatia" ,title.size=50 , caption ='@SaidMeaza') +



  annotate("text", x = 80, y = 13, family = "Trebuchet MS",colour="#ff4d4d",size=5,label = "42 Successful passes (88% accuracy)")+
  annotate("text", x = 80, y = 8, family = "Trebuchet MS",colour="#3d85c6",size=5,label = "1 Key pass")

ggp2



(ggp1 | ggp2)
# ggp_image = ggp +                  # Combine plot & image
#   inset_element(p = my_image,
#                 left = 0.5,
#                 bottom = 0.55,
#                 right = 0.95,
#                 top = 0.95)
ggp_image                           # Draw combined plot

pdf=image_read_pdf("D:/Projects/football analysis/Messi & Enzo vs CRO/Rplot07.pdf")
png=image_convert(pdf,"PNG")
image_trim(png)
#=======================================================
#Antoine Griezmann vs Morocco   ID=80241

library("patchwork")                
library(magick)
library(ggplot2)
library(ggsoccer)
library(readxl)
library(dplyr)
library("png")                      
library("gridExtra")
library(ggnewscale)

data.source.xls <- read_excel("D:/Projects/football analysis/Griezman vs MOR/Griez.xlsx")

#get    Defensive actions
sAerial = data.source.xls %>% 
  filter(`type/displayName`=="Aerial" &playerId==80241&`outcomeType/displayName`=="Successful" )
uAerial = data.source.xls %>% 
  filter(`type/displayName`=="Aerial" &playerId==80241&`outcomeType/displayName`=="Unsuccessful" )

sTackles= data.source.xls %>% 
  filter(`type/displayName`=="Tackle" & playerId==80241&`outcomeType/displayName`=="Successful" )
uTackles= data.source.xls %>% 
  filter(`type/displayName`=="Tackle" & playerId==80241&`outcomeType/displayName`=="Unsuccessful" )


sBallRecovery= data.source.xls %>% 
  filter(`type/displayName`=="BallRecovery" & playerId==80241&`outcomeType/displayName`=="Successful" )
uBallRecovery= data.source.xls %>% 
  filter(`type/displayName`=="BallRecovery" & playerId==80241&`outcomeType/displayName`=="Unsuccessful" )


sClearances= data.source.xls %>% 
  filter(`type/displayName`=="Clearance" & playerId==80241& playerId==80241&`outcomeType/displayName`=="Successful" )
uClearances= data.source.xls %>% 
  filter(`type/displayName`=="Clearance" & playerId==80241 & playerId==80241&`outcomeType/displayName`=="Unsuccessful")


sInterceptions= data.source.xls %>% 
  filter(`type/displayName`=="Interception" & playerId==80241&`outcomeType/displayName`=="Successful" )
uInterceptions= data.source.xls %>% 
  filter(`type/displayName`=="Interception" & playerId==80241 &`outcomeType/displayName`=="Unsuccessful")

def=data.source.xls %>% filter((`type/displayName`=="Tackle"|`type/displayName`=="Aerial"|`type/displayName`=="BallRecovery"|`type/displayName`=="Clearance"|`type/displayName`=="Interception") & playerId==80241&`outcomeType/displayName`=="Successful")



# Get   passes
SuccPasseso = data.source.xls %>% 
  filter(`type/displayName`=="Pass" &`outcomeType/displayName`=="Successful"& playerId==80241 )


# Calculationg progressive passes 
ppbegin = sqrt( (100-SuccPassesGriezo["x"])**2 + (50 - SuccPassesGriezo["y"])**2 )
ppend = sqrt( (100-SuccPassesGriezo["endX"])**2 + (50 - SuccPassesGriezo["endY"])**2 )
ratio = ppend / ppbegin 
progressive= ratio %>% filter(ratio < 0.75)

SuccPassesGriezo["ppbegin"]= sqrt( (100-SuccPassesGriezo["x"])**2 + (50 - SuccPassesGriezo["y"])**2 )
SuccPassesGriezo["ppend"]= sqrt( (100-SuccPassesGriezo["endX"])**2 + (50 - SuccPassesGriezo["endY"])**2 )
SuccPassesGriezo["Ratio"]= SuccPassesGriezo["ppend"]/SuccPassesGriezo["ppbegin"]


ProgressivePassesGriezo=SuccPassesGriezo %>% filter( Ratio < 0.75)



#get Griezo KEY passes and Assists 

KeyPassGriezo=data.source.xls%>%filter(playerId==80241) %>% filter_all(any_vars(. %in% c('KeyPass')))
AssistsGriezo=data.source.xls%>%filter(playerId==80241) %>% filter_all(any_vars(. %in% c('IntentionalGoalAssist')))



#Passing plots 
ggp1=ggplot() +
  annotate_pitch(fill = "#333333", colour = "white") +
  geom_segment(data = SuccPassesGriezo,colour="#ff4d4d",size =1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data = ProgressivePassesGriezo,colour="white",size =1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data = KeyPassGriezo,colour="#3d85c6",size = 1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data =AssistsGriezo,colour="#3fcf5d",size = 1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  
  # scale_y_reverse()+
  # coord_flip()+
  theme_pitch() +   #aspect_ratio = 105/68
  theme(plot.title = element_text(size=25,colour = "yellow", face='bold'),panel.background = element_rect(fill = "black") , plot.background = element_rect(fill = "black"), title = element_text(colour = "white",size=15))+
  direction_label(colour = "white")+
  labs( title = "Antoine Griezmann passes vs Morocco" , caption = "Twitter : @SaidMeaza") + 



  
  
  annotate("text", x = 17, y = 19, family = "Trebuchet MS",colour="#ff4d4d",size=5,label = "24/30 Successful passes")+
  annotate("text", x = 18, y = 14, family = "Trebuchet MS",colour="white",size=5,label = "4 Progressive Passes(key passes)")+
  annotate("text", x = 15, y = 9, family = "Trebuchet MS",colour="#3d85c6",size=5,label = "4 Key passes")+
  annotate("text", x = 15, y = 4, family = "Trebuchet MS",colour="#3fcf5d",size=5,label = "0 Assists")

ggp1

# Defensive actions plot
ggp2<-ggplot() +
  annotate_pitch(fill = "#333333", colour = "white") +
  
  
  # Just for Legend
  geom_point(data = def, aes(x = x, y = y,stroke =3, shape = `type/displayName`,color= `type/displayName`),size = 7, alpha = 1)+
  scale_color_manual(values=c("#B00000","#ef9021","#00004b" ,"#3fcf5d","#25b5af"))+
  scale_shape_manual(values = c(16,15,18,4,17))+
  #======================================================
  
  geom_point(data = sAerial, aes(x = x, y = y,stroke =3), shape = 16,colour="#B00000" ,size = 7, alpha = 1)+

  geom_point(data = uAerial, aes(x = x, y = y,stroke =3 ),shape = 1,colour="#B00000" ,size = 6, alpha = 1,)+



  geom_point(data = sTackles, aes(x = x, y = y ,stroke =3), shape = 17,colour="#25b5af" ,size = 6, alpha = 1)+

  geom_point(data = uTackles, aes(x = x, y = y,stroke =3 ),shape = 2,colour="#25b5af" ,size = 6, alpha = 1)+



  geom_point(data = sInterceptions, aes(x = x, y = y,stroke =3 ), shape = 4,colour="#3fcf5d" ,size = 6, alpha = 1)+

  geom_point(data = uInterceptions, aes(x = x, y = y ,stroke =3),shape = 8,colour="#3fcf5d" ,size = 6, alpha = 1)+



  geom_point(data = sBallRecovery, aes(x = x, y = y ,stroke =3), shape = 15,colour="#ef9021" ,size = 6, alpha = 1)+

  geom_point(data = uBallRecovery, aes(x = x, y = y,stroke =3 ),shape = 0,colour="#ef9021" ,size = 6, alpha = 1)+




  geom_point(data = sClearances, aes(x = x, y = y ,stroke =3), shape = 18,colour="#00004b" ,size = 7, alpha = 1)+

  geom_point(data = uClearances, aes(x = x, y = y,stroke =3 ),shape = 5,colour="#00004b" ,size = 7, alpha = 1)+


  
  # scale_y_reverse()+
  # coord_flip()+
  theme_pitch() +   #aspect_ratio = 105/68
  theme(legend.spacing.x = unit(2, 'cm'),legend.key.size = unit(2, 'cm'),legend.text = element_text(size=20),legend.position ="right",legend.key = element_rect(colour = "transparent", fill = "white"),plot.title = element_text(size=25,colour = "yellow", face='bold'),panel.background = element_rect(fill = "black") , plot.background = element_rect(fill = "black"), title = element_text(colour = "white",size=15))+
  direction_label(colour = "white")+
  labs(title = "Antoine Griezmann Defensive actions vs Morocco" ) 
  
  
  
ggp2




ggp2 + ggp1



pdf=image_read_pdf("D:/Projects/football analysis/Griezman vs MOR/Griezo.pdf")
png=image_convert(pdf,"PNG")
image_trim(png)
  #=============================================================
pdf=image_read_pdf("D:/Projects/football analysis/Grieazo vs Messi/Messi vs Griezo.pdf")
png=image_convert(pdf,"PNG")
image_trim(png)
#===============================================================
install.packages('httr','tibble','tidyr','dplyr','purrr','janitor')
install.packages('httr')
install.packages('tibble')
install.packages('tidyr')
install.packages('purrr')
install.packages('janitor')
install.packages("writexl")
library("writexl")
library('httr')
library('tibble')
library('tidyr')
library('dplyr')
library('purrr')
library('janitor')


stats_resp <- GET(url ='https://fdh-api.fifa.com/v1/stats/match/128083/players.json', warn = F)
stats <- stats_resp |>
  content() |>
  enframe( 'player_id' , 'values') |>
  unnest_longer(values) |>
  hoist( 
  values , 
  'stat' = 1 ,
  'value' = 2 
  )|>
  
  select(-values) |>
  mutate( 
      across(player_id, as.integer)
  )|>
  pivot_wider( 
      names_from = stat, 
      values_from = value 
  )|>  
  clean_names() 
  
  
squad_resp <- GET( url ='https://play.fifa.com/json/fantasy/squads_fifa.json')  
player_resp <- GET( url ='https://play.fifa.com/json/fantasy/players.json') 

squad_cont <- content(squad_resp) 
player_cont <- content(player_resp) 
    
squads <- tibble( 
        squad_id = squad_cont|> map_int(~pluck(.x,'id')),
        country =  squad_cont|> map_chr(~pluck(.x,'name'))
)

players <- tibble( 
    player_id = player_cont|> map_int(~pluck(.x,'id')), 
    squad_id = player_cont|> map_int(~pluck(.x,'squadId')),  
    player = player_cont|> map_chr(~pluck(.x,'name'))
)
stats |> 
  inner_join(players,by = 'player_id')  |> 
  inner_join(squads, by = 'squad_id') |> 
  select(player, country, linebreaks_attempted_completed) |> 
  arrange( desc( linebreaks_attempted_completed)) |> 
  slice_max(linebreaks_attempted_completed, n=10 ) 


write_xlsx(stats,"D:/Projects/football analysis/ARGvsFRA.xlsx")


write_xlsx(squads,"D:/Projects/football analysis/Squad_id.xlsx")
write_xlsx(players,"D:/Projects/football analysis/Player_id.xlsx")

#================
matches_resp <- GET('https://api.fifa.com/api/v3/calendar/matches?language=en&count=500&idSeason=255711')
results <- content(matches_resp) |> pluck('Results')

## for incomplete matches (anything beyond the group stage at the moment), there will be `NULL`s
##   which causes `pluck()` to throw an error. using a `.default` of `NA` fixes the issue.
pluck2 <- partial(pluck, .default = NA_character_, ... = )

map_pluck_chr <- function(x, ...) {
  map_chr(x, pluck2, ...)
}

map_pluck_results_chr <- function(...) {
  results |> map_pluck_chr(...)
}

matches <- tibble(
  ## this won't join with the match stats, but it seems to be Fifa's "true" match ID
  match_id = map_pluck_results_chr('IdMatch') |> as.integer(),
  ## use this to join with the match stats
  results_id = map_pluck_results_chr('Properties', 'IdIFES') |> as.integer(),
  home_abbr = map_pluck_results_chr('Home', 'IdCountry'),
  away_abbr = map_pluck_results_chr('Away', 'IdCountry')
)

#=====================================================================
#Achraf Hakimi vs COR   ID=320834

library("patchwork")                
library(magick)
library(ggplot2)
library(ggsoccer)
library(readxl)
library(dplyr)
library("png")                      
library("gridExtra")
library(ggnewscale)

data.source.xls <- read_excel("D:/Projects/football analysis/Hakimi vs CRO/Hakimi.xlsx")

#get Defensive actions
sAerial = data.source.xls %>% 
  filter(`type/displayName`=="Aerial" &playerId==320834&`outcomeType/displayName`=="Successful" )
uAerial = data.source.xls %>% 
  filter(`type/displayName`=="Aerial" &playerId==320834&`outcomeType/displayName`=="Unsuccessful" )

sTackles= data.source.xls %>% 
  filter(`type/displayName`=="Tackle" & playerId==320834&`outcomeType/displayName`=="Successful" )
uTackles= data.source.xls %>% 
  filter(`type/displayName`=="Tackle" & playerId==320834&`outcomeType/displayName`=="Unsuccessful" )


sBallRecovery= data.source.xls %>% 
  filter(`type/displayName`=="BallRecovery" & playerId==320834&`outcomeType/displayName`=="Successful" )
uBallRecovery= data.source.xls %>% 
  filter(`type/displayName`=="BallRecovery" & playerId==320834&`outcomeType/displayName`=="Unsuccessful" )


sClearances= data.source.xls %>% 
  filter(`type/displayName`=="Clearance" & playerId==320834&`outcomeType/displayName`=="Successful" )
uClearances= data.source.xls %>% 
  filter(`type/displayName`=="Clearance" & playerId==320834 &`outcomeType/displayName`=="Unsuccessful")


sInterceptions= data.source.xls %>% 
  filter(`type/displayName`=="Interception" & playerId==320834&`outcomeType/displayName`=="Successful" )
uInterceptions= data.source.xls %>% 
  filter(`type/displayName`=="Interception" & playerId==320834 &`outcomeType/displayName`=="Unsuccessful")

def=data.source.xls %>% filter((`type/displayName`=="Tackle"|`type/displayName`=="Aerial"|`type/displayName`=="BallRecovery"|`type/displayName`=="Clearance"|`type/displayName`=="Interception") & playerId==320834)



# Get passes
SuccPasses = data.source.xls %>% 
  filter(`type/displayName`=="Pass" &`outcomeType/displayName`=="Successful"& playerId==320834 )


# Calculationg progressive passes 
ppbegin = sqrt( (100-SuccPasses["x"])**2 + (50 - SuccPasses["y"])**2 )
ppend = sqrt( (100-SuccPasses["endX"])**2 + (50 - SuccPasses["endY"])**2 )
ratio = ppend / ppbegin 
progressive= ratio %>% filter(ratio < 0.75)

SuccPasses["ppbegin"]= sqrt( (100-SuccPasses["x"])**2 + (50 - SuccPasses["y"])**2 )
SuccPasses["ppend"]= sqrt( (100-SuccPasses["endX"])**2 + (50 - SuccPasses["endY"])**2 )
SuccPasses["Ratio"]= SuccPasses["ppend"]/SuccPasses["ppbegin"]


ProgressivePasses =SuccPasses  %>% filter( Ratio < 0.75)



#get KEY passes and Assists 

KeyPass =data.source.xls%>%filter(playerId==320834) %>% filter_all(any_vars(. %in% c('KeyPass')))
Assists =data.source.xls%>%filter(playerId==320834) %>% filter_all(any_vars(. %in% c('IntentionalGoalAssist')))



#Passing plots 
ggp1=ggplot() +
  annotate_pitch(fill = "#333333", colour = "white") +
  geom_segment(data = SuccPasses ,colour="#ff4d4d",size =1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data = ProgressivePasses ,colour="white",size =1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data = KeyPass ,colour="#3d85c6",size = 1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data =Assists ,colour="#3fcf5d",size = 1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  
  scale_y_reverse()+
  coord_flip()+
  theme_pitch(aspect_ratio = 105/68) +
  theme(plot.title = element_text(size=20,colour = "yellow", face='bold'),panel.background = element_rect(fill = "black") , plot.background = element_rect(fill = "black"))+
  direction_label(colour = "white",text_size = 0  )+
  labs( title = "Achraf Hakimi passes vs CRO" , caption = "Twitter : @SaidMeaza\n Data:Whoscored") + 
  
  annotate("text", x = 35, y = 70, family = "Trebuchet MS",colour="#ff4d4d",size=5,label = "60/71 Successful passes")+
  annotate("text", x = 30, y = 70, family = "Trebuchet MS",colour="white",size=5,label = "3 Progressive Passes")+
  annotate("text", x = 25, y = 75, family = "Trebuchet MS",colour="#3d85c6",size=5,label = "3 Key passes")+
  annotate("text", x = 20, y = 75, family = "Trebuchet MS",colour="#3fcf5d",size=5,label = "0 Assists")

ggp1

# Defensive actions plot
ggp2<-ggplot() +
  annotate_pitch(fill = "#333333", colour = "white") +
  # Just for Legend
  geom_point(data = def, aes(x = x, y = y,stroke =3, shape = `type/displayName`,color= `type/displayName`),size = 7, alpha = 1)+
  scale_color_manual(values=c("#B00000","#ef9021","#00004b" ,"#3fcf5d","#25b5af"))+
  scale_shape_manual(values = c(1,15,18,4,17))+
  #======================================================

  geom_point(data = sInterceptions, aes(x = x, y = y,stroke =3 ), shape = 4,colour="#3fcf5d" ,size = 6, alpha = 1)+
  
  geom_point(data = uInterceptions, aes(x = x, y = y ,stroke =3),shape = 8,colour="#3fcf5d" ,size = 6, alpha = 1)+
  

  geom_point(data = sAerial, aes(x = x, y = y,stroke =3), shape = 16,colour="#B00000" ,size = 7, alpha = 1)+
  
  geom_point(data = uAerial, aes(x = x, y = y,stroke =3 ),shape = 1,colour="#B00000" ,size = 6, alpha = 1,)+
  
  
  
  geom_point(data = sTackles, aes(x = x, y = y ,stroke =3), shape = 17,colour="#25b5af" ,size = 6, alpha = 1)+
  
  geom_point(data = uTackles, aes(x = x, y = y,stroke =3 ),shape = 2,colour="#25b5af" ,size = 6, alpha = 1)+
  
  
  
  geom_point(data = sBallRecovery, aes(x = x, y = y ,stroke =3), shape = 15,colour="#ef9021" ,size = 6, alpha = 1)+
  
  geom_point(data = uBallRecovery, aes(x = x, y = y,stroke =3 ),shape = 0,colour="#ef9021" ,size = 6, alpha = 1)+
  
  

  geom_point(data = sClearances, aes(x = x, y = y ,stroke =3), shape = 18,colour="#00004b" ,size = 7, alpha = 1)+
  
  geom_point(data = uClearances, aes(x = x, y = y,stroke =3 ),shape = 5,colour="#00004b" ,size = 7, alpha = 1)+
  
  
  

  scale_y_reverse()+
  coord_flip()+
  theme_pitch(aspect_ratio = 105/68) +
  theme( legend.key.size = unit(1, 'cm'),legend.position ="right",legend.key = element_rect(colour = "transparent", fill = "white"),plot.title = element_text(size=25,colour = "yellow", face='bold'),panel.background = element_rect(fill = "black") , plot.background = element_rect(fill = "black"), title = element_text(colour = "white",size=15))+
  direction_label(colour = "white",text_size = 0 )+
  labs(title = "Achraf Hakimi Defensive actions \nvs CRO" )



ggp2




ggp2 + ggp1



pdf=image_read_pdf("D:/Projects/football analysis/Griezman vs MOR/ .pdf")
png=image_convert(pdf,"PNG")
image_trim(png)
#======================================================================================
library("patchwork")                
library(magick)
library(ggplot2)
library(ggsoccer)
library(readxl)
library(dplyr)
library("png")                      
library("gridExtra")
library(ggnewscale)

data.source.xls <- read_excel("D:/Projects/football analysis/Hakimi vs CRO/Hakimi.xlsx")

#get Defensive actions
sAerial = data.source.xls %>% 
  filter(`type/displayName`=="Aerial" &playerId==320834&`outcomeType/displayName`=="Successful" )
uAerial = data.source.xls %>% 
  filter(`type/displayName`=="Aerial" &playerId==320834&`outcomeType/displayName`=="Unsuccessful" )

sTackles= data.source.xls %>% 
  filter(`type/displayName`=="Tackle" & playerId==320834&`outcomeType/displayName`=="Successful" )
uTackles= data.source.xls %>% 
  filter(`type/displayName`=="Tackle" & playerId==320834&`outcomeType/displayName`=="Unsuccessful" )


sBallRecovery= data.source.xls %>% 
  filter(`type/displayName`=="BallRecovery" & playerId==320834&`outcomeType/displayName`=="Successful" )
uBallRecovery= data.source.xls %>% 
  filter(`type/displayName`=="BallRecovery" & playerId==320834&`outcomeType/displayName`=="Unsuccessful" )


sClearances= data.source.xls %>% 
  filter(`type/displayName`=="Clearance" & playerId==320834&`outcomeType/displayName`=="Successful" )
uClearances= data.source.xls %>% 
  filter(`type/displayName`=="Clearance" & playerId==320834 &`outcomeType/displayName`=="Unsuccessful")


sInterceptions= data.source.xls %>% 
  filter(`type/displayName`=="Interception" & playerId==320834&`outcomeType/displayName`=="Successful" )
uInterceptions= data.source.xls %>% 
  filter(`type/displayName`=="Interception" & playerId==320834 &`outcomeType/displayName`=="Unsuccessful")

def=data.source.xls %>% filter((`type/displayName`=="Tackle"|`type/displayName`=="Aerial"|`type/displayName`=="BallRecovery"|`type/displayName`=="Clearance"|`type/displayName`=="Interception") & playerId==320834)



# Get passes
SuccPasses = data.source.xls %>% 
  filter(`type/displayName`=="Pass" &`outcomeType/displayName`=="Successful"& playerId==320834 )


# Calculationg progressive passes 
ppbegin = sqrt( (100-SuccPasses["x"])**2 + (50 - SuccPasses["y"])**2 )
ppend = sqrt( (100-SuccPasses["endX"])**2 + (50 - SuccPasses["endY"])**2 )
ratio = ppend / ppbegin 
progressive= ratio %>% filter(ratio < 0.75)

SuccPasses["ppbegin"]= sqrt( (100-SuccPasses["x"])**2 + (50 - SuccPasses["y"])**2 )
SuccPasses["ppend"]= sqrt( (100-SuccPasses["endX"])**2 + (50 - SuccPasses["endY"])**2 )
SuccPasses["Ratio"]= SuccPasses["ppend"]/SuccPasses["ppbegin"]


ProgressivePasses =SuccPasses  %>% filter( Ratio < 0.75)



#get KEY passes and Assists 

KeyPass =data.source.xls%>%filter(playerId==320834) %>% filter_all(any_vars(. %in% c('KeyPass')))
Assists =data.source.xls%>%filter(playerId==320834) %>% filter_all(any_vars(. %in% c('IntentionalGoalAssist')))



#Passing plots 
ggplot() +
  annotate_pitch(fill = "#333333", colour = "white") +
  geom_segment(data = SuccPasses ,colour="#ff4d4d",size =1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data = ProgressivePasses ,colour="white",size =1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data = KeyPass ,colour="#3d85c6",size = 1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data =Assists ,colour="#3fcf5d",size = 1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  
  scale_y_reverse()+
  coord_flip()+
  theme_pitch(aspect_ratio = 105/68) +
  theme(plot.title = element_text(size=20,colour = "yellow", face='bold'),panel.background = element_rect(fill = "black") , plot.background = element_rect(fill = "black"))+
  direction_label(colour = "white",text_size = 0  )+
  labs( title = "Achraf Hakimi passes vs CRO" , caption = "Twitter : @SaidMeaza\n Data:Whoscored") + 
  
  annotate("text", x = 35, y = 70, family = "Trebuchet MS",colour="#ff4d4d",size=5,label = "60/71 Successful passes")+
  annotate("text", x = 30, y = 70, family = "Trebuchet MS",colour="white",size=5,label = "3 Progressive Passes")+
  annotate("text", x = 25, y = 75, family = "Trebuchet MS",colour="#3d85c6",size=5,label = "3 Key passes")+
  annotate("text", x = 20, y = 75, family = "Trebuchet MS",colour="#3fcf5d",size=5,label = "0 Assists")

install.packages("pdftools")
pdf=image_read_pdf("D:/Projects/football analysis/Hakimi vs CRO/Rplot07.pdf")
png=image_convert(pdf,"PNG")
image_trim(png)
#===============================================================
#Messi vs FRA             MessiID = 11119 
install.packages("cowplot")
library("patchwork")                
library(magick)
library(ggplot2)
library(ggsoccer)
library(readxl)
library(dplyr)
library("png")                      
library("gridExtra")
library(ggnewscale)
library(cowplot)


data.source.xls <- read_excel("D:/Projects/football analysis/ARG vs FRA/ARG.xlsx")

# Get Messi passes
SuccPasses  = data.source.xls %>% 
  filter(`type/displayName`=="Pass" &`outcomeType/displayName`=="Successful"& playerId==11119 )


# Calculationg progressive passes 
ppbegin = sqrt( (100-SuccPasses ["x"])**2 + (50 - SuccPasses ["y"])**2 )
ppend = sqrt( (100-SuccPasses ["endX"])**2 + (50 - SuccPasses ["endY"])**2 )
ratio = ppend / ppbegin 
progressive= ratio %>% filter(ratio < 0.75)

SuccPasses ["ppbegin"]= sqrt( (100-SuccPasses ["x"])**2 + (50 - SuccPasses ["y"])**2 )
SuccPasses ["ppend"]= sqrt( (100-SuccPasses ["endX"])**2 + (50 - SuccPasses ["endY"])**2 )
SuccPasses ["Ratio"]= SuccPasses ["ppend"]/SuccPasses ["ppbegin"]


ProgressivePasses =SuccPasses  %>% filter( Ratio < 0.75)



#get KEY passes and Assists 

KeyPass =data.source.xls%>%filter(playerId==11119) %>% filter_all(any_vars(. %in% c('KeyPass')))
Assists =data.source.xls%>%filter(playerId==11119) %>% filter_all(any_vars(. %in% c('IntentionalGoalAssist')))



#Passing plots 
ggplot() +
  annotate_pitch(fill = "#333333", colour = "white") +
  geom_segment(data = SuccPasses ,colour="#ff4d4d",size =1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data = ProgressivePasses ,colour="white",size =1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data = KeyPass ,colour="#3d85c6",size = 1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data =Assists ,colour="#3fcf5d",size = 1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  
  scale_y_reverse()+
  coord_flip()+
  theme_pitch(aspect_ratio = 105/68) +   #
  theme(plot.title = element_text(size=15,colour = "yellow", face='bold'),panel.background = element_rect(fill = "black") , plot.background = element_rect(fill = "black"), title = element_text(colour = "white",size=15))+
  direction_label(colour = "white",text_size = 0 )+
  labs( title = "L.Messi(WORLD CUP CHAMPION!!!) Passes vs FRA" , caption = "Twitter : @SaidMeaza\nData:Opta via WhoScored") + 
  
  
  
  annotate("text", x = 23, y = 25, family = "Trebuchet MS",colour="#ff4d4d",size=5,label = "48/59 Successful passes")+
  annotate("text", x = 15, y = 25, family = "Trebuchet MS",colour="white",size=5,label = "10 Progressive Passes")+
  annotate("text", x = 10, y = 25, family = "Trebuchet MS",colour="#3d85c6",size=5,label = "2 Key passes")+
  annotate("text", x = 5, y = 25, family = "Trebuchet MS",colour="#3fcf5d",size=5,label = "0 Assists")


install.packages("pdftools")
pdf=image_read_pdf("D:/Projects/football analysis/ARG vs FRA/MessivsFRA.pdf")
png=image_convert(pdf,"PNG")
image_trim(png)

#===============================================================
#Enzo vs FRA        EnzoID=369430
library("patchwork")                
library(magick)
library(ggplot2)
library(ggsoccer)
library(readxl)
library(dplyr)
library("png")                      
library("gridExtra")
library(ggnewscale)
library("tidyverse")
library('cowplot')

data.source.xls <- read_excel("D:/Projects/football analysis/ARG vs FRA/ARG.xlsx")

#get Defensive actions
sAerial = data.source.xls %>% 
  filter(`type/displayName`=="Aerial" &playerId==369430&`outcomeType/displayName`=="Successful" )
uAerial = data.source.xls %>% 
  filter(`type/displayName`=="Aerial" &playerId==369430&`outcomeType/displayName`=="Unsuccessful" )

sTackles= data.source.xls %>% 
  filter(`type/displayName`=="Tackle" & playerId==369430&`outcomeType/displayName`=="Successful" )
uTackles= data.source.xls %>% 
  filter(`type/displayName`=="Tackle" & playerId==369430&`outcomeType/displayName`=="Unsuccessful" )


sBallRecovery= data.source.xls %>% 
  filter(`type/displayName`=="BallRecovery" & playerId==369430&`outcomeType/displayName`=="Successful" )
uBallRecovery= data.source.xls %>% 
  filter(`type/displayName`=="BallRecovery" & playerId==369430&`outcomeType/displayName`=="Unsuccessful" )


sClearances= data.source.xls %>% 
  filter(`type/displayName`=="Clearance" & playerId==369430& playerId==369430&`outcomeType/displayName`=="Successful" )
uClearances= data.source.xls %>% 
  filter(`type/displayName`=="Clearance" & playerId==369430 & playerId==369430&`outcomeType/displayName`=="Unsuccessful")


sInterceptions= data.source.xls %>% 
  filter(`type/displayName`=="Interception" & playerId==369430&`outcomeType/displayName`=="Successful" )
uInterceptions= data.source.xls %>% 
  filter(`type/displayName`=="Interception" & playerId==369430 &`outcomeType/displayName`=="Unsuccessful")

sdef=data.source.xls %>% filter((`type/displayName`=="Tackle"|`type/displayName`=="Aerial"|`type/displayName`=="BallRecovery"|`type/displayName`=="Clearance"|`type/displayName`=="Interception") & playerId==369430&`outcomeType/displayName`=="Successful")
udef=data.source.xls %>% filter((`type/displayName`=="Tackle"|`type/displayName`=="Aerial"|`type/displayName`=="BallRecovery"|`type/displayName`=="Clearance"|`type/displayName`=="Interception") & playerId==369430&`outcomeType/displayName`=="Unsuccessful")



# Get   passes
SuccPasses  = data.source.xls %>% 
  filter(`type/displayName`=="Pass" &`outcomeType/displayName`=="Successful"& playerId==369430 )


# Calculationg progressive passes 
ppbegin = sqrt( (100-SuccPasses ["x"])**2 + (50 - SuccPasses ["y"])**2 )
ppend = sqrt( (100-SuccPasses ["endX"])**2 + (50 - SuccPasses ["endY"])**2 )
ratio = ppend / ppbegin 
progressive= ratio %>% filter(ratio < 0.75)

SuccPasses ["ppbegin"]= sqrt( (100-SuccPasses ["x"])**2 + (50 - SuccPasses ["y"])**2 )
SuccPasses ["ppend"]= sqrt( (100-SuccPasses ["endX"])**2 + (50 - SuccPasses ["endY"])**2 )
SuccPasses ["Ratio"]= SuccPasses ["ppend"]/SuccPasses ["ppbegin"]


ProgressivePasses =SuccPasses  %>% filter( Ratio < 0.75)



#get   KEY passes and Assists 

KeyPass =data.source.xls%>%filter(playerId==369430) %>% filter_all(any_vars(. %in% c('KeyPass')))
Assists =data.source.xls%>%filter(playerId==369430) %>% filter_all(any_vars(. %in% c('IntentionalGoalAssist')))



#Passing plots 
ggp1=ggplot() +
  annotate_pitch(fill = "#333333", colour = "white") +
  geom_segment(data = SuccPasses ,colour="#ff4d4d",size =1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data = ProgressivePasses ,colour="white",size =1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data = KeyPass ,colour="#3d85c6",size = 1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data =Assists ,colour="#3fcf5d",size = 1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  
  scale_y_reverse()+
  coord_flip()+
  theme_pitch(aspect_ratio = 105/68) +   #
  theme(plot.title = element_text(size=15,colour = "yellow", face='bold'),panel.background = element_rect(fill = "black") , plot.background = element_rect(fill = "black"), title = element_text(colour = "white",size=15))+
  direction_label(colour = "white",text_size = 0)+
  labs( title = "Enzo Fernández Passes vs France"  ,caption = "Twitter : @SaidMeaza\nData:Opta via WhoScored") + 
  
  
  
  
  
  annotate("text", x = 15, y = 26, family = "Trebuchet MS",colour="#ff4d4d",size=5,label = "77/91 Successful passes")+
  annotate("text", x = 10, y = 23, family = "Trebuchet MS",colour="white",size=5,label = "1 Progressive Passes")+
  annotate("text", x = 5, y = 15, family = "Trebuchet MS",colour="#3d85c6",size=5,label = "1 Key passes")
  #annotate("text", x = 15, y = 4, family = "Trebuchet MS",colour="#3fcf5d",size=5,label = "0 Assists")

ggp1

# Defensive actions plot
ggp2<-ggplot() +
  annotate_pitch(fill = "#333333", colour = "white") +
  
  # Just for Legend
  geom_point(data = sdef, aes(x = x, y = y,stroke =3, shape = factor(`type/displayName`),color= factor(`type/displayName`)),size = 7, alpha = 1)+
  scale_color_manual(values=c("#B00000","#ef9021","#25b5af","#00004b" ,"#3fcf5d"))+
  scale_shape_manual(values = c(16,15,17,4,18))+
  # geom_point(data = udef, aes(x = x, y = y,stroke =3, shape = factor(`type/displayName`),color= factor(`type/displayName`)),size = 7, alpha = 1)+
  # #scale_color_manual(values=c("#B00000","#ef9021","#00004b" ,"#3fcf5d","#25b5af"))+
  # scale_shape_manual(values = c(0,1,2,5,8))+
  #======================================================

# geom_point(data = sAerial, aes(x = x, y = y,stroke =3), shape = 16,colour="#B00000" ,size = 7, alpha = 1)+
#   
   geom_point(data = uAerial, aes(x = x, y = y,stroke =3 ),shape = 1,colour="#B00000" ,size = 6, alpha = 1,)+
#   
#   
  
  
  
#   geom_point(data = sBallRecovery, aes(x = x, y = y ,stroke =3), shape = 15,colour="#ef9021" ,size = 6, alpha = 1)+
  #   
  geom_point(data = uBallRecovery, aes(x = x, y = y,stroke =3 ),shape = 0,colour="#B00000" ,size = 6, alpha = 1)+
  #   
  #   
  
  
  
  #   geom_point(data = sClearances, aes(x = x, y = y ,stroke =3), shape = 18,colour="#00004b" ,size = 7, alpha = 1)+
  #   
  geom_point(data = uClearances, aes(x = x, y = y,stroke =3 ),shape = 5,colour="#ef9021" ,size = 7, alpha = 1)+
  
  
  
  #   geom_point(data = sInterceptions, aes(x = x, y = y,stroke =3 ), shape = 4,colour="#3fcf5d" ,size = 6, alpha = 1)+
  #   
  geom_point(data = uInterceptions, aes(x = x, y = y ,stroke =3),shape = 8,colour="#00004b" ,size = 6, alpha = 1)+
  
  
  
  
  #geom_point(data = sTackles, aes(x = x, y = y ,stroke =3), shape = 17,colour="#25b5af" ,size = 6, alpha = 1)+

  geom_point(data = uTackles, aes(x = x, y = y,stroke =5 ),shape = 2,colour="#25b5af" ,size = 5, alpha = 1)+

#   

#   
#   
#   

#   
#   
#   

  
  
  scale_y_reverse()+
  coord_flip()+
  theme_pitch(aspect_ratio = 105/68) +   #
  theme(legend.background = element_rect(fill  = "black") ,legend.key = element_rect(fill ="black"),legend.position = "top",legend.spacing.x = unit(0.5, 'cm'),legend.justification = c('left','bottom'),legend.key.size = unit(1, 'cm'), title = element_blank(),legend.text = element_text(size=12,colour = "white"),plot.title = element_text(size=15,colour = "yellow", face='bold'),panel.background = element_rect(fill = "black") , plot.background = element_rect(fill = "black"))+
  direction_label(colour = "white",text_size = 0)+
  labs(title = "Enzo Fernández Defensive actions vs Farnce" ) 
  #legend.position =c(0.72, 0.82),  


ggp2
 
# plot1=ggp2+ggp1
# i=image_read("D:/Projects/football analysis/ARG vs FRA/p100910386.png")
# ggdraw(plot1)+ draw_image(i,  x = .2, y = .43, scale = .13) 

ggp1+ggp2



pdf=image_read_pdf("D:/Projects/football analysis/ARG vs FRA/Enzo vs France.pdf")
png=image_convert(pdf,"PNG")
image_trim(png)


#===============================================================================================
#De bruyne vs LIV     ID:73084
library("patchwork")                
library(magick)
library(ggplot2)
library(ggsoccer)
library(readxl)
library(dplyr)
library("png")                      
library("gridExtra")
library(ggnewscale)
library("tidyverse")
library('cowplot')

data.source.xls <- read_excel("D:/Projects/football analysis/De Bruyne vs LIV/DE.xlsx")


# Get   passes
SuccPasses  = data.source.xls %>% 
  filter(`type/displayName`=="Pass" &`outcomeType/displayName`=="Successful"& playerId==73084 )


# Calculationg progressive passes 
ppbegin = sqrt( (100-SuccPasses ["x"])**2 + (50 - SuccPasses ["y"])**2 )
ppend = sqrt( (100-SuccPasses ["endX"])**2 + (50 - SuccPasses ["endY"])**2 )
ratio = ppend / ppbegin 
progressive= ratio %>% filter(ratio < 0.75)

SuccPasses ["ppbegin"]= sqrt( (100-SuccPasses ["x"])**2 + (50 - SuccPasses ["y"])**2 )
SuccPasses ["ppend"]= sqrt( (100-SuccPasses ["endX"])**2 + (50 - SuccPasses ["endY"])**2 )
SuccPasses ["Ratio"]= SuccPasses ["ppend"]/SuccPasses ["ppbegin"]


ProgressivePasses =SuccPasses  %>% filter( Ratio < 0.75)


#get KEY passes and Assists 

KeyPass =data.source.xls%>%filter(playerId==73084) %>% filter_all(any_vars(. %in% c('KeyPass')))
Assists =data.source.xls%>%filter(playerId==73084) %>% filter_all(any_vars(. %in% c('IntentionalGoalAssist')))



#get Defensive actions

uAerial = data.source.xls %>% 
  filter(`type/displayName`=="Aerial" &playerId==73084&`outcomeType/displayName`=="Unsuccessful" )


uTackles= data.source.xls %>% 
  filter(`type/displayName`=="Tackle" & playerId==73084&`outcomeType/displayName`=="Unsuccessful" )



sdef=data.source.xls %>% filter((`type/displayName`=="Tackle"|`type/displayName`=="Aerial"|`type/displayName`=="BallRecovery"|`type/displayName`=="Clearance"|`type/displayName`=="Interception"|`type/displayName`=="BlockedPass") & playerId==73084&`outcomeType/displayName`=="Successful")





#get all his touches (ball recovery included)
data.source.xls <- read_excel("D:/Projects/football analysis/De Bruyne vs LIV/DE.xlsx")
AllTouches=data.source.xls %>% filter(playerId==73084&(`type/displayName`=="Aerial"|`type/displayName`=="BallRecovery"|`type/displayName`=="BlockedPass"|`type/displayName`=="Clearance"|`type/displayName`=="Foul"|`type/displayName`=="Goal" |`type/displayName`=="Interception"|`type/displayName`=="MissedShots"|`type/displayName`=="SavedShot"|`type/displayName`=="Pass"|`type/displayName`=="Tackle"|`type/displayName`=="TakeOn"|`type/displayName`=="BallTouch"))



#Passing plots 
ggplot() +
  annotate_pitch(fill = "#333333", colour = "white") +
  geom_segment(data = SuccPasses ,colour="#ff4d4d",size =1,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#ff4d4d") +
  geom_segment(data = ProgressivePasses ,colour="white",size =1,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "white") +
  geom_segment(data = KeyPass ,colour="#3d85c6",size = 1.5,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#3d85c6") +
  geom_segment(data =Assists ,colour="#3fcf5d",size = 2,aes(x = x, y = y, xend = endX, yend = endY),
               arrow = arrow(length = unit(0.20, "cm"), type = "closed"),arrow.fill = "#3fcf5d") +
  
  scale_y_reverse()+
  coord_flip()+
  scale_x_continuous(limits =c(-15, 100) )+
  theme_pitch(aspect_ratio = 105/68) +   #
  theme(plot.title = element_text(size=15,colour = "yellow", face='bold'),panel.background = element_rect(fill = "black") , plot.background = element_rect(fill = "black"), title = element_text(colour = "white",size=15))+
  direction_label(colour = "white",text_size = 0)+
  labs( title = "Passing map" ) + 
  
  

  
  
  annotate("text", x = -8, y = 75, family = "Trebuchet MS",colour="#ff4d4d",size=5,label = "41/59 Successful passes")+
  annotate("text", x = -13, y = 75, family = "Trebuchet MS",colour="white",size=5,label = "9 Progressive Passes")+
  annotate("text", x = -8, y = 30, family = "Trebuchet MS",colour="#3d85c6",size=5,label = "5 Key passes")+
  annotate("text", x = -13, y = 30, family = "Trebuchet MS",colour="#3fcf5d",size=5,label = "2 Assists")


#Debruyne defensive actions vs liverpool


ggplot() +
  annotate_pitch(fill = "#333333", colour = "white") +
  
  # Just for Legend
  geom_point(data = sdef, aes(x = x, y = y,stroke =3, shape = factor(`type/displayName`),color= factor(`type/displayName`)),size = 7, alpha = 1)+
  scale_color_manual(values=c("#B00000","#ef9021","#ef9021","#25b5af","#00004b" ,"#3fcf5d") )+
  scale_shape_manual(values = c(16,4,4,17,15,18))+
  
  
  geom_point(data = uAerial, aes(x = x, y = y,stroke =3 ),shape = 1,colour="#B00000" ,size = 6, alpha = 1,)+
    
  
  geom_point(data = uTackles, aes(x = x, y = y,stroke =5 ),shape = 2,colour="#25b5af" ,size = 5, alpha = 1)+
  
  scale_x_continuous(limits = c(-15, 100))+
  scale_y_reverse()+
  coord_flip()+
  theme_pitch(aspect_ratio = 105/68) +   #
  theme(legend.background = element_rect(fill  = "black") ,legend.key = element_rect(fill ="black"),legend.position = "none",legend.spacing.x = unit(0, 'cm'),legend.direction = "vertical",legend.justification = c('left'),legend.key.size = unit(1, 'cm'), title = element_blank(),legend.text = element_text(size=12,colour = "white"),plot.title = element_text(size=15,colour = "yellow", face='bold'),panel.background = element_rect(fill = "black") , plot.background = element_rect(fill = "black"))+
  direction_label(colour = "white",text_size = 0)+
  labs(title = " Defensive actions map " )+
  annotate("text",x=-5,y=50,family = "Trebuchet MS",colour="#B00000",size=5,label = "7 Ball Recoveries")+
  annotate("text",x=-10,y=50,family = "Trebuchet MS",colour="#ef9021",size=5,label = "2 Interceptions & Blocked passes")+
  annotate("text",x=-15,y=50,family = "Trebuchet MS",colour="#25b5af",size=5,label = "(1/2) Successful Tackles")
  
  


#Heatmap for all player touches 


ggplot() +
  annotate_pitch(fill = "#333333", colour = "white") +
  geom_density_2d_filled(data = AllTouches, aes(x = x, y = y, fill = ..level..,), alpha = .4, 
                         contour_var = "ndensity" , breaks = seq(0.1, 1.0, length.out = 10)) +
  
  coord_flip()+
  scale_x_continuous(limits = c(-15, 100)) +
  scale_y_reverse(limits=c(100,-0))+
  
  
  
  direction_label(colour = "white",text_size = 0,y_label=3)+
  theme_pitch(aspect_ratio = 105/68) +   #
  theme(legend.position = "none",plot.title = element_text(size=15,colour = "yellow", face='bold'),panel.background = element_rect(fill = "black") , plot.background = element_rect(fill = "black"), title = element_text(colour = "white",size=15))+
  direction_label(colour = "white",text_size = 0)+
  labs( title = "All touches heatmap "  ) +
  annotate("text", x = -4, y = 50,vjust= 1, family = "Trebuchet MS",colour="yellow",size=5,label = "Yellow color = more touches")



install.packages("pdftools")
pdf=image_read_pdf("D:/Projects/football analysis/De Bruyne vs LIV/Rplot07.png")
png=image_convert(pdf,"PNG")
image_trim(png)
#===============================================================
#shot map using ggsoccer    ID:73084   ID:Mahrez 104749
library("patchwork")                
library(magick)
library(ggplot2)
library(ggsoccer)
library(readxl)
library(dplyr)
library("png")                      
library("gridExtra")
library(ggnewscale)
library("tidyverse")
library('cowplot')

data.source.xls <- read_excel("D:/Projects/football analysis/De Bruyne vs LIV/DE.xlsx")


Shots = data.source.xls %>% 
  filter((`type/displayName`=="SavedShot" | `type/displayName`=="MissedShots" |`type/displayName`=="Goal" )& playerId==104749 )

# Goals = data.source.xls %>%
#   filter(`type/displayName`=="Goal" & playerId==104749 )

single_team_shots <- Shots %>%
  mutate(
    goal = case_when(
      `type/displayName` == "Goal" ~ "True",
      `type/displayName` != "Goal" ~ "False",
    ))
    
    
ggplot() +
  annotate_pitch(fill = "#333333", colour = "white") +
  
  geom_point( data=single_team_shots,aes(x = x, y = y,fill=goal),
             shape = 21,
             size = 8) +
  
  # geom_point( data=Goals,aes(x = x, y = y),
  #             shape = 21,
  #             size = 8,colour="green",fill="green") +
  
  scale_fill_manual(breaks=c("True", "False"), values = c("green3", "gray15"), labels=c("Goal", "No Goal")) +
  
  scale_y_reverse()+
  
  coord_flip()+
  
  scale_x_continuous(limits =c(-15, 100) )+
  
  theme_pitch(aspect_ratio = 105/68) +   
  theme(legend.position = "bottom",plot.title = element_text(size=15,colour = "yellow", face='bold'),panel.background = element_rect(fill = "black") , plot.background = element_rect(fill = "black"))+
  direction_label(colour = "white",text_size = 0)+
  labs( title = "Shot map" ) 
  

install.packages("pdftools")
pdf=image_read_pdf("D:/Projects/football analysis/De Bruyne vs LIV/Rplot01.pdf")
png=image_convert(pdf,"PNG")
image_trim(png)
