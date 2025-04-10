#In this script we download the Statsbomb package and load in the event data from the 2015/16 Premier League.
install.packages("devtools")
devtools::install_github("statsbomb/StatsBombR")
library("StatsBombR")

#load the competition information for the desired season
Comp=FreeCompetitions()%>%filter(competition_name=="Premier League", season_name=="2015/2016")
#we load all matches from this season
Matches=FreeMatches(Comp)
#we load all events from all matches from that season
events=free_allevents(MatchesDF = Matches, Parallel = T)
#this function extracts all the information we will need from all of the events
events=allclean(events)
#we load the player minutes data using the following function
player_minutes = get.minutesplayed(events) 
#we group the player minutes by player to get each players minutes for the whole season
player_minutes = player_minutes %>%
  group_by(player.id) %>%
  summarise(minutes = sum(MinutesPlayed)) 

