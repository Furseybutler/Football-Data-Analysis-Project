#In this script we perform the creativity clustering and t-SNE example.
#first load necessary packages
install.packages("BiocManager")
BiocManager::install("M3C")
install.packages("Rtsne")
library(M3C)
library(Rtsne)
library(factoextra)
library(ggcorrplot)

#filter so its just attacking midfielders, wingers, and forwards
#get dribbles, unsuccesful dribles, shot assists, goal assists, crosses, throughballs, passes into box, carries and miscontrols.
creative_actions<-events%>%filter(position.id==12 | position.id==16 | position.id==17 | position.id==18 | position.id==19 | position.id==20 | position.id==21)%>%group_by(player.name, player.id)%>%summarise(successful_dribbles=sum(dribble.outcome.name=="Complete", na.rm=TRUE), unsuccessful_dribbles=sum(dribble.outcome.name=="Incomplete", na.rm =TRUE), shot_assists=sum(pass.shot_assist=TRUE, na.rm=TRUE), goal_assists=sum(pass.goal_assist=TRUE, na.rm=TRUE), crosses=sum(pass.cross=TRUE, na.rm=TRUE), through_balls=sum(pass.technique.name=="Through Ball", na.rm=TRUE), passes_into_box=sum(type.name=="Pass" & pass.end_location.x>102 & pass.end_location.y>18 & pass.end_location.y<62, na.rm=TRUE), carries=sum(type.name=="Carry", na.rm=TRUE), miscontrols=sum(type.name=="Miscontrol"))
#add minutes played to table
creative_actions=left_join(creative_actions, player_minutes)
#get 90s
creative_actions = creative_actions %>% mutate(nineties = minutes/90)
#transform features to be per 90 mins, and get dribble success percentage.
creative_actions=creative_actions%>%mutate(succ_dribbles_per90=successful_dribbles/nineties, dribble_succ_percent=successful_dribbles/(successful_dribbles+unsuccessful_dribbles), shot_assists_per90=shot_assists/nineties, goal_assists_per90=goal_assists/nineties, crosses_per90=crosses/nineties, through_balls_per90=through_balls/nineties, passes_into_box_per90=passes_into_box/nineties, carries_per90=carries/nineties, miscontrols_per90=miscontrols/nineties)
#filter so we only consider players with more than 5 matches played
creative_actions=creative_actions%>%filter(nineties>5)

#remove all features we don't want in the clustering 
creative_actions=subset(creative_actions,select=-successful_dribbles)
creative_actions=subset(creative_actions,select=-unsuccessful_dribbles)
creative_actions=subset(creative_actions,select=-goal_assists)
creative_actions=subset(creative_actions,select=-shot_assists)
creative_actions=subset(creative_actions,select=-crosses)
creative_actions=subset(creative_actions,select=-carries)
creative_actions=subset(creative_actions,select=-player.id)
creative_actions=subset(creative_actions,select=-passes_into_box)
creative_actions=subset(creative_actions,select=-miscontrols)
creative_actions=subset(creative_actions,select=-through_balls)
creative_actions=subset(creative_actions,select=-minutes)
creative_actions=subset(creative_actions,select=-nineties)
#filter so atleast 0.01 dribbles per game
creative_actions=creative_actions%>%filter(succ_dribbles_per90>0.01)
#set player name to index variable
creative_actions<- creative_actions |> column_to_rownames(var="player.name")


#get correlations
corr<-round(cor(creative_actions),1)
head(corr[ ,1:9])
#plot the correlations
ggcorrplot(corr, hc.order=TRUE, type="lower", lab=TRUE)

#create sum of least squares plot to determine optimal k.
fviz_nbclust(creative_actions, kmeans, method="wss")
#perform k-means with k=3.
km<-kmeans(creative_actions, centers = 3, nstart=25)
km
aggregate(creative_actions, by=list(cluster=km$cluster), mean)

#Create tsne plot, using Rtsne package
tsne_result <- Rtsne(creative_actions, dims = 2, perplexity = 3, colvec = c('gold'))
plot(tsne_result$Y,asp=1, xlab="1st dimension", ylab="2nd dimension", cex.lab=1.5)
labels <- rownames(creative_actions)
#label points with corresponding player name
text(tsne_result$Y, labels = labels, pos = 4, cex = 0.7, col = "black")
