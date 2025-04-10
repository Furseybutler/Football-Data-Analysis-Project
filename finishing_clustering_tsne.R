#In this script we perform the finishing clustering and t-SNE example.
#first load necessary packages
install.packages("BiocManager")
BiocManager::install("M3C")
install.packages("Rtsne")
library(M3C)
library(factoextra)
library(StatsBombR)


#filter so its just attackers
#get shots, shots on target and goals for attackers.
att_actions<-events%>%filter(position.id==17 | position.id==21 | position.id==22 | position.id==23 | position.id==24 | position.id==25)%>%group_by(player.name, player.id)%>%summarise(shots=sum(type.name=="Shot", na.rm=TRUE), SoT=sum(shot.outcome.name=="Goal",shot.outcome.name=="Saved", shot.outcome.name=="Saved To Post", na.rm=TRUE), goals=sum(shot.outcome.name=="Goal", na.rm=TRUE))
#add minutes played to table
att_actions=left_join(att_actions, player_minutes)
#get 90s
att_actions = att_actions %>% mutate(nineties = minutes/90)
#transform features to be per 90 mins, and get shot accuracy and shot success.
att_actions=att_actions%>%mutate(shot_accuracy=SoT*100/shots, shot_success=goals*100/shots, shots_per90=shots/nineties, SoT_per90=SoT/nineties, goals_per90=goals/nineties)
#omit strikers with 0 shots
att_actions<-na.omit(att_actions)
#filter so only care about players with more than 5 matches played
att_actions=att_actions%>%filter(nineties>5)

#remove minutes, 90s and non per 90 stats 
att_actions=subset(att_actions,select=-shots)
att_actions=subset(att_actions,select=-SoT)
att_actions=subset(att_actions,select=-goals)
att_actions=subset(att_actions,select=-minutes)
att_actions=subset(att_actions,select=-nineties)
att_actions=subset(att_actions,select=-player.id)
#set player name to index variable
att_actions<- att_actions |> column_to_rownames(var="player.name")

#get correlations
corr<-round(cor(att_actions),1)
head(corr[, 1:5])
#plot the correlations
ggcorrplot(corr, hc.order=TRUE, type="lower", lab=TRUE)

#create sum of least squares plot to determine optimal k.
fviz_nbclust(att_actions, kmeans, method="wss")
#perform k-means with k=4.
km<-kmeans(att_actions, centers = 4, nstart=25)
km
aggregate(att_actions, by=list(cluster=km$cluster), mean)

#Create tsne plot, using Rtsne package.
tsne_result <- Rtsne(att_actions, dims = 2, perplexity = 3, colvec = c('gold'))
plot(tsne_result$Y,asp=1, xlab="1st dimension", ylab="2nd dimension", cex.lab=1.5)
labels <- rownames(att_actions)
#label points with corresponding player name
text(tsne_result$Y, labels = labels, pos = 4, cex = 0.7, col = "black")

