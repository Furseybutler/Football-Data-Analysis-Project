#In this script we perform the passing clustering and t-SNE example.
#first load necessary packages
install.packages("BiocManager")
BiocManager::install("M3C")
install.packages("Rtsne")
library(M3C)
library(factoextra)
library(StatsBombR)
library(ggcorrplot)


#filter so its just midfielders
#get number of total passes and incompolete passes, as well as both of these seperated in to short, medium, and long passes.
pass_actions<-events%>%filter(position.id==9 | position.id==10 | position.id==11 | position.id==12 | position.id==13 | position.id==14 | position.id==15 | position.id==16 | position.id==18 | position.id==19 | position.id==20)%>%group_by(player.name, player.id)%>%summarise(total_passes=sum(type.name=="Pass"), incomplete_passes=sum(pass.outcome.name=="Incomplete" | pass.outcome.name=="Pass Offside" | pass.outcome.name=="Out", na.rm=TRUE), short_passes=sum(pass.length<10, na.rm=TRUE), short_passes_incomplete=sum(pass.length<10 & (pass.outcome.name=="Incomplete" | pass.outcome.name=="Pass Offside" | pass.outcome.name=="Out"), na.rm=TRUE), medium_passes=sum(pass.length>10 & pass.length<35, na.rm=TRUE), medium_passes_incomplete=sum(pass.length>10 & pass.length<35 & (pass.outcome.name=="Incomplete" | pass.outcome.name=="Pass Offside" | pass.outcome.name=="Out"), na.rm=TRUE), long_passes=sum(pass.length>35, na.rm=TRUE), long_passes_incomplete=sum(pass.length>35 & (pass.outcome.name=="Incomplete" | pass.outcome.name=="Pass Offside" | pass.outcome.name=="Out"), na.rm=TRUE))
#add minutes played to table
pass_actions=left_join(pass_actions, player_minutes)
#get 90s
pass_actions = pass_actions %>% mutate(nineties = minutes/90)
##transform features to be per 90 mins, and get percentages of each type of pass.
pass_actions=pass_actions%>%mutate(complete_passes_per90=(total_passes-incomplete_passes)/nineties, short_passes_per90=(short_passes-short_passes_incomplete)/nineties, medium_passes_per90=(medium_passes-medium_passes_incomplete)/nineties, long_passes_per90=(long_passes-long_passes_incomplete)/nineties, percentage_passes_short=short_passes/total_passes, percentage_passes_medium=medium_passes/total_passes, percentage_passes_long=long_passes/total_passes)
#get pass success percentages for each type of pass.
pass_actions=pass_actions%>%mutate(pass_sucess=(total_passes-incomplete_passes)/total_passes, short_pass_success=(short_passes-short_passes_incomplete)/short_passes, medium_pass_success=(medium_passes-medium_passes_incomplete)/medium_passes, long_pass_success=(long_passes-long_passes_incomplete)/long_passes)
#filter so only care about players with more than 5 matches played
pass_actions=pass_actions%>%filter(nineties>5)

#remove all features we don't want in the clustering 
pass_actions=subset(pass_actions,select=-total_passes)
pass_actions=subset(pass_actions,select=-incomplete_passes)
pass_actions=subset(pass_actions,select=-short_passes)
pass_actions=subset(pass_actions,select=-short_passes_incomplete)
pass_actions=subset(pass_actions,select=-medium_passes)
pass_actions=subset(pass_actions,select=-player.id)
pass_actions=subset(pass_actions,select=-long_passes)
pass_actions=subset(pass_actions,select=-medium_passes_incomplete)
pass_actions=subset(pass_actions,select=-long_passes_incomplete)
pass_actions=subset(pass_actions,select=-minutes)
pass_actions=subset(pass_actions,select=-nineties)
#set player name to index variable
pass_actions<- pass_actions |> column_to_rownames(var="player.name")
#filter so atleast 5 passes per game
pass_actions=pass_actions%>%filter(complete_passes_per90>5)


#get correlations
corr<-round(cor(pass_actions),1)
head(corr[ ,1:11])
#plot the correlations
ggcorrplot(corr, hc.order=TRUE, type="lower", lab=TRUE)

#create sum of least squares plot to determine optimal k.
fviz_nbclust(pass_actions, kmeans, method="wss")
#perform k-means with k=3.
km<-kmeans(pass_actions, centers = 3, nstart=25)
km
aggregate(pass_actions, by=list(cluster=km$cluster), mean)

#Create tsne plot, using Rtsne package.
tsne_result <- Rtsne(pass_actions, dims = 2, perplexity = 3, colvec = c('gold'))
plot(tsne_result$Y,asp=1, xlab="1st dimension", ylab="2nd dimension", cex.lab=1.5)
labels <- rownames(pass_actions)
#label points with corresponding player name
text(tsne_result$Y, labels = labels, pos = 4, cex = 0.7, col = "black")
