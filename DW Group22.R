###### WebScrapping 
rm(list=ls())

library(devtools)
devtools::install_github('charlie86/spotifyr')
library(spotifyr)
library(tidyverse)
library(knitr)

Sys.setenv(SPOTIFY_CLIENT_ID = '8d5d30535e5f4132823fffdb2fe0963e')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '87e29b8a38974f8db02373a9bbf442e1')

access_token <- get_spotify_access_token()

data<-read.csv('Spotify-2000.csv')
unique(data$Artist)

data$Artist[data$Artist=="Beyoncé"]<-"Beyonc?"
data$Artist[data$Artist=="BL?~F"]<-"BL?F"
data$Artist[data$Artist=="BL?~F"]<-"BL?F"
data$Artist[data$Artist=="Michael Bublé"]<-"Michael Bubl?"
data$Artist[data$Artist=="Herbert Grönemeyer"]<-"Herbert Gr?nemeyer"
data$Artist[data$Artist=="Rowwen Hèze"]<-"Rowwen H?ze"
data$Artist[data$Artist=="Gé Reinders"]<-"G? Reinders"
data$Artist[data$Artist=="Tiësto"]<-"Ti?sto"
data<-data[data$Artist!="Neil Young",]
data<-data[data$Artist!="De Poema's",]

unique(data$Artist)

#loop for artist ID

IDs<-character(0)

for (i in 1:150){
  features<-get_artist_audio_features(data$Artist[i])
  ID<-print(unique(features$artist_id))
  IDs<-c(IDs,ID)
}
IDs
artists<-data.frame(IDs, data$Artist)


#loop for artist popularity and spotify follower count

popularity2<-character(0)

for(i in 1:150){
  features2<-get_artists(IDs[i])
  popularity<-print(features2$popularity)
  popularity2<-c(popularity2,popularity)
}

popularity2<-data.frame(IDs,popularity2)


followers<-character(0)

for(i in 1:150){
  features3<-get_artists(IDs[i])
  follower<-print(features3$followers.total)
  followers<-c(followers,follower)
}

followers<-data.frame(IDs,followers)

#Data Integration to get the final dataset

newinfo<-merge(popularity2,followers,by="IDs",all.x=T,all.y = T)
newinfo2<-merge(artists,newinfo,by="IDs") #add artist column to merge with original dataset

final<-merge(data,newinfo2,by.x="Artist",by.y = "data.Artist",all.x=T,all.y = T)
final<-unique(final)

names(final)[names(final)=="popularity2"]<- "Artist Popularity"
names(final)[names(final)=="followers"]<- "Spotify Followers"
names(final)[names(final)=="?..Index"]<- "Index"
final$X<-NULL
final$IDs<-NULL
final<-final[,c(2,1,16,17,3:15)]

write.csv(final,"Spotify_Final_Data.csv")
######################################################## Analysis 
rm(list=ls())

final<-read.csv("Spotify_Final_Data.csv")
str(final)
#delete the extra column if its still there 
final$X<-NULL

#change Popularity to Song.Popularity
#names(final)[names(final)=='Popularity']<- 'Song.Popularity'

################# generate the artist popularity summary table 
summary(final)

library(dplyr)
Artists <- group_by(final,Artist)
summary <- summarize(Artists,
                     Songs = n(),
                     ArtistPopularity = mean(Artist.Popularity),
                     MinSongPopularity = min(Popularity),
                     MaxSongPopularity = max(Popularity),
                     AverageSongPopularity = ceiling(mean(Popularity)))
summary


################# hypothesis testings
#artists popularity with song popularity
ap<-chisq.test(table(final$Popularity,final$Artist.Popularity))
ap$p.value
cor.test(final$Popularity,final$Artist.Popularity)

#energy with song popularity
e<-chisq.test(table(final$Popularity,final$Energy))
e$p.value
cor.test(final$Popularity,final$Energy)

#BPM with song popularity 
bpm<-chisq.test(table(final$Popularity,final$Beats.Per.Minute..BPM.))
bpm$p.value
cor.test(final$Popularity,final$Beats.Per.Minute..BPM.)

################## model fitting
#summary tables 
### artist
final$ArtistEvaluation<-ifelse(final$Artist.Popularity<=50,'Not popular','Popular')
library(dplyr)
artist_summary<-summarize(group_by(final,ArtistEvaluation),
                          total_songs = n(),
                          Min.Pop = min(Popularity),
                          Avg.Pop = mean(Popularity),
                          Max.Pop = max(Popularity))
artist_summary

### valence
final$ValenceEvaluation<-ifelse(final$Valence<=50,'Not positive','Positive')
valence_summary<-summarize(group_by(final,ValenceEvaluation),
                           total_songs = n(),
                           Min.Pop = min(Popularity),
                           Avg.Pop = mean(Popularity),
                           Max.Pop = max(Popularity))
valence_summary

### length
#create a summary table for length 
library(qwraps2)

length_summary<-
  list("Song Length" =
         list("min" = ~ min(final$Length..Duration.),
              "max" = ~ max(final$Length..Duration.),
              "mean(sd)" = ~ qwraps2::mean_sd(final$Length..Duration.)))

length_summary1 <-summary_table(final,length_summary)
length_summary

### linear regression 
song_fit<-lm(Popularity~Artist.Popularity+Valence+Length..Duration.,data=final)
summary(song_fit)

#linearity
plot(final$Popularity,final$Valence) #no linear relationship
plot(final$Popularity,final$Artist.Popularity) # positive linear relationship
plot(final$Popularity,final$Length..Duration.) #negative linear relationship

#normality
qqnorm(song_fit$residuals)
qqline(song_fit$residuals, col="red")
#check the normality
shapiro.test(song_fit$residuals) 
#H0:it is normal 
#reject H0, cuz p-value is smaller than 0.05

#homoscedasticity
plot(song_fit$fitted.values,song_fit$residuals)
#everything is good and there is no systematical trend 

#multicollinearity
library(car)
song_fit<-lm(Popularity~Artist.Popularity+Valence+Length..Duration.,data=final)
vif(song_fit)
#vif<5, so no issue of multicollinearity

### visualizations 
#Relationship between danceability, acousticness, and song popularity
library(ggplot2)

ggplot(final,aes(x=Danceability, y=Popularity))+
  geom_point()+geom_smooth(method=lm, se=FALSE)
#weak, positive relationship
cor.test(final$Danceability,final$Popularity)

ggplot(final,aes(x=Acousticness, y=Popularity))+
  geom_point()+geom_smooth(method=lm, se=FALSE)
#weak, negative relationship
cor.test(final$Acousticness,final$Popularity)

#What genres are most popular?

#Top 15 Genres by Popularity

genre_grouped<-group_by(final, Top.Genre)

genre<-summarize(genre_grouped,Popularity=round(mean(Popularity)),
                 Danceability=round(mean(Danceability)),
                 Acousticness=round(mean(Acousticness)))

genre<-genre[order(genre$Popularity, decreasing = T),]
genre<-genre[1:15,]
colnames(genre)<-c("Genre","Average.Popularity","Average.Danceability","Average.Acousticness")
genre

ggplot(genre,aes(x=Genre, y=Average.Popularity,fill=Genre))+geom_bar(stat="identity")+geom_text(aes(label=Average.Popularity),vjust=-0.4,
                                                                                                size= 3)+ theme(axis.text.x = element_blank(),legend.title = element_blank())+ggtitle("15 Most Popular Genres")

















