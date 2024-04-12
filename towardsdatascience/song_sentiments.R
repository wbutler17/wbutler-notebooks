# sentiment analysis of all songs in 'billboard_lyrics...' file
install.packages("syuzhet")
library(syuzhet)

songs <- billboard_lyrics_1964_2015

# remove 'featuring' artist
for (i in c(1:5100)){
  artist <- songs$Artist[i]
  split <- strsplit(artist, "featuring")
  songs$single_artist[i] <- split[[1]][1]
}

# remove instrumentals
songs <- songs[-c(1051,1085,1110,1129,1143,1197,1271,1449,1480,1554),]

# scrape Lyrics from genius.com to run sentiment analysis line by line
install.packages(c("devtools","dplyr","geniusR","tidyverse","RCurl"))
library(devtools)
library(dplyr)
library(geniusR)
library(tidyverse)
library(RCurl)
devtools::install_github("josiahparry/geniusR")


songs$mean_sent_line <- 0
songs$sd_sent_line <- 0
songs$median_sent_line <- 0
songs$num_lines <- 0
songs$num_neg_lines <- 0
songs$num_pos_lines <- 0
for (i in c(1:5100)){
  current_artist <- songs$single_artist[i]
  current_song <- songs$Song[i]
  if (url.exists(geniusR::gen_song_url(artist = current_artist, song = current_song))){
  l <- geniusR::genius_lyrics(artist=current_artist,song=current_song)
  l$sent <- get_sentiment(l$lyric)
  songs$mean_sent_line[i] <- mean(l$sent)
  songs$sd_sent_line[i] <- sd(l$sent)
  songs$median_sent_line[i] <- median(l$sent)
  songs$num_lines[i] <- nrow(l)
  songs$num_pos_lines[i] <- sum(l$sent>0)
  songs$num_neg_lines[i] <- sum(l$sent<0)
  }
}

# new dataframe only incorporating songs that had lyrics
songs2 <- songs[songs$num_lines > 5,]

# aggregate by year
songs_agg <- aggregate(songs2, by=list(songs2$Year), FUN=median)
songs_agg_sd <- aggregate(songs2, by=list(songs2$Year), FUN=sd)

# these plots together
par(mfrow=c(1,3))
# plot median song sentiment by year + best fit line
l<-lm(songs_agg$mean_sent_line~songs_agg$Group.1)
plot(songs_agg$Group.1,songs_agg$mean_sent_line,ylab = 'median sentiment',xlab='year', ylim = c(0,0.25), main = 'Sentiment by year')
lines(songs_agg$Group.1,l$fitted.values,col='red')

#histograms of number of overall "negative" and "positive" songs
hist(songs2$Year[songs2$mean_sent_line<(0)], xlab = 'year', ylab = 'count', main = 'Number of negative songs \n(mean sentiment < 0)', col = 'blue')
hist(songs2$Year[songs2$mean_sent_line>0.25], xlab = 'year', ylab = 'count', main = 'Number of positive songs \n(mean sentiment > 0.25)', col = 'red')

# followed by figure with example lyrics from 4 pos and 4 neg songs


# use Spotify for additional data about each song
devtools::install_github('charlie86/spotifyr')
install.packages('spotifyr')
Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxx')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxx')

access_token <- get_spotify_access_token()

# get features of each track
for (i in c(1:nrow(songs2))){
  track <- get_tracks(songs2$Song[i],songs2$single_artist[i],return_closest_track = TRUE)
  if (nrow(track)>0){
    track <- track[1,]

  track_analysis <- get_track_audio_features(track)
  songs2$dance[i] <- track_analysis$danceability
  songs2$energy[i] <- track_analysis$energy
  songs2$val[i] <- track_analysis$valence
  songs2$time[i] <- track_analysis$duration_ms / 1000 #convert to s
  songs2$mode[i] <- track_analysis$mode
  songs2$tempo[i] <- track_analysis$tempo
  }
}

# get word count
songs2$word_count <- 0
for (i in c(1:nrow(songs2))){
  current_artist <- songs2$single_artist[i]
  current_song <- songs2$Song[i]
  if (url.exists(geniusR::gen_song_url(artist = current_artist, song = current_song))){
    l <- geniusR::genius_lyrics(artist=current_artist,song=current_song)
    l$count <- lengths(strsplit(l$lyric, " "))
    songs2$word_count[i] <- sum(l$count)
  }
}
songs2$wpm <- songs2$word_count/songs2$time*60 #words per minute

# wordiness
plot(songs2$Year, songs2$wpm, col = 'grey',xlab = 'year', ylab = 'words per minute', main = 'Wordiness increases over time')
lines(songs_agg$Group.1,songs_agg$wpm, col = 'red', lwd = 2)
text(1965.5,197,"I Like it Like That", adj = 0, cex = 0.7)
text(1974.5,0,"Tubular Bells", adj = 0, cex = 0.7)
text(1994.5,276, "Bop Gun (One Nation)", adj = 0, cex = 0.7)
text(1967.5,2,"Soul Finger", adj = 0, cex = 0.7)
text(1982.5,5,"Theme from Chariots of Fire", adj = 0, cex = 0.7)
text(2007.5,260,"Walk it Out", adj = 0, cex = 0.7)
text(2000.5,255,"Bounce with Me", adj = 0, cex = 0.7)

par(mfrow=c(5,1))
hist(songs2$wpm[songs2$Year<=1975],c(0:60)*5, col = 'grey',xlim=c(0,250),main = "1965 to 1975", xlab = 'words per minute', freq = FALSE)
hist(songs2$wpm[songs2$Year>1975 & songs2$Year<=1985],c(0:60)*5, col = 'grey',xlim=c(0,250),main = "1975 to 1985", xlab = 'words per minute', freq = FALSE)
hist(songs2$wpm[songs2$Year>1985 & songs2$Year<=1995],c(0:60)*5, col = 'grey',xlim=c(0,250),main = "1985 to 1995", xlab = 'words per minute', freq = FALSE)
hist(songs2$wpm[songs2$Year>1995 & songs2$Year<=2005],c(0:60)*5, col = 'grey',xlim=c(0,250),main = "1995 to 2005", xlab = 'words per minute', freq = FALSE)
hist(songs2$wpm[songs2$Year>2005 & songs2$Year<=2015],c(0:60)*5, col = 'grey',xlim=c(0,250),main = "2005 to 2015", xlab = 'words per minute', freq = FALSE)


# examine how tempo changes over time
plot(songs2$Year, songs2$tempo, col='grey', xlab = 'year', ylab = 'tempo (BPM)')
lines(songs_agg$Group.1,songs_agg$tempo,col='red',lwd=2)
lines(songs_agg$Group.1,songs_agg$tempo+songs_agg_sd$tempo,col='red',lwd=1)
lines(songs_agg$Group.1,songs_agg$tempo-songs_agg_sd$tempo,col='red',lwd=1)

par(mfrow=c(1,5))
hist(songs2$tempo[songs2$Year<=1975],50, col = 'grey',xlim=c(50,250),main = "1965 to 1975", xlab = 'tempo (BPM)')
hist(songs2$tempo[songs2$Year>1975 & songs2$Year<=1985],50, col = 'grey',xlim=c(50,250),main = "1975 to 1985", xlab = 'tempo (BPM)')
hist(songs2$tempo[songs2$Year>1985 & songs2$Year<=1995],50, col = 'grey',xlim=c(50,250),main = "1985 to 1995", xlab = 'tempo (BPM)')
hist(songs2$tempo[songs2$Year>1995 & songs2$Year<=2005],50, col = 'grey',xlim=c(50,250),main = "1995 to 2005", xlab = 'tempo (BPM)')
hist(songs2$tempo[songs2$Year>2005 & songs2$Year<=2015],50, col = 'grey',xlim=c(50,250),main = "2005 to 2015", xlab = 'tempo (BPM)')

# homogenization of pop music?
# variance in billboard song length, sentiment, and energy have significantly decreased
par(mfrow=c(3,2))
plot(songs2$Year, songs2$time, xlab = 'year', ylab = 'duration (s)')
plot(songs_agg_sd$Group.1,songs_agg_sd$time, xlab = 'year', ylab = 'std. dev. duration')
lines(songs_agg_sd$Group.1,songs_agg_sd$time)
plot(songs2$Year, songs2$mean_sent_line, xlab = 'year', ylab = 'mean sentiment')
plot(songs_agg_sd$Group.1,songs_agg_sd$mean_sent_line, xlab = 'year', ylab = 'std. dev. mean sentiment')
lines(songs_agg_sd$Group.1,songs_agg_sd$mean_sent_line)
plot(songs2$Year, songs2$energy, xlab = 'year', ylab = 'energy')
plot(songs_agg_sd$Group.1,songs_agg_sd$energy, xlab = 'year', ylab = 'std. dev. energy')
lines(songs_agg_sd$Group.1,songs_agg_sd$energy)

# using k-means to cluster across most variables
kss <- c(1:30)
for (i in c(3:34)){
  tempo_k <- kmeans(scale(songs2[,c(7,8,10,14,15,16,17,19,21)]),i)
  sil <- silhouette(tempo_k$cluster,dist(scale(songs2[,c(7,8,10,14,15,16,17,19,21)])))
  sil_sum <- summary(sil)
  kss[i-2] <- sil_sum$avg.width
}
plot(kss)

# 6 clusters appears to be optimal
tempo_k <- kmeans(scale(songs2[,c(7,8,10,14,15,16,17,19,21)]),6)
plot(songs2$Year,songs2$tempo,col=tempo_k$cluster)

# examine clusters
clus_1 <- songs2[tempo_k$cluster==1,]
####################################################
#save(songs2,file = 'C:/Users/wbutler/Dropbox/Datafun/songs2')
