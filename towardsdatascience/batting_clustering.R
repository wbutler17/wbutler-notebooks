library(readr)
Batting <- read_csv("C:/Users/wbutl/Dropbox/baseballdatabank-master/core/Batting.csv")
#only look at seasons 1969 and later (post-expansion)
Batting_mod <- Batting[Batting$yearID >= 1969,1:22]

#only include players with at least 300 at bats/season
Batting_mod <- Batting_mod[Batting_mod$AB >= 300,1:22]
Batting_mod$IBB <- as.numeric(Batting_mod$IBB)
Batting_mod$HBP <- as.numeric(Batting_mod$HBP)
Batting_mod$SH <- as.numeric(Batting_mod$SH)
Batting_mod$SF <- as.numeric(Batting_mod$SF)

library(mclust)

#aggregate data by player
mod_agg = aggregate(Batting_mod,by=list(Batting_mod$playerID), FUN=median, na.rm= TRUE)
g_agg = aggregate(Batting_mod$G,by=list(Batting_mod$playerID), FUN=sum, na.rm = TRUE)
mod_agg$G = g_agg$x

#only keep players with >= 600 games (~5 reasonably full seasons)
mod_agg2 = mod_agg[mod_agg$G >= 600,1:23]

# add in number of singles, OBP, and SLG
mod_agg2$singles = mod_agg2$H - mod_agg2$'2B' - mod_agg2$'3B' - mod_agg2$HR
mod_agg2$OBP <- (mod_agg2$H + mod_agg2$BB)/mod_agg2$AB
mod_agg2$SLG <- (mod_agg2$singles + mod_agg2$'2B'*2 + 3*mod_agg2$'3B'+4*mod_agg2$'HR')/mod_agg2$AB
mod_agg2$SO <- -mod_agg2$SO



# add in Fielding stats
Fielding <- read_csv("C:/Users/wbutl/Dropbox/baseballdatabank-master/core/Fielding.csv")
Fielding <- Fielding[Fielding$yearID>=1969,]
Fielding <- aggregate(Fielding[,7:13], by=list(Fielding$playerID,Fielding$yearID), FUN = sum)
n_players <- nrow(mod_agg2)
for (i in c(1:n_players)){
  player_sub <- Fielding[Fielding$Group.1==mod_agg2$Group.1[i],]
  player_sub <- t(apply(player_sub[,3:9], 2, median))
  mod_agg2$InnOuts[i] <- player_sub[,3]
  mod_agg2$PO[i] <- player_sub[,4]
  mod_agg2$A[i] <- player_sub[,5]
  mod_agg2$E[i] <- player_sub[,6]
  mod_agg2$DP[i] <- player_sub[,7]
}
#ensure InnOuts has no NAs
mod_agg2$InnOuts[is.na(mod_agg2$InnOuts)] <- median(mod_agg2$InnOuts,na.rm=TRUE)

# label player positions
Fielding <- read_csv("C:/Users/wbutl/Dropbox/baseballdatabank-master/core/Fielding.csv")
for (i in c(1:nrow(mod_agg2))){
  sub = Fielding[mod_agg2$Group.1[i] == Fielding$playerID,1:18]
  sub_agg <- aggregate(sub,by=list(sub$POS),FUN=mean,na.rm=TRUE)
  mod_agg2$pos[i] <- sub_agg$Group.1[which.max(sub_agg$G)] 
}

#run PCA on all numeric stats, except RBIs
X <- mod_agg2[,c(7:13,15:26,28:31)]
batters.pca <- prcomp(X, center = TRUE, scale = TRUE)

# fit gmm to PCs
mod1 = Mclust(batters.pca$x[,1:6], G=4:7) #uses PCs with eigenvalue > 1
plot(mod1,what = "classification")
summary(mod1)

table(mod_agg2$pos,mod1$classification)


class <- mod1$classification
unc <- mod1$uncertainty
mix <- mod1$z

# bar plot of PC parameters for each cluster
model_mean <- mod1$parameters$mean
frame()
pca_space <- batters.pca$x[,1:6]
par(mfrow = c(2,4))
for (i in c(1:6)){
  barplot(c(model_mean[,i]-colMeans(pca_space))/c(apply(pca_space,2,sd)),ylim=c(-2,2))
}


# cluster ids, and mix amounts for example players
hank <- which(mod_agg2$Group.1=='aaronha01')
alomar <- which(mod_agg2$Group.1=='alomaro01')
bonds <- which(mod_agg2$Group.1=='bondsba01')
carew <- which(mod_agg2$Group.1=='carewro01')
mauer <- which(mod_agg2$Group.1=='mauerjo01')
pujols <- which(mod_agg2$Group.1=='pujolal01')
arod <- which(mod_agg2$Group.1=='rodrial01')
pete <- which(mod_agg2$Group.1=='rosepe01')
trout <- which(mod_agg2$Group.1=='troutmi01')
players <- c(hank,alomar,bonds,carew,mauer,pujols,arod,pete,trout)
#Aaron, Bonds, Carew, Griffey, Mauer, Pujols, A-Rod, Rose,Trout
frame()
par(mfrow = c(3,3))
for (i in c(1:9)){
  player = players[i]
  barplot(t(c((pca_space[player,]-colMeans(pca_space))/c(apply(pca_space,2,sd)))),ylim=c(-3,3), ylab = 'worse--average--better')
  title(paste(mod_agg2$Group.1[player],' cluster',class[player]))
}
par(mfrow = c(3,3))
for (i in c(1:9)){
  player = players[i]
  barplot(mix[player,], ylim=c(0,1))
  title(paste(mod_agg2$Group.1[player],' cluster',class[player]))
}

# odd ones
brook <- which(mod_agg2$Group.1=='robinbr01') #class 1
mccov <- which(mod_agg2$Group.1=='mccovwi01') #class 2
youk <- which(mod_agg2$Group.1=='youklke01') #class 3
bret <- which(mod_agg2$Group.1=='boonebr01') #class 4
pete <- pete
bats <- which(mod_agg2$Group.1=='bautijo02') #class 6
odds <- c(brook,mccov, youk, bret, pete, bats)
par(mfrow = c(2,3))
for (i in c(1:6)){
  player = odds[i]
  barplot(mix[player,], ylim=c(0,1))
  title(paste(mod_agg2$Group.1[player],' cluster',class[player]))
}

# measures for each cluster on normal stats
pos <- aggregate(X, by = list(class), FUN = mean)
pos2 <- round(pos)
pos2$OBP <- round(pos$OBP*1000)/1000
pos2$SLG <- round(pos$SLG*1000)/1000
pos2$SO <- -pos2$SO

# dendrogram labeled with player names
# plot(hclust(dist(X)),labels = mod_agg2$playerID,hang= -1)
