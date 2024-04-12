library(readr)
pbp_2018 <- read_csv("Datafun/football/pbp-2018.csv")

par(mfrow=c(1,4))
hist(pbp_2018$Yards[grepl("DIGGS", pbp_2018$Description)],30)
hist(pbp_2018$Yards[grepl("THIELEN", pbp_2018$Description)],30)
hist(pbp_2018$Yards[grepl("K.RUDOLPH", pbp_2018$Description)],30)
hist(pbp_2018$Yards[grepl("D.COOK", pbp_2018$Description)],30)

# determine passer and receiver for each pass play
pass <- pbp_2018[pbp_2018$IsPass==1,]

for (i in c(1:nrow(pass))){
  pass_split <- strsplit(pass$Description[i],c("-",pass$PassType[i]))
  passer <- strsplit(pass_split[[1]][2]," PASS")[[1]][1]
  target <- strsplit(pass_split[[1]][3],c(" "))[[1]][1]
  pass$passer[i] <- passer
  pass$target[i] <- gsub("\\.$|]$","",target) # removes trailing "." or "]"
}

# median and total receiving yards/Viking receiver/pass type
vik_rec_names <- as.data.frame(table(pass$target[pass$OffenseTeam=="MIN"]))
main_recs <- as.character(vik_rec_names$Var1[vik_rec_names$Freq>=10])

vik_rec_yards <- aggregate(pass$Yards[pass$target %in% main_recs & pass$OffenseTeam=="MIN"],by=list(pass$target[pass$target %in% main_recs & pass$OffenseTeam=="MIN"],pass$PassType[pass$target %in% main_recs & pass$OffenseTeam=="MIN"]),FUN=median)
vik_rec_yards_sum <- aggregate(pass$Yards[pass$target %in% main_recs & pass$OffenseTeam=="MIN"],by=list(pass$target[pass$target %in% main_recs & pass$OffenseTeam=="MIN"],pass$PassType[pass$target %in% main_recs & pass$OffenseTeam=="MIN"]),FUN=sum)
vik_rec_yards$sum <- vik_rec_yards_sum$x

vik_wide <- reshape(vik_rec_yards_sum,timevar = "Group.2", idvar  = "Group.1", direction = "wide")
vik_wide <- vik_wide[order(rowSums(vik_wide[,2:7],na.rm=TRUE)),]
image(t(as.matrix(vik_wide[,2:7])),col = parula(20),ylim=c(1.1,-0.1))


# predict plays based on game conditions
library(randomForest)
library(class)
require(caTools)


team_to_use <- "KC"
# first, figure out game score at each play
all_vike <- pbp_2018[grepl(team_to_use, pbp_2018$OffenseTeam)|grepl(team_to_use,pbp_2018$DefenseTeam),]
all_vike <- all_vike[order(all_vike$GameDate),]
all_vike$score_differential <- 0
all_vike$time <- (4-all_vike$Quarter)*15 + all_vike$Minute + all_vike$Second/60

date_set <- unique(all_vike$GameDate)
for (current_date in date_set){
  current_game <- all_vike[all_vike$GameDate==current_date,]
  current_game <- current_game[order(current_game$time, decreasing = TRUE),]
  current_differential <- 0
  for (play_num in c(1:nrow(current_game))){
    current_play <- current_game[play_num,]
    if (grepl("FIELD GOAL IS GOOD",current_play$Description)){
      current_differential <- current_differential + ifelse(current_play$OffenseTeam==team_to_use,3,-3)
    } 
    if (grepl("TOUCHDOWN",current_play$Description)){
      if (grepl("TOUCHDOWN NULLIFIED",current_play$Description)|grepl("REVERSED",current_play$Description)){
        current_differential <- current_differential
      } else {
        if (current_play$IsInterception==1){
          current_differential <- current_differential + ifelse(current_play$OffenseTeam==team_to_use,-6,6)
        } 
        if (grepl("RECOVERED BY",current_play$Description)){
          if (grepl(paste("RECOVERED BY ",team_to_use),current_play$Description)){
            current_differential <- current_differential + 6
          } else {
            current_differential <- current_differential - 6
          }
        } else {
          current_differential <- current_differential + ifelse(current_play$OffenseTeam==team_to_use,6,-6)
        } 
      }
    }
    if (grepl("EXTRA POINT IS GOOD",current_game$Description[play_num])){
      current_differential <- current_differential + ifelse(current_game$OffenseTeam[play_num]==team_to_use,1,-1)
    }
    if (grepl("ATTEMPT SUCCEEDS",current_game$Description[play_num])){
      current_differential <- current_differential + ifelse(current_game$OffenseTeam[play_num]==team_to_use,2,-2)
    }
    current_game$score_differential[play_num] <- current_differential
  }
  
  all_vike$score_differential[all_vike$GameDate==current_date] <- current_game$score_differential
}

vike <- all_vike[grepl(team_to_use, all_vike$OffenseTeam),]
# remove "no down" plays
vike <- vike[vike$Down>0,]


# logistic regression

current_splitr <- 0.8
accuracies_logistic <- matrix(nrow=100)
for (splits in c(1:100)){
  train_ind <- sample.split(vike$IsPass,SplitRatio = current_splitr)
  test_ind <- !train_ind
  logit_fit <- glm(IsPass ~ time+score_differential+as.factor(DefenseTeam)+Down+ToGo+YardLine, data=vike[train_ind,], family="binomial")
  p <- predict(logit_fit,vike[test_ind,],family="binomial")
  probs <- exp(p)/(1+exp(p))
  test_outcomes <- probs>0.5
  cm <- table(Actual = vike$IsPass[test_ind],Predicted = test_outcomes)
  accuracy <- sum(diag(cm))/sum(test_ind)
  accuracies_logistic[splits] <- accuracy
}
c(mean(accuracies_logistic),sd(accuracies_logistic))

rf <- randomForest(as.factor(PlayType) ~ time+score_differential+as.factor(vike$DefenseTeam)+Down+ToGo+YardLine, data=vike)

# dotplot of penalties/type/team
ggplot(off2, aes(x=(penalty_type),y=num_penalties,fill=factor(penalty_type))) +
  + geom_dotplot(binaxis="y",binwidth = 3.5,stackdir = "center")
