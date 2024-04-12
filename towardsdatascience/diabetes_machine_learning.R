library(readr)
diabetes <- read_csv("C:/Users/wbutler/Desktop/diabetes.csv")
View(diabetes)

library(class)
require(caTools)
library(ggplot2)

######################################################################
# KNN classifier, repeated 100 times for different training splits
all_test_accuracies_knn <- matrix(nrow=100,ncol=9)
for (split_number in c(1:100)){
  train_ind <- sample.split(diabetes$Pregnancies,SplitRatio = 0.8)
  test_ind <- !train_ind
  
  neighbors <- c(2:10)
  accuracies <- matrix(nrow=1, ncol=9)
  
  for (n_neighbors in neighbors){
    knn_fit <- knn(diabetes[train_ind,],diabetes[test_ind,],diabetes$Outcome[train_ind],k=n_neighbors)
    cm <- table(Actual = diabetes$Outcome[test_ind],Predicted = knn_fit)
    accuracy <- sum(diag(cm))/sum(test_ind)
    accuracies[n_neighbors-1] <- accuracy
  }
  all_test_accuracies_knn[split_number,] <- accuracies
}

q <- data.frame(
  values = t(all_test_accuracies_knn)
)
q$mean <- apply(q,1,function(row) mean(row[-1]))
q$sd <- apply(q,1,function(row) sd(row[-1]))
eb <- aes(ymax = mean+sd, ymin = mean-sd)
ggplot(data=q,aes(x=neighbors,y=mean)) +
  geom_line(size = 2) +
  geom_ribbon(eb,alpha=0.5)

##############################################################
# logistic regression
all_test_accuracies_logistic <- matrix(nrow=100,ncol=1)
for (split_number in c(1:100)){
  train_ind <- sample.split(diabetes$Pregnancies,SplitRatio = 0.8)
  test_ind <- !train_ind
  
  logit_fit <- glm(Outcome ~ ., data=diabetes[train_ind,], family="binomial")
  p <- predict(logit_fit,diabetes[test_ind,],family="binomial")
  probs <- exp(p)/(1+exp(p))
  test_outcomes <- probs>0.5
  cm <- table(Actual = diabetes$Outcome[test_ind],Predicted = test_outcomes)
  accuracy <- sum(diag(cm))/sum(test_ind)
  all_test_accuracies_logistic[split_number] <- accuracy
}

q_log <- as.data.frame(all_test_accuracies_logistic)
ggplot(data=q_log,aes(x=V1)) +
  geom_histogram(binwidth=0.01) +
  xlab("accuracy")

#############################################################################
# decision tree classifier
all_test_accuracies_dtree <- matrix(nrow=100,ncol=10)
buckets <- seq(2,20,2)
for (split_number in c(1:100)){
  for (bucketsize in buckets){
  train_ind <- sample.split(diabetes$Pregnancies,SplitRatio = 0.8)
  test_ind <- !train_ind
  tree <- rpart(as.factor(Outcome) ~ ., data = diabetes[train_ind,],minbucket=bucketsize, model=TRUE)
  cm <- table(predict(tree,diabetes[test_ind,])[,2]>0.5,diabetes$Outcome[test_ind])
  accuracy <- sum(diag(cm))/sum(test_ind)
  all_test_accuracies_dtree[split_number,bucketsize/2] <- accuracy
  }
}

q_tree <- data.frame(
  values = t(all_test_accuracies_dtree)
)
q_tree$mean <- apply(q_tree,1,function(row) mean(row[-1]))
q_tree$sd <- apply(q_tree,1,function(row) sd(row[-1]))
eb <- aes(ymax = mean+sd, ymin = mean-sd)
ggplot(data=q_tree,aes(x=buckets,y=mean)) +
  geom_line(size = 2) +
  geom_ribbon(eb,alpha=0.5) +
  ylim(0.5,0.9)

# importance of each measure (on average, bucketsize = 10)
all_dtree_importance <- matrix(nrow=8,ncol=100) # Glucose, BMI, Age, Insulin, DiabPed, Preg, BP, Skin
bucketsize <- 10
for (split_number in c(1:100)){
    train_ind <- sample.split(diabetes$Pregnancies,SplitRatio = 0.8)
    test_ind <- !train_ind
    tree <- rpart(as.factor(Outcome) ~ ., data = diabetes[train_ind,],minbucket=bucketsize, model=TRUE)

    importance <- t(tree$variable.importance)
    importance <- importance/sum(importance)
    all_dtree_importance[1,split_number] <- importance[,"Glucose"]
    all_dtree_importance[2,split_number] <- importance[,"BMI"]
    all_dtree_importance[3,split_number] <- importance[,"Age"]
    all_dtree_importance[4,split_number] <- importance[,"Insulin"]
    all_dtree_importance[5,split_number] <- importance[,"DiabetesPedigreeFunction"]
    all_dtree_importance[6,split_number] <- importance[,"Pregnancies"]
    all_dtree_importance[7,split_number] <- importance[,"BloodPressure"]
    all_dtree_importance[8,split_number] <- importance[,"SkinThickness"]
}

q_tree_importance <- data.frame(
  values = (all_dtree_importance)
)
q_tree_importance$mean <- apply(q_tree_importance,1,function(row) mean(row[-1]))
q_tree_importance$sd <- apply(q_tree_importance,1,function(row) sd(row[-1]))
eb <- aes(ymax = 100*(mean+sd), ymin = 100*(mean-sd))
ggplot(data=q_tree_importance,aes(x=c(8:1),y=mean*100)) +
  geom_bar(stat="identity") +
  geom_errorbar(eb,width=0.5) +
  scale_x_continuous("factor",breaks=c(8:1),labels=c("Glucose","BMI","Age","Insulin","Pedigree","Pregnancies","BP","SkinThick")) +
  labs(y="mean importance (%)") +
  coord_flip()


#######################################################
# random forest
library(randomForest)

all_train_accuracies_rf <- matrix(nrow=100,ncol=1)
all_test_accuracies_rf <- matrix(nrow=100,ncol=1)
all_importances_rf <- matrix(nrow=100,ncol=8)
for (split_number in c(1:100)){
  train_ind <- sample.split(diabetes$Pregnancies,SplitRatio = 0.8)
  test_ind <- !train_ind
  rf <- randomForest(as.factor(Outcome) ~ ., data = diabetes[train_ind,],ntree=100)
  train_accuracy <- sum(diag(rf$confusion))/sum(train_ind)
  cm <- table(predict(rf,diabetes[test_ind,]),diabetes$Outcome[test_ind])
  test_accuracy <- sum(diag(cm))/sum(test_ind)
  
  all_train_accuracies_rf[split_number] <- train_accuracy
  all_test_accuracies_rf[split_number] <- test_accuracy
  
  importance <- rf$importance/sum(rf$importance)
  all_importances_rf[split_number,1] <- importance["Glucose",]
  all_importances_rf[split_number,2] <- importance["BMI",]
  all_importances_rf[split_number,3] <- importance["Age",]
  all_importances_rf[split_number,4] <- importance["Insulin",]
  all_importances_rf[split_number,5] <- importance["DiabetesPedigreeFunction",]
  all_importances_rf[split_number,6] <- importance["Pregnancies",]
  all_importances_rf[split_number,7] <- importance["BloodPressure",]
  all_importances_rf[split_number,8] <- importance["SkinThickness",]
}

q_rf_importance <- data.frame(
  values = t(all_importances_rf)
)
q_rf_importance$mean <- apply(q_rf_importance,1,function(row) mean(row[-1]))
q_rf_importance$sd <- apply(q_rf_importance,1,function(row) sd(row[-1]))
eb <- aes(ymax = 100*(mean+sd), ymin = 100*(mean-sd))
ggplot(data=q_rf_importance,aes(x=c(8:1),y=100*mean)) +
  geom_bar(stat="identity") +
  geom_errorbar(eb,width=0.5) +
  scale_x_continuous("factor",breaks=c(8:1),labels=c("Glucose","BMI","Age","Insulin","Pedigree","Pregnancies","BP","SkinThick")) +
  labs(y="mean importance (%)") +
  coord_flip()


#############################################
# gradient boosting
install.packages("gbm")
library(gbm)

all_gb_accuracies <- matrix(nrow=100)
all_gb_relative_inf <- matrix(nrow=100,ncol=8)
for (split_number in c(1:100)){
  train_ind <- sample.split(diabetes$Pregnancies,SplitRatio = 0.8)
  test_ind <- !train_ind
  gb <- gbm(Outcome ~ ., data = diabetes[train_ind,], distribution = "bernoulli")
  vals <- predict.gbm(gb, diabetes[test_ind,],n.trees=100)
  probs <- exp(vals)/(1+exp(vals))
  class1 <- probs>0.5
  cm <- table(class1,diabetes$Outcome[test_ind])
  gb_accuracy <- sum(diag(cm))/sum(test_ind)
  all_gb_accuracies[split_number] <- gb_accuracy
  
  s <- summary.gbm(gb,plotit = FALSE)
  all_gb_relative_inf[split_number,1] <- s$rel.inf[s$var=="Glucose"]
  all_gb_relative_inf[split_number,2] <- s$rel.inf[s$var=="BMI"]
  all_gb_relative_inf[split_number,3] <- s$rel.inf[s$var=="Age"]
  all_gb_relative_inf[split_number,4] <- s$rel.inf[s$var=="Insulin"]
  all_gb_relative_inf[split_number,5] <- s$rel.inf[s$var=="DiabetesPedigreeFunction"]
  all_gb_relative_inf[split_number,6] <- s$rel.inf[s$var=="Pregnancies"]
  all_gb_relative_inf[split_number,7] <- s$rel.inf[s$var=="BloodPressure"]
  all_gb_relative_inf[split_number,8] <- s$rel.inf[s$var=="SkinThickness"]
}

q_gb_importance <- data.frame(
  values = t(all_gb_relative_inf)
)
q_gb_importance$mean <- apply(q_gb_importance,1,function(row) mean(row[-1]))
q_gb_importance$sd <- apply(q_gb_importance,1,function(row) sd(row[-1]))
eb <- aes(ymax = mean+sd, ymin = mean-sd)
ggplot(data=q_gb_importance,aes(x=c(8:1),y=mean)) +
  geom_bar(stat="identity") +
  geom_errorbar(eb,width=0.5) +
  scale_x_continuous("factor",breaks=c(8:1),labels=c("Glucose","BMI","Age","Insulin","Pedigree","Pregnancies","BP","SkinThick")) +
  labs(y="mean importance (%)") +
  coord_flip()
