##  User behavior analysis of ASSISTments online tutor system

- Project description:
What are the behavioral differences between high- and low-performing students in an online tutoring environment? Do those differences determine who is more successful v.s who is not? This project uses a large dataset from collected from the [ASSISTments tutor system](https://sites.google.com/site/assistmentsdata/home/assistment-2009-2010-data/skill-builder-data-2009-2010) to investigate these questions. Specifically questions include:

1. What are the key behavioral features that could distinguish high and low-performance users?
2. Using classification predictive models, can we use these key behavioral features to predict the outcome of question attempt?
3. Which predictive model performs the best, especially, to predict who's likely to answer a question correctly(true positive)?

#### install packages
```{r}
library(dplyr)
library(tidyr)
library(ggplot2)
```

#### load dataset
```{r}
df1 <- read.csv("skill_building_data_2009to2010.csv")
```
#### check dimension of dataset
```{r}
dim(df1)
# df1 has 401756 rows and 30 columns
```
#### data screening and pre-processing
```{r}
str(df1)
```

#### explore data
```{r}
df2 <- df1 %>% select(user_id, assistment_id, problem_id, original, correct, attempt_count,first_action,ms_first_response, overlap_time, hint_count, hint_total, first_action, bottom_hint)

df3 <- df2[complete.cases(df2), ]
#pairs(df3)
```

#### use 10% of the sample to examine data & outliers
```{r}
# draw a subset sample for data exploration
set.seed(1234)
select <- sample(1:nrow(df3), 0.1*nrow(df3), replace = F)
df_sub <- df3[select, ] # 40175 x 9
summary(df_sub)
# remove data points that have negative values in `ms_first_response` & `overlap_time`
df_sub <- filter(df_sub,ms_first_response >= 0 & overlap_time >=0) 
# `ms_first_response`, `overlap_time`, `attempt_count` seem to suffer outlier problems.
# use qqplot to inspect outlier
library(car)
qqPlot(df_sub$ms_first_response)
qqPlot(df_sub$overlap_time)
qqPlot(df_sub$attempt_count)
# use the following criteria to filter out outliers
# (1) first response : <= 280000 ms (4.67 min)
# (2) count of attempt: <= 20 
# (3) overlap time: <= 1000000 ms (16.67 min)
df_sub_clean <- filter(df_sub, ms_first_response <= 280000 & overlap_time <= 1000000 & attempt_count <= 20) # 6518 x 12

qqPlot(df_sub_clean$ms_first_response)
qqPlot(df_sub_clean$overlap_time)
qqPlot(df_sub_clean$attempt_count)
```

#### recode some variables
```{r}
df_sub_clean$question_type <- recode(df_sub_clean$original, "1='Main problem';0='Scaffolding problem'")
df_sub_clean$question_type <- factor(df_sub_clean$question_type, level= c('Main problem','Scaffolding problem'))
```

#### see relations between selected variables (leveled by question types)
```{r}
pairs(df_sub_clean[, 4:12], main = "relations between variables", col = df_sub_clean$question_type, upper.panel = NULL, pch = 16, cex = 0.5)
par(xpd=TRUE)
legend("topright", fill = unique(df_sub_clean$question_type), legend = c(levels(df_sub_clean$question_type)))
# no significant diffferent patterns are observed between 2 question types
```
![scatter-problem](https://github.com/eddiecylin/data-analytics/blob/master/6.%20Online-Tutoring-ASSISTments/scatter-problem.png)


Based on users' responses to questions, I performed a k-means to explore users' behavioral patterns

#### k-means on 10% of the data based on question responses
```{r}
# length(unique(df_sub_clean$user_id)) # unique user id: 1716 
library(cluster)
library(factoextra)
df_kmeans <- df_sub_clean[order(df_sub_clean$user_id), ]
df_kmeans <- df_kmeans %>% 
        select(attempt_count, ms_first_response, overlap_time, hint_count)
df_kmeans_scaled <- scale(df_kmeans)
k3 <- kmeans(df_kmeans_scaled, centers = 3, nstart = 30)
k4 <- kmeans(df_kmeans_scaled, centers = 4, nstart = 30)
k5 <- kmeans(df_kmeans_scaled, centers = 5, nstart = 30)
# k3 K-means clustering with 3 clusters of size 989, 1547, 3982
# k4 K-means clustering with 4 clusters of size 749, 3894, 1510, 365
# k5 K-means clustering with 5 clusters of sizes 240, 519, 2887, 1374, 1498

p3 <- fviz_cluster(k3, geom = "point",  data = df_kmeans_scaled) + ggtitle("k = 3")
p4 <- fviz_cluster(k4, geom = "point",  data = df_kmeans_scaled) + ggtitle("k = 4")
p5 <- fviz_cluster(k5, geom = "point",  data = df_kmeans_scaled) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p3, p4, p5, nrow = 3) 
# initial data viz indicates that k = 3 may be the optimal cluster number. Use elbow method(wss) to verify
set.seed(123)
fviz_nbclust(df_kmeans_scaled, kmeans, method = "wss") # 3-4 clusters should be the optimal choice. Here I choose 3 cluster.
set.seed(1357)
final <- kmeans(df_kmeans_scaled, 3, nstart = 30)
#print(final)
df_kmeans_cluster <- df_kmeans %>%
        mutate(cluster = final$cluster) %>%
        group_by(cluster) %>%
        summarise_all("mean")

# there are generally 3 types of users(based on question responses): 
# (1) higher attmpt counts x long first response time
# (2) lower attempt counts x shorter response time x use more hints
# (3) lower attempt counts x shorter response time x use fewer hints

```
![cluster-question](https://github.com/eddiecylin/data-analytics/blob/master/6.%20Online-Tutoring-ASSISTments/cluster-question.png)
![elbow-question](https://github.com/eddiecylin/data-analytics/blob/master/6.%20Online-Tutoring-ASSISTments/elbow-question.png)

As a single user could attempt multiple questons(many rows of data), I did k-means again but based on "unique" users and individual-based behaviors (but not question response as used previously)

#### associate behavioral patterns with performance levels
```{r}
df_all <- df_sub_clean[order(df_sub_clean$user_id), ]
df_all <- cbind(df_all, final$cluster)
# select only the original questions
df_all_orig_q <- filter(df_all, original == 0)
# df for correct answers
df_perf <- df_all_orig_q %>% 
        select(user_id,correct) %>%
        group_by(user_id) %>%
        summarise(avg_score = mean(correct))

# only 1 person has a non-zero value in the `correct` column
# use the whole df1 to experiment (a total of 4217 `user_id`)
```

#### create a new dataframe based on average performance
```{r}
df1_perf <- df1 %>% 
        select(user_id,correct,original) %>%
        filter(original == 1) %>%
        group_by(user_id) %>%
        summarise(avg_score = mean(correct)) 

# 1st quantile 0.47
# 3rd quantile 0.8
```

#### subset dataframe based on 1st & 3rd quantile of average performance
```{r}
df1_perf_hi_lo <- filter(df1_perf, avg_score >= 0.8 | avg_score <= 0.47) # 2192 unique users
df1_perf_hi_lo$performance <- recode(df1_perf_hi_lo$avg_score, "lo:0.47='low';0.8:hi='high'")
df1_perf_hi_lo <- df1_perf_hi_lo[ , c(1,3)] 
```

#### create a new dataframe based on average of number of attempts
```{r}
df1_attempt <- df1 %>% 
        select(user_id, attempt_count, original) %>%
        filter(original == 1 & attempt_count <= 20) %>%
        group_by(user_id) %>%
        summarise(avg_attempt = mean(attempt_count)) 
```

#### create a new dataframe based on average time of 1st response
```{r}
df1_first_res_time <- df1 %>% 
        select(user_id, ms_first_response, original) %>%
        filter(original == 1 & ms_first_response <= 280000) %>%
        group_by(user_id) %>%
        summarise(avg_res_time = mean(ms_first_response)) 
# change mille second into minute

df1_first_res_time$avg_res_time <- df1_first_res_time$avg_res_time*1.67e-5
```

#### create a new dataframe based on average time spent on a question
```{r}
df1_overlap_time <- df1 %>% 
        select(user_id, overlap_time, original) %>%
        filter(original == 1 & overlap_time <= 1000000) %>%
        group_by(user_id) %>%
        summarise(avg_overlap_time = mean(overlap_time)) 
# change mille second into minute

df1_overlap_time$avg_overlap_time <- df1_overlap_time$avg_overlap_time*1.67e-5
```

#### dcreate a new dataframe based on average number of hints used
```{r}
df1_avg_hint <- df1 %>% 
        select(user_id, hint_count,hint_total, original) %>%
        filter(original == 1) %>%
        mutate(hint_used = hint_count) %>%
        group_by(user_id) %>%
        summarise(avg_hint_used = mean(hint_used)) 
```

#### put sub-dataframes together for a user-based composite dataframe
```{r}
df_composite <- inner_join(df1_perf_hi_lo, df1_attempt, by ='user_id')
df_composite <- inner_join(df_composite, df1_first_res_time, by ='user_id')
df_composite <- inner_join(df_composite, df1_overlap_time, by ='user_id' )
df_composite <- inner_join(df_composite, df1_avg_hint, by ='user_id' )

# total of 2178 unique users 
```

#### variable relations (user-based)
```{r}
df_composite$performance <- factor(df_composite$performance, levels = c('low', 'high'))
pairs(df_composite[, 3:6], main = "relations between variables", col = df_composite$performance, upper.panel = NULL, pch = 16, cex = 0.5)
par(xpd=TRUE)
legend("topright", fill = unique(df_composite$performance), legend = c(levels(df_composite$performance)))
```
![scatter-performance](https://github.com/eddiecylin/data-analytics/blob/master/6.%20Online-Tutoring-ASSISTments/scatter-performance.png)

#### seperate variables to have a different view
```{r}
df_composite_hi <- filter(df_composite, performance == 'high')
df_composite_lo <- filter(df_composite, performance == 'low')  
```

```{r eval=FALSE, include=FALSE}
par(mfrow=c(2,2))
pairs(df_composite_hi[, 3:6], main = "relations between variables(high performance)", upper.panel = NULL, pch = 16, cex = 0.5)
pairs(df_composite_lo[, 3:6], main = "relations between variables(low performance)", upper.panel = NULL, pch = 16, cex = 0.5)

```

#### kmeans on 2,178 unique users
```{r}
df_kmeans_com <- df_composite[order(df_composite$user_id), ]
df_kmeans_com <- df_kmeans_com %>% 
        select(avg_attempt, avg_res_time, avg_overlap_time, avg_hint_used)

df_kmeans_com_scaled <- scale(df_kmeans_com)

k2_com <- kmeans(df_kmeans_com_scaled, centers = 2, nstart = 30)
k3_com <- kmeans(df_kmeans_com_scaled, centers = 3, nstart = 30)
k4_com <- kmeans(df_kmeans_com_scaled, centers = 4, nstart = 30)


p2_com <- fviz_cluster(k2_com, geom = "point",  data = df_kmeans_com_scaled) + ggtitle("k = 2")
p3_com <- fviz_cluster(k3_com, geom = "point",  data = df_kmeans_com_scaled) + ggtitle("k = 3")
p4_com <- fviz_cluster(k4_com, geom = "point",  data = df_kmeans_com_scaled) + ggtitle("k = 4")

library(gridExtra)
grid.arrange(p2_com, p3_com, p4_com, nrow = 3) 
# initial data viz indicates that k = 3 may be the optimal cluster number. Use elbow method(wss) to verify
set.seed(123)
fviz_nbclust(df_kmeans_com_scaled, kmeans, method = "wss") # 3-4 clusters should be the optimal choice. Here I choose 2 cluster.

set.seed(1357)
final_com <- kmeans(df_kmeans_com_scaled, 3, nstart = 30)
print(final_com)

df_kmeans_cluster_com <- df_kmeans_com %>%
        mutate(cluster = final_com$cluster) %>%
        group_by(cluster) %>%
        summarise_all("mean")

df_composite <- cbind(df_composite,cluster = final_com$cluster)

```
![cluster-users](https://github.com/eddiecylin/data-analytics/blob/master/6.%20Online-Tutoring-ASSISTments/cluster-users.png)
![elbow-users](https://github.com/eddiecylin/data-analytics/blob/master/6.%20Online-Tutoring-ASSISTments/elbow-users.png)

#### see patterns between cluster labels and performance labels
```{r}

t <- select(df_composite, performance, cluster)

t <- t %>%
        group_by(performance, cluster) %>%
        summarise(count = n())
```

#### key behavioral patterns between high/ low-performance users:
- low: high attempt, short respond time, use more hints
- high: low attempt, short total time, use fewer hints

After identifying behavioral patterns and some distinguishing features(using user-based data), I built 3 different predictive models below to predict outcome of attempting a question (note: these models are trained on question response, not user-based) 

#### use random forest to see feature importance
```{r}
df_rf <- select(df1, correct, attempt_count,ms_first_response,overlap_time,hint_count, skill_name, tutor_mode)
# I used 2 additional features:skill_name, tutor_mode to explore if they will have any influence (to verify the initial results from visualizaton of feature relation)
df_rf <- df_rf[complete.cases(df_rf), ]
df_rf$correct <- factor(df_rf$correct)
```

#### subset 10% of the sample 
```{r}
set.seed(123)
select <- sample(1:nrow(df_rf()), 0.1*nrow(df_rf), replace = FALSE)
df_rf_sub <- df_rf[select, ]
rownames(df_rf_sub) <- c(1:nrow(df_rf_sub))
```

#### initial random forest fitting 
```{r}
library(randomForest)
set.seed(1234)
rfFit <- randomForest(correct ~ ., data = df_rf_sub, ntree = 500)
print(rfFit)

```
![fit-rf](https://github.com/eddiecylin/data-analytics/blob/master/6.%20Online-Tutoring-ASSISTments/fit-rf.png)

#### tuning random forest
```{r}
mtry <- tuneRF(df_rf_sub[-1], df_rf_sub$correct, ntreeTry=1501,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

set.seed(42)
rfFit2 <-randomForest(correct ~., data = df_rf_sub, mtry=best.m, importance=TRUE, ntree=501)
print(rfFit2)

varImpPlot(rfFit2)
# feature importance shows that 4 features are of great importance:hint_count, attempt_count, overlap_time, ms_first_response
```

![feature-importance-rf](https://github.com/eddiecylin/data-analytics/blob/master/6.%20Online-Tutoring-ASSISTments/feature-importance-rf.png)

#### create test set for random forest
```{r}
df_rf_test <- df_rf[-select, ]
test_select <- sample(1:nrow(df_rf_test), 0.1*nrow(df_rf_test), replace = FALSE)
df_rf_test <- df_rf_test[test_select, ] # 36,158 x 7
```

#### predict with random forest
```{r}
library(caret)
library(ROCR)
predicted_rf <- predict(rfFit2, newdata = df_rf_test, type = 'prob')
pred_rf <- prediction(predicted_rf[,2], df_rf_test$correct)
perf_rf_acc <- performance(pred_rf, measure = "acc")
perf_rf_auc <- performance(pred_rf, measure = "auc")
plot(perf_rf_acc) 

ind_rf <-  which.max(slot(perf_rf_acc, "y.values")[[1]])
acc_rf = slot(perf_rf_acc, "y.values")[[1]][ind_rf]
cutoff_rf = slot(perf_rf_acc, "x.values")[[1]][ind_rf]
print(c(accuracy= acc_rf, cutoff = cutoff_rf))
# accuracy = .94ˇ / AUC = .96 with cutoff = 0.52
```

#### re-create new train/ test dataset for the other 2 model trainings
```{r}
new_train <- df_rf_sub[, 1:5]
new_test <- df_rf_test[, 1:5]
```

#### KNN model training
```{r}
library(class)

trControl <- trainControl(method  = "cv",
                          number  = 10)

knnFit <- train(correct ~ attempt_count + ms_first_response + overlap_time + hint_count,
        preProcess = c('center','scale'),
        method = "knn",
        tuneGrid = expand.grid(k = 1:10),
        trControl  = trControl,
        metric = "Accuracy",
        data = new_train)
knnFit
plot(knnFit)
# accuracy: .95 with k=1

# predict test data & ROC
predicted_knn <- predict(knnFit, new_test, type = 'prob')
pred_knn <- prediction(predicted_knn[,2], new_test$correct)
perf_knn_acc <- performance(pred_knn, measure = "acc")
perf_knn_auc <- performance(pred_knn, measure = "auc")
plot(perf_knn_acc) 
```
![number-of-neighbor-knn](https://github.com/eddiecylin/data-analytics/blob/master/6.%20Online-Tutoring-ASSISTments/number-of-neighbor-knn.png)

#### predict new data with kNN and accuracy 
```{r}
ind_knn <-  which.max(slot(perf_knn_acc, "y.values")[[1]])
acc_knn = slot(perf_knn_acc, "y.values")[[1]][ind_knn]
cutoff_knn = slot(perf_knn_acc, "x.values")[[1]][ind_knn]
print(c(accuracy= acc_knn, cutoff = cutoff_knn))
# accuracy =.95 / AUC = .94 with cutoff = .5

```

#### SVM model training
```{r}
library(e1071)
set.seed(123)
# initial model
svmFit <- svm(correct ~ attempt_count + ms_first_response + overlap_time + hint_count, data = new_train, kernel = 'radial', cost = 16, gamma =2, scale = TRUE)
```

#### note: I used cost = 16 and gamma = 2 through SVM hyperparameter tuning below
```{r eval=FALSE, include=FALSE}
library(pROC)
pkgs <- c('foreach', 'doParallel')
lapply(pkgs, require, character.only = T)
registerDoParallel(cores = 4)
x <- paste("attempt_count + ms_first_response + overlap_time + hint_count")
fml <- as.formula(paste("as.factor(correct) ~ ", x))
# split data into 10 folds
set.seed(1357)
new_train$fold <- caret::createFolds(1:nrow(new_train), k = 10, list = FALSE)
# set parameters forgrid search 
cost <- 2^(2:4)
gamma <- 2^(-1:1)
parms <- expand.grid(cost = cost, gamma = gamma)
# loop thru parameter values
result <- foreach(i = 1:nrow(parms), .combine = rbind) %do% {
  c <- parms[i, ]$cost
  g <- parms[i, ]$gamma
  ### K-FOLD VALIDATION ###
  out <- foreach(j = 1:max(new_train$fold), .combine = rbind, .inorder = FALSE) %dopar% {
    tra <- new_train[new_train$fold != j, ]
    vali <- new_train[new_train$fold == j, ]
    svmFit <- e1071::svm(fml, data = tra, type = "C-classification", kernel = "radial", cost = c, gamma = g, probability = TRUE)
    pred <- predict(svmFit, vali, decision.values = TRUE, probability = TRUE)
    data.frame(y = vali$correct, prob = attributes(pred)$probabilities[, 2])
  }
  # svm model performance w/ ROC 
  roc <- pROC::roc(as.factor(out$y), out$prob) 
  data.frame(parms[i, ], roc = roc$auc[1])
}
```

#### predict new data with SVM and accuracy
```{r}
predicted_svm <- predict(svmFit, new_test, type = 'prob')
df_predicted_svm <-data.frame(predicted_svm) # store predicted results in a df
df_predicted_svm <- df_predicted_svm$predicted_svm
df_pre_true_label <- data.frame(cbind(df_predicted_svm = df_predicted_svm, svm_tru_label = new_test$correct))

pred_svm <- prediction(df_pre_true_label$df_predicted_svm, df_pre_true_label$svm_tru_label)
perf_svm_acc <- performance(pred_svm, measure = "acc")
perf_svm_auc <- performance(pred_svm, measure = "auc")
plot(perf_svm_acc)

ind_svm <-  which.max(slot(perf_svm_acc, "y.values")[[1]])
acc_svm = slot(perf_svm_acc, "y.values")[[1]][ind_svm]
cutoff_svm = slot(perf_svm_acc, "x.values")[[1]][ind_svm]
print(c(accuracy= acc_svm, cutoff = cutoff_svm)) 
# accuracy =.92 / AUC = .89   
```
Brief summary: it seems that `attempt_count` and `hint _count` are the most important features in all 3 predictive model for correct question responses

#### examine model performance on the original complete dataset(excluding data used previously for training & validating )
```{r}
# exclude training and validating data from the original df
df_predict <- select(df1, c(colnames(new_train)))
df_predict$correct <- factor(df_predict$correct)
df_predict <- setdiff(df_predict, new_train)
df_predict <- setdiff(df_predict, new_test) # 117,275 x 5
```
#### random forest
```{r}
all_rf <- predict(rfFit2, newdata = df_predict, type = 'response')
# check model accuracy with confusion matrix
cmat_rf <- confusionMatrix(all_rf, df_predict$correct)  
# accuracy = .92 ; kappa: .82
```
#### kNN
```{r}
all_knn <- predict(knnFit, newdata = df_predict, type = 'raw')

cmat_knn <- confusionMatrix(all_knn, df_predict$correct) 
# accuracy = .92 ; kappa: .82
```
#### SVM
```{r}
all_svm <- predict(svmFit, newdata = df_predict, type = 'raw')

cmat_svm <- confusionMatrix(all_svm, df_predict$correct)
# accuracy = .89 ; kappa: .76
```

#### Project summary: 
1. High/low- performance users are distinguished by few/more number of question attempts & hints use
2. There is no significant difference in response time between the 2 kinds of users 
3. Number of question attempts & hints use are the 2 most important features to distinguish the 2 types of users
4. Using 4 selected features(number of attempts, number of used hints, response time, total time spent on a question), random forest and kNN models in this project can predict nearly 92% of outcome when a user attempts a question
5. While there is no significant difference in average time spent on questions between high- and low-performance users, it could be inferred that high-performance users take time to think and answer questions. On the other hand, low-performance users spend time asking for hints and attempting questions.
