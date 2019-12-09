# PredictMatch.R
#
# Niti Wattanasirichaigoon
#
# Takes dataset generated from createMatchData.R and fits several models to predict results of a match
# The model performance are compared to each other

library(caret)
library(glmnet)

# import data
AUBdata <- read.csv("AvsB.csv")
AtoBdata <- read.csv("AtoB.csv")
AUBdata <- subset(AUBdata, select = -c(Round, A.b.HR, B.b.HR, A.s.IP, B.s.IP)) # Removing highly correlated fields
AtoBdata <- subset(AtoBdata,select = -c(Round, A.b.HR, A.s.IP))

ctrl <- trainControl(method = "repeatedcv", number = 10,  repeats = 10)
# train logistic regression models with 10 fold cross-validation all features
print("training LR models")
AUB_all_LR <- train(result ~., data = AUBdata, method = 'glm', family = binomial(), trControl = ctrl)
AtoB_all_LR <- train(result ~., data = AtoBdata, method = 'glm', family = binomial(), trControl = ctrl)

# train lasso regression models to use for feature selection
print("training lasso")
x <- model.matrix(result ~., data = AUBdata)[,-ncol(AUBdata)]
y <- factor(AUBdata$result)
lassoAUB <- cv.glmnet(x, y, alpha = 1, family='binomial')
#print(coef(lassoAUB, lassoAUB$lambda.min))
x <- model.matrix(result ~., data = AtoBdata)[,-ncol(AtoBdata)]
y <- factor(AtoBdata$result)
lassoAtoB <- cv.glmnet(x, y, alpha = 1, family='binomial')
#print(coef(lassoAtoB, lassoAtoB$lambda.min))

# feature reduction by selecting only those with p-value < 0.1 
print("training p selected LR")
AUB_p_LR <- train(result ~ A.r.LOB + A.b.K + B.r.LOB + B.b.K, data = AUBdata, method = 'glm', family = binomial(), trControl = ctrl)
AtoB_p_LR <- train(result ~ A.r.LOB + A.b.K + A.s.K9 + A.r.IP, data = AtoBdata, method = 'glm', family = binomial(), trControl = ctrl)

# feature selection by lasso
print("training lasso selected LR")
AUB_las_LR <- train(result ~ A.b.K + B.b.K, data = AUBdata, method = 'glm', family = binomial(), trControl = ctrl)
AtoB_las_LR <- train(result ~ A.b.BB + A.b.K + A.b.AVG + A.b.ISO + A.b.BsR + A.s.K9 + A.s.BB9 + A.s.HR9 + A.r.LOB + A.r.IP, data = AtoBdata, method = 'glm', family = binomial(), trControl = ctrl)

# train LDA models via 10 fold cross-validation
print("training LDS models")
AUB_all_LDA <- train(result ~., data = AUBdata, method = 'lda', trControl = ctrl)
AtoB_all_LDA <- train(result ~., data = AtoBdata, method = 'lda', trControl = ctrl)

# feature reduction by selecting only those with p-value < 0.1 
print("training p selected LDA")
AUB_p_LDA <- train(result ~ A.r.LOB + A.b.K + B.r.LOB + B.b.K, data = AUBdata, method = 'lda', trControl = ctrl)
AtoB_p_LDA <- train(result ~ A.r.LOB + A.b.K + A.s.K9 + A.r.IP, data = AtoBdata, method = 'lda', trControl = ctrl)

# feature reduction by selecting only those with p-value < 0.1 
print("training lasso selected LDA")
AUB_las_LDA <- train(result ~ A.b.K + B.b.K, data = AUBdata, method = 'lda', trControl = ctrl)
AtoB_las_LDA <- train(result ~ A.b.BB + A.b.K + A.b.AVG + A.b.ISO + A.b.BsR + A.s.K9 + A.s.BB9 + A.s.HR9 + A.r.LOB + A.r.IP, data = AtoBdata, method = 'lda', trControl = ctrl)

model.acc <- c(AUB_all_LR$results$Accuracy, AtoB_all_LR$results$Accuracy, 
    AUB_p_LR$results$Accuracy, AtoB_p_LR$results$Accuracy, 
    AUB_las_LR$results$Accuracy, AtoB_las_LR$results$Accuracy,
    AUB_all_LDA$results$Accuracy, AtoB_all_LDA$results$Accuracy, 
    AUB_p_LDA$results$Accuracy, AtoB_p_LDA$results$Accuracy, 
    AUB_las_LDA$results$Accuracy, AtoB_las_LDA$results$Accuracy)

model.names <- c("AUB_all_LR", "AtoB_all_LR", "AUB_p_LR", "AtoB_p_LR", "AUB_las_LR", "AtoB_las_LR",
                 "AUB_all_LDA", "AtoB_all_LDA", "AUB_p_LDA", "AtoB_p_LDA", "AUB_las_LDA", "AtoB_las_LDA")

# import 2019 data for testing
# (this dataset contains year and team)
AtoB2019 <- read.csv("AtoB2019.csv")
AUB2019 <- read.csv("AvsB2019.csv")
AtoB2019 <- AtoB2019[,-c(1:3)] #remove year and team 
AUB2019 <- AUB2019[,-c(1:3)]

# predict the testing data
print("predicting test data")
mods <- list(AUB_all_LR, AtoB_all_LR, AUB_p_LR, AtoB_p_LR, AUB_las_LR, AtoB_las_LR, AUB_all_LDA, AtoB_all_LDA, AUB_p_LDA, AtoB_p_LDA, AUB_las_LDA, AtoB_las_LDA)
test_acc <- c()
correct <- c()
for (i in 1:length(mods)) {
  testdata <- AUB2019
  if (i %% 2 == 0){
    testdata = AtoB2019
  }
  win_probs <- predict(mods[i], newdata = testdata, type = "prob")
  pred <- rep("lose",nrow(testdata))
  pred[win_probs[[1]][2]>0.5] <- "win"
  pred <- factor(pred, levels = c("lose", "win"))
  test_acc[i] <- sum(pred == "win")/7
  correct[i] <- sum(pred == "win")
}
Accuracies <- data.frame(Models = factor(model.names, levels = model.names), trainAcc = model.acc, testAcc = test_acc, matchesCorrect = correct)

par(mar = c(8,5,3,2))
acc_plot <- barplot(Accuracies$trainAcc, names = Accuracies$Models, ylim = c(0,0.7), las = 2, ylab = "Accuracy", main = "Model Accuracies")
text(x = acc_plot, y = Accuracies$trainAcc, label = round(Accuracies$trainAcc, digits = 2), pos = 3, cex = 0.8, col = "red")
res_plot <- barplot(Accuracies$matchesCorrect, names = Accuracies$Models, ylim = c(0,7), las = 2, ylab = "Matches correct", main = "Correctly Predicted Matches", col = "chartreuse3")