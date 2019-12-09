# Tournament.R
#
# Predicts the outcome of the entire tounament of 8 MLB teams
# The logistic regression model predictors selected by lasso: A.b.K and B.b.K (strikeouts per plate)

library(caret)

# import and filter data
team_stats <- read.csv("dataPPS.csv")
teams <- team_stats[team_stats$Season==2019,-c(1,21:27)]
columns <- colnames(teams)[3:19]

# train CV logistic model (based on 2001-2018 data)
AUBdata <- read.csv("AvsB.csv")
AUBdata <- subset(AUBdata, select = c(A.b.K, B.b.K, result))

# train a logistic regression model via 10 fold cross-validation
ctrl <- trainControl(method = "repeatedcv", number = 10,  repeats = 10)
#AUB_las_LR <- train(result ~ A.b.K + B.b.K, data = AUBdata, method = 'glm', family = binomial(), trControl = ctrl)

# predict 2019 matches
results <- read.csv("AvsB2019.csv")
res2019 <- results[results$Year==2019,]
res2019 <- subset(res2019, select = c(win, Lose, A.b.K, B.b.K, result))
res2019$win_p <- 0
res2019$pred <- "lose"
pred.matches <- function(res2019){
  for (i in 1:nrow(res2019)) {
    win_prob <- predict(AUB_las_LR, newdata = res2019[i,], type = "prob")
    res2019$win_p[i] = win_prob[,2]
    if (res2019$win_p[i] > 0.5){
      res2019$pred[i] <- "win"
    }
    print(res2019[i,])
  }
}

# Assign teams by data row according to real brackets
team1 <- teams[1,] #Astros
team2 <- teams[6,] #Rays
team3 <- teams[8,] #Yankees
team4 <- teams[7,] #Twins
team5 <- teams[5,] #Nationals
team6 <- teams[4,] #Dodgers
team7 <- teams[3,] #Cardinals
team8 <- teams[2,] #Braves
bracket <- rbind(team1, team2, team3, team4, team5, team6, team7, team8)

# predict the outcome of the whole tournament
winning_team <- ""
round_name <- c("Quarterfinals", "Semifinals", "Grand finals")

playoff <- function(){
  for (rnd in 1:3) {
    for (match in 2^(3-rnd):1) {
      temp_stats <- data.frame(bracket[2*match-1, "b.K"], bracket[2*match, "b.K"])
      colnames(temp_stats) <- c("A.b.K", "B.b.K")
      win_prob <- predict(AUB_las_LR, newdata = temp_stats, type = "prob")
      teamA <- bracket[2*match-1, 2]
      teamB <- bracket[2*match, 2]
      if (win_prob["win"]>0.5){
        winning_team = teamA
        bracket <- bracket[-2*match,]
      }
      else {
        winning_team = teamB
        bracket <- bracket[-(2*match-1),]
      }
      print(paste(round_name[rnd], ": ", teamA, "vs", teamB, ", winner =", winning_team))
    }
  }
}
