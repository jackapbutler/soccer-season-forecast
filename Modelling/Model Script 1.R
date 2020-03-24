# Load data
setwd('C:/Users/jacka/Desktop/Pythia Sports Test/Modelling')
data <- read.csv("Train_Data.csv", header = T)
library(dplyr)
library(magrittr)
library(xlsx)
library(xtable)
data <- rename(data, HomeScore = ï..HomeScore)
head(data)

# Remove shots variables as these are impossible to know before the match
# Remove match ID as this is irrelevant
# Also remove season ID as we do not have enough data for this to be a valid predictor
newdata <- data[c(-3,-5,-7,-9)]
head(newdata)
print.xtable(xtable(head(newdata)), file = "./newdatahead.txt")

# Observe an approximate Poisson distribution
plotdata <- subset(newdata, HomeScore < 8)
plotdata <- subset(plotdata, AwayScore < 8)

goals <- plotdata[c(-3,-4,-5,-6,-7,-8,-9)]
goals <- data.frame(goals)
head(goals)
library(ggplot2)
library(reshape2)
library(scales)

my_post_theme=
  theme_minimal() +
  theme(axis.text.x = element_text(face="bold", color="#666666", 
                                   size=10, margin = margin(t = -5)),
        axis.title = element_text(color="black", face="bold",size=12),
        plot.title = element_text(color="black", face="bold", size=14),
        axis.text.y = element_text(face="bold", color="#666666", 
                                   size=10),
        legend.text=element_text(size=12),
        legend.title=element_text(size=13),
        legend.key=element_blank(),
        axis.ticks.length=unit(0, "cm"))

# Find Poisson approximation
lambda_H = mean(newdata$HomeScore)
lambda_A = mean(newdata$AwayScore)
Home_Score <- rpois(nrow(newdata), lambda = lambda_H)
Away_Score <- rpois(nrow(newdata), lambda = lambda_A)
Away_Score <- subset(Away_Score, Away_Score < 8)
Home_Score <- subset(Home_Score, Home_Score < 8)
simulation <- data.frame(Home_Score, Away_Score)

# Plotting frequency
ggplot(melt(simulation), aes(value, fill = variable)) + 
  geom_bar(position = "dodge") +
  labs(title="Poisson Approximated Goals per Match") +
  labs(x="Goals", y="Count of Matches") +
  my_post_theme + 
  scale_fill_manual(values=c("dodgerblue4", "tan2")) +
  theme(legend.title = element_blank())

ggplot(melt(goals), aes(value, fill = variable)) + 
  geom_bar(position = "dodge") +
  labs(title="Actual Goals per Match") +
  labs(x="Goals", y="Count of Matches") +
  my_post_theme + 
  scale_fill_manual(values=c("dodgerblue4", "tan2")) +
  theme(legend.title = element_blank()) 

# Convert all categorical variables to factors
newdata$Gameweek = as.factor(newdata$Gameweek)
newdata$AwayTeamID = as.factor(newdata$AwayTeamID)
newdata$HomeTeamID = as.factor(newdata$HomeTeamID)


# Removed Gameweek as a predictor variable as it raises the AIC
n = round(0.20*54, 0)
traindata <- head(newdata,-(n*14))
testdata <- tail(newdata, (n*14))

# Poisson Model
poisson_model_data <- rbind(
  data.frame(goals=traindata$HomeScore,
               team=traindata$HomeTeamID,
               opponent=traindata$AwayTeamID,
               home=1),
  data.frame(goals=traindata$AwayScore,
               team=traindata$AwayTeamID,
               opponent=traindata$HomeTeamID,
               home=0)) 

poisson_model <- glm(goals ~ home + team + opponent, 
                     family = "poisson", data = poisson_model_data)
summary(poisson_model)

# Predict
test_data <- rbind(
  data.frame(team=testdata$HomeTeamID,
             opponent=testdata$AwayTeamID,
             home=1),
  data.frame(team=testdata$AwayTeamID,
             opponent=testdata$HomeTeamID,
             home=0)) 

test_data['Predicted_Goals'] <- round(predict.glm(poisson_model, 
                  newdata = test_data, type = "response"), 0)

# Reformatting of the predictions 
dat_h <- test_data[(test_data$home==1),]
dat_h <- rename(dat_h, Predicted_Home_Goals = Predicted_Goals)

dat_a <- test_data[(test_data$home==0),]
dat_a <- rename(dat_a, Predicted_Away_Goals = Predicted_Goals)

test_data <- cbind(dat_h, dat_a)
test_data <- test_data[c(-5,-6,-7)]
test_data <- rename(test_data, Home_Team = team)
test_data <- rename(test_data, Away_Team = opponent)
head(test_data)

# Make tables for LaTeX
write.csv(test_data,"C:/Users/jacka/Desktop/Pythia Sports Test/Modelling/test.csv", row.names = FALSE)
wld <- read.csv(file.choose(), header = TRUE)
wld <- rename(wld, Metric = ï..Metric)
print.xtable(xtable(head(wld)), file = "./win_lose_draw.txt")

# Predict Season 2
fulltrain_data <- rbind(
  data.frame(goals=newdata$HomeScore,
             team=newdata$HomeTeamID,
             opponent=newdata$AwayTeamID,
             home=1),
  data.frame(goals=newdata$AwayScore,
             team=newdata$AwayTeamID,
             opponent=newdata$HomeTeamID,
             home=0)) 

full_poisson_model <- glm(goals ~ home + team +opponent, 
                     family = "poisson", data = fulltrain_data)
summary(full_poisson_model)

S2_data = read.csv('fixtures.csv', header = T)
S2_data$AwayTeamID = as.factor(S2_data$AwayTeamID)
S2_data$HomeTeamID = as.factor(S2_data$HomeTeamID)

S2_data <- rbind(
  data.frame(team=S2_data$HomeTeamID,
             opponent=S2_data$AwayTeamID,
             home=1),
  data.frame(team=S2_data$AwayTeamID,
             opponent=S2_data$HomeTeamID,
             home=0)) 

S2_data['Predicted_Goals'] <- round(predict.glm(full_poisson_model, 
                                                  newdata = S2_data, type = "response"), 0)

head(S2_data)
dat_h <- S2_data[(S2_data$home==1),]
dat_h <- rename(dat_h, Predicted_Home_Goals = Predicted_Goals)

dat_a <- S2_data[(S2_data$home==0),]
dat_a <- rename(dat_a, Predicted_Away_Goals = Predicted_Goals)

S2_data <- cbind(dat_h, dat_a)
S2_data <- S2_data[c(-5,-6,-7)]
S2_data <- rename(S2_data, Home_Team = team)
S2_data <- rename(S2_data, Away_Team = opponent)
head(S2_data)

write.csv(S2_data,"C:/Users/jacka/Desktop/Pythia Sports Test/Modelling/season2_preds.csv", row.names = FALSE)
write.csv(S2_data,"C:/Users/jacka/Desktop/Pythia Sports Test/Modelling/season2_rank.csv", row.names = FALSE)

season2 <- read.csv(file.choose())
season2 <- rename(season2, Metric = ï..Metric)

# Simulate matches, matrix is equivalent to Prob(scoreline)
simulate_match <- function(full_poisson_model, homeTeam, awayTeam, max_goals=10){
  home_goals_avg <- predict(full_poisson_model,
                            data.frame(home=1, team=homeTeam, 
                                       opponent=awayTeam), type="response")
  away_goals_avg <- predict(full_poisson_model, 
                            data.frame(home=0, team=awayTeam, 
                                       opponent=homeTeam), type="response")
  dpois(0:max_goals, home_goals_avg) %o% dpois(0:max_goals, away_goals_avg) 
}

# Converting these simulations to probabilities
match_15_20 <- simulate_match(full_poisson_model, "15", "20", max_goals = 8)
"P(15 winning)" <- sum(match_15_20[lower.tri(match_15_20)])
"P(20 winning)" <- sum(match_15_20[upper.tri(match_15_20)])
"P(Draw)" <- 1- `P(15 winning)` - `P(20 winning)`

# league table
season2_rank = read.csv('season2_rank.csv', header = T)
unique_ids <- unique(season2_rank$Home_Team)
head(season2_rank)
  
rank <- function (match_result){
  table <- data.frame(name = unique_ids,
                      goal_score = rep(0,length(unique_ids)),
                      goal_conceded = rep(0,length(unique_ids)),
                      point = rep(0,length(unique_ids)))
  # loop through all the results and then update
  for (i in 1:nrow(match_result)){
    home = match_result$Home_Team[i]
    away = match_result$Away_Team[i]
    h_goal = match_result$Predicted_Home_Goals[i]
    a_goal = match_result$Predicted_Away_Goals[i]
    
    # add goal
    table[table$name == home,]$goal_score = table[table$name == home,]$goal_score + h_goal
    table[table$name == home,]$goal_conceded = table[table$name == home,]$goal_conceded + a_goal
    table[table$name == away,]$goal_score = table[table$name == away,]$goal_score + a_goal
    table[table$name == away,]$goal_conceded = table[table$name == away,]$goal_conceded + h_goal
    
    
    # calculate point
    if (h_goal > a_goal){
      table[table$name==home,]$point = table[table$name == home,]$point + 3
    }
    else if (h_goal < a_goal){
      table[table$name == away,]$point = table[table$name == away,]$point + 3
    }
    else{
      table[table$name == home,]$point = table[table$name == home,]$point + 1
      table[table$name == away,]$point = table[table$name == away,]$point + 1
      
    table_season2 = table
    }
  }
  table <- table[order(-table$point, -table$goal_score), ]
  
  return (table)
}

rank(season2_rank)

avg_goals <- read.csv('avg_goals.csv')
head(avg_goals)

# Stochastic simulations
scoreline_predict <- function (home, away){
  h_scored = rpois(1, avg_goals[avg_goals$TeamID == home,]$avg_home_goals)
    
  a_scored = rpois(1, avg_goals[avg_goals$TeamID == away,]$avg_away_goals)
  
  return (list(h_scored, a_scored))
}

simulate_season <- function(fixtures){
  matches <- mapply(scoreline_predict, fixtures$Home_Team, fixtures$Away_Team, SIMPLIFY = FALSE)
  fixtures$Predicted_Home_Goals <- unlist(sapply(matches, function(x) x[1]))
  fixtures$Predicted_Away_Goals <- unlist(sapply(matches, function(x) x[2]))
  table <- rank(fixtures)
  return (table)
}

# Wrap the above simulation functions into a league table generator
nsim = 10000
tabulate_data <- data.frame(name = unique_ids, One = rep(0,length(unique_ids)), 
Two = rep(0,length(unique_ids)), Three = rep(0,length(unique_ids)), Four = rep(0,length(unique_ids)), 
Five = rep(0, length(unique_ids)),Six = rep(0, length(unique_ids)),Seven = rep(0, length(unique_ids)),
Eight = rep(0, length(unique_ids)),Nine = rep(0, length(unique_ids)),Ten = rep(0, length(unique_ids)),
Eleven = rep(0, length(unique_ids)),Twelve = rep(0, length(unique_ids)),Thirteen = rep(0, length(unique_ids)),
Fourteen = rep(0, length(unique_ids)),Fifteen = rep(0, length(unique_ids)),Sixteen = rep(0, length(unique_ids)),
Seventeen = rep(0, length(unique_ids)),Eighteen = rep(0, length(unique_ids)),Nineteen = rep(0, length(unique_ids)), 
Twenty = rep(0, length(unique_ids)), Twenty_One = rep(0, length(unique_ids)),Twenty_Two = rep(0, length(unique_ids)), 
Twenty_Three = rep(0, length(unique_ids)), Twenty_Four = rep(0, length(unique_ids)),
Twenty_Five = rep(0, length(unique_ids)),Twenty_Six = rep(0, length(unique_ids)), 
Twenty_Seven = rep(0, length(unique_ids)), Twenty_Eight = rep(0, length(unique_ids)))
pb <- txtProgressBar(min = 0, max = nsim, style = 3)

for (sim in 1:nsim){
  table = simulate_season(season2)
  One_p = table$name[1]; Two_p = table$name[2]; Three_p = table$name[3]; Four_p = table$name[4]; 
  Five_p = table$name[5]; Six_p = table$name[6]; Seven_p = table$name[7]; Eight_p = table$name[8]; 
  Nine_p = table$name[9]; Ten_p = table$name[10]; Eleven_p = table$name[11]; Twelve_p = table$name[12]
  Thirteen_p = table$name[13]; Fourteen_p = table$name[14]; Fifteen_p = table$name[15]; Sixteen_p = table$name[16]
  Seventeen_p = table$name[17]; Eighteen_p = table$name[18]; Nineteen_p = table$name[19]; 
  Twenty_p = table$name[20];Twenty_One_p = table$name[21]; Twenty_Two_p = table$name[22]; 
  Twenty_Three_p = table$name[23]; Twenty_Four_p = table$name[24]; Twenty_Five_p = table$name[25]; 
  Twenty_Six_p = table$name[26]; Twenty_Seven_p = table$name[27]; Twenty_Eight_p = table$name[28]
  
  tabulate_data <- tabulate_data %>%
    mutate(One = ifelse(name == One_p, One+1, One), Two = ifelse(name == Two_p, Two+1, Two), 
Three = ifelse(name == Three_p, Three+1, Three),Four = ifelse(name == Four_p, Four+1, Four), 
Five = ifelse(name == Five_p, Five+1, Five), Six = ifelse(name == Six_p, Six+1, Six),
Seven = ifelse(name == Seven_p, Seven+1, Seven), Eight = ifelse(name == Eight_p, Eight+1, Eight), 
Nine = ifelse(name == Nine_p, Nine+1, Nine),Ten = ifelse(name == Ten_p, Ten+1, Ten), 
Eleven = ifelse(name == Eleven_p, Eleven+1, Eleven), Twelve = ifelse(name == Twelve_p, Twelve+1, Twelve),
Thirteen = ifelse(name == Thirteen_p, Thirteen+1, Thirteen), 
Fourteen = ifelse(name == Fourteen_p, Fourteen+1, Fourteen), 
Fifteen = ifelse(name == Fifteen_p, Fifteen+1, Fifteen),Fifteen = ifelse(name == Fifteen_p, Fifteen+1, Fifteen),
Sixteen = ifelse(name == Sixteen_p, Sixteen+1, Sixteen), 
Seventeen = ifelse(name == Seventeen_p, Seventeen+1, Seventeen),
Eighteen = ifelse(name == Eighteen_p, Eighteen+1, Eighteen), 
Nineteen = ifelse(name == Nineteen_p, Nineteen+1, Nineteen), 
Twenty = ifelse(name == Twenty_p, Twenty+1, Twenty),
Twenty_One = ifelse(name == Twenty_One_p, Twenty_One+1, Twenty_One),
Twenty_Two = ifelse(name == Twenty_Two_p, Twenty_Two+1, Twenty_Two), 
Twenty_Three = ifelse(name == Twenty_Three_p, Twenty_Three+1, Twenty_Three),
Twenty_Four = ifelse(name == Twenty_Four_p, Twenty_Four+1, Twenty_Four), 
Twenty_Five = ifelse(name == Twenty_Five_p, Twenty_Five+1, Twenty_Five),
Twenty_Six = ifelse(name == Twenty_Six_p, Twenty_Six+1, Twenty_Six),
Twenty_Seven = ifelse(name == Twenty_Seven_p, Twenty_Seven+1, Twenty_Seven),
Twenty_Eight = ifelse(name == Twenty_Eight_p, Twenty_Eight+1, Twenty_Eight))
setTxtProgressBar(pb, sim)}

# write result into csv
write.csv(tabulate_data, "league_simulation_all_places.csv", row.names = FALSE)
