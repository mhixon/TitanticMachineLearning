#set directory to work within
setwd("~/Documents/Developer/Kaggle/Titanic")

### Prediction 1: Everyone dies :(

train <- read.csv("train.csv", stringsAsFactors = FALSE, header = T)
test <- read.csv("test.csv", stringsAsFactors = FALSE, header = T)

test$Survived <- rep(0,418)

# create a data frame with only values Kaggle cares about
submission <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

# write to a csv file our solution without row numbers
write.csv(submission, file = "1-titanic_solution_1.csv", row.names = FALSE)

# 62.679% Accurate
# Ranked: 5521
### End of Prediction 1


### Prediction 2: Females survive, Males die

# Change so that all of females survived

train <- read.csv("train.csv", stringsAsFactors = FALSE, header = T)
test <- read.csv("test.csv", stringsAsFactors = FALSE, header = T)

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

submission = data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submission, file = "1-titanic_solution_2.csv", row.names = FALSE)

# 76.555% Accurate
# Ranked: 4431
### End of Prediction 2


### Prediction 3: Females survive unless..., check if children survive

# Create new feature child if under 18

train <- read.csv("train.csv", stringsAsFactors = FALSE, header = T)
test <- read.csv("test.csv", stringsAsFactors = FALSE, header = T)

train$Child <- 0
train$Child[test$Age < 18] <- 1

#aggregate(Survived ~ Child + Sex, data=train, FUN=length)
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Being a child did not help you survive on the titanic :(

train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

# how did the rich fare?
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Males did bad
# Females with Pclass 3 did relatively bad, and females who paid 30+ did really bad

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

submission = data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submission, file = "1-titanic_solution_3.csv", row.names = FALSE)

# 77.990% Accurate
# Ranked: 3077
### End of Prediction 3


### Prediction 4: Decision Tree

train <- read.csv("train.csv", stringsAsFactors = FALSE, header = T)
test <- read.csv("test.csv", stringsAsFactors = FALSE, header = T)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
plot(fit)
text(fit)
fancyRpartPlot(fit)
prediction <- predict(fit, test, type="class")
submission = data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submission, file = "1-titanic_solution_4.csv", row.names = FALSE)

# 78.469% Accurate
# Ranked: 2303
### End of Prediction 4


### Prediction 5: Feature Engineering (Names)

train <- read.csv("train.csv", stringsAsFactors = FALSE, header = T)
test <- read.csv("test.csv", stringsAsFactors = FALSE, header = T)

test$Survived <- NA
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)
combi$Name[1]

combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
table(combi$Title)

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)

famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

train <- combi[1:891,]
test <- combi[892:1309,]
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")
fancyRpartPlot(fit)
prediction <- predict(fit, test, type="class")
submission = data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submission, file = "1-titanic_solution_5.csv", row.names = FALSE)

# 79.426% Accurate
# Ranked: 1496
### End of Prediction 5

### Prediction 6: Random Forest Model

#reload data
train <- read.csv("train.csv", stringsAsFactors = FALSE, header = T)
test <- read.csv("test.csv", stringsAsFactors = FALSE, header = T)
library(rpart)
library(randomForest)
library(party)
test$Survived <- NA
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)
combi$Name[1]

combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
table(combi$Title)

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)

famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

# must fill in NA's
# use basic decision tree to estimate age
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, 
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

combi$FamilyID2 <- combi$FamilyID
# Convert back to string
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
# And convert back to factor
combi$FamilyID2 <- factor(combi$FamilyID2)
combi$Sex <- factor(combi$Sex)
train <- combi[1:891,]
test <- combi[892:1309,]

set.seed(415)
# must factorize chars and have no nulls
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2
                    ,data = train, importance = TRUE, ntree = 2000)

varImpPlot(fit)
prediction <- predict(fit, test)
submission = data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submission, file = "1-titanic_solution_6.csv", row.names = FALSE)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3)) 
prediction <- predict(fit, test, OOB=TRUE, type = "response")
submission = data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submission, file = "1-titanic_solution_7.csv", row.names = FALSE)
# 80.861% Accurate
# Ranked: 519 
### End of Prediction 6




### Test out some R features


#load data from csv
#train <- read.csv("train.csv", stringsAsFactors = FALSE, header = T)
#test <- read.csv("test.csv", stringsAsFactors = FALSE, header = T)
#find mean fare
#mean(train$Fare)
#find most frequent Age
#mode(train$Age) # this returns type of object
#real_mode <- table(train$Age) # creates table of ages
#names(real_mode)[real_mode = max(real_mode)]
#median(train$Fare) # compute the median, divides values into to sets
#range(train$Fare) # finds the range of values (min, max)
#get boxplot of age vs. class
#boxplot(train$Age ~ train$Pclass, xlab = "Class", ylab = "Age", col = c("red"))
#var(train$Fare) # variance of Fare
#sqrt(var(train$Fare)) # std deviation of Fare
# create bar chart 
#hist(train$Fare, main = "Fare per Person", xlab = "Fare", col = "grey", breaks = 40, xlim = c(0,300))
#table(train$Survived) # number survived
#prop.table(table(train$Survived)) #proportion of survival

### End of Feature Testing