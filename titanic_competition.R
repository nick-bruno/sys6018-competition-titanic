## Titanic Competition ##
## August 28, 2018 ##
## Nick Bruno (nhb3zf) ##

# Upload libraries and set working directory #
library(tidyverse)
library(ggplot2)
library(car)
setwd("/Users/nickbruno/Documents/fall_2018/SYS6018/Titanic_competition/all")

# Import data #
test <- read_csv('test.csv')
train <- read_csv('train.csv')
gender <- read_csv('gender_submission.csv')

# Analyze training data #
names(train) # looks at variable names
summary(train) # summarizes each variable
survivors <- train[train['Survived'] == 1, ] # subsets the data by those who survived
dead <- train[train['Survived'] == 0, ] # subsets the data by those who died
head(train)

# Before analysis,  the variables that seem the most important to me are ticket class, sex, age, fare, and
# cabin number. However, I fer their may be multicollinearity between fair and ticket class #

# Age #
mean(survivors$Age, na.rm=T)
  # [1] 28.34369
summary(survivors$Age, na.rm=T)
  # min = 0.42
mean(dead$Age, na.rm=T)
  # [1] 30.62618
summary(dead$Age, na.rm=T)
  # min = 1.0
# Implies that younger people (specifically children) were more likely to survive

# Investigating Ticket Class #
gb_class <- group_by(survivors, Pclass)
survive_class <- summarise(gb_class, sum=sum(Pclass))
  # Looks at number of survivors per class
gb_class_die <- group_by(dead, Pclass)
die_class <- summarise(gb_class_die, sum=sum(Pclass))
  # Looks at number of dead per class
new_tibble <- survive_class/(die_class + survive_class) # finds percentages of class members who lived
new_tibble$Pclass <- c(1,2,3)
rename(new_tibble, perc_live = sum)
  # Creates final tibble. As expected, if you are in tibble class one you are more likely to live

# Analyzing Cabins #
# Only letters should really matter. #
train$Cabin
train$cabinletter <- substr(train$Cabin, 0, 1)
a_cabins <- train[train['cabinletter'] == 'A', ]
a_cabins$Pclass # all 1
b_cabins <- train[train['cabinletter'] == 'B', ]
b_cabins$Pclass # all 1
c_cabins <- train[train['cabinletter'] == 'C', ]
c_cabins$Pclass # all 1
d_cabins <- train[train['cabinletter'] == 'D', ]
d_cabins$Pclass # 1's and 2's (mostly 1's)
e_cabins <- train[train['cabinletter'] == 'E', ]
e_cabins$Pclass # 1-3
f_cabins <- train[train['cabinletter'] == 'F', ]
f_cabins$Pclass # all 2's and 3's

# It seems that the cabin level, indicated by a letter, leads to which ticket class they are in. I do
# not believe that specific room numbers is valid in this regression. May be multicollinearity with 
# this variable and ticket class, so ticket class is likely better in the model while I will omit the 
# cabin number. Also, there are so many null values that it probably is not worth it to include this 
# variable in the regression. 

# Logit regression #
names(train)
train$Pclass <- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)

nrow(train) # 891
nrow(train)*(3/4)
# I will randomly select 669 observations for my test set
sub <- sample(1:nrow(train),size=669)
train_train <- train[sub,]
train_valid <- train[-sub,]
names(train)
logit_1 <- glm(Survived ~ PassengerId + Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train_train, family = "binomial",
               na.action = na.omit)
summary(logit_1)
  # significant variables: Pclass, Sex, Age, SibSp
nobs(logit_1)
vif(logit_1) # only multicollinearity in the cabin letters (which is expected)

logit_2 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Embarked, data=train_train, family='binomial')
summary(logit_2)
nobs(logit_2)
vif(logit_2)

logit_3 <- glm(Survived ~ Pclass + Sex + Age + SibSp, data=train_train, family='binomial')
summary(logit_3)
nobs(logit_2)
vif(logit_3)

AIC(logit_1)
AIC(logit_2)
AIC(logit_3)
  # logit_3 seems to be the best fit based off of AIC

# Now I will validate my model #
# Logit Model 1
probs<-as.vector(predict(logit_1,newdata=train_valid, type="response")) # testing logistic model 1
nrow(train_valid)
preds <- rep(0, nrow(train_valid))  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
table(preds, train_valid$Survived)

# Logit Model 2
probs<-as.vector(predict(logit_2,newdata=train_valid, type="response")) # testing logistic model 1
preds <- rep(0, nrow(train_valid))  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
table(preds, train_valid$Survived)

# Logit Model 3
probs<-as.vector(predict(logit_3,newdata=train_valid, type="response")) # testing logistic model 1
preds <- rep(0, nrow(train_valid))  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
table(preds, train_valid$Survived)

# Logit Model 3 did the best job of predicting those who survived, so I will use this model to 
# on the test data to create predictions

# Applying model to test data #
test$Pclass <- as.factor(test$Pclass)
test$Sex <- as.factor(test$Sex)
test$Embarked <- as.factor(test$Embarked)

# Model 3 Predictions #
survive_pred <- predict(logit_3, newdata = data.frame(test))
survive_new_preds <- rep(0, nrow(test))
survive_new_preds[survive_pred>0.5] <- 1 # sets predictions = 1 if their value is above 0.5
Survived <- survive_new_preds
Passengerid <- test$PassengerId
predictions <- tibble(Passengerid, Survived)
write.table(predictions, file='titanic_predictions.csv', row.names=F, sep=',')

# Model 2 Predictions #
survive_pred <- predict(logit_2, newdata = data.frame(test))
survive_new_preds <- rep(0, nrow(test))
survive_new_preds[survive_pred>0.5] <- 1 # sets predictions = 1 if their value is above 0.5
Survived <- survive_new_preds
Passengerid <- test$PassengerId
predictions <- tibble(Passengerid, Survived)
write.table(predictions, file='titanic_new_predictions.csv', row.names=F, sep=',')

# Although AIC is larger for model 2, submitting it into the competition provides higher accuracy, so
# I will submit that model for now.
