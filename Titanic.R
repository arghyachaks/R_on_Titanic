#Load raw data
train <- read.csv("train.csv",header=TRUE)
test <- read.csv("test.csv",header=TRUE)

#Add a Survived variable to the test set to allow for combining data set
test.survived <- data.frame(Survived = rep("None",nrow(test)),test[,])

#Combine Data Sets
data.combined <- rbind(train,test.survived)

#A bit of R data types(e.g factors)
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

#Take a look at gross survival rates
table(data.combined$Survived)

#Distribution accross classes
table(data.combined$Pclass)

#Load up ggplot2 package for visualization
library(ggplot2)

#Hypothesis rich folk survived at a higher rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill=factor(Survived))) + 
  geom_bar(width = 0.5) + 
  xlab("Pclass") + 
  ylab("Total Count") +
  labs(fill="Survived")

#Examine the first few names in the training data set
head(as.character(train$Name))

#How many uniques names are there in the data.combined data set
length(unique(as.character(data.combined$Name)))

#Two duplicate names. Take a closer look
#First get the duplicate names and stores them in a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])

# Next look the record at combine data set
data.combined[which(data.combined$Name %in% dup.names),]


# what is up with 'Miss' and 'Mr' thing
library(stringr)

#Any correlation with other variable(e.g., sibsp)?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]


mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

#Checkout males to see if pattern continues
males <- data.combined[which(train$Sex == "male"),]
males[1:5,]


#Expand upon the relationship between "Survived" and "Pclass" by adding the new "Title" variable
#data set and then explore a potential 3-dimesional relationship.

#Create a utility function to help the extraction
extractTitle <- function(name){
  name <-as.character(name)
  
  if(length(grep("Miss.", name)) > 0){
    return("Miss.")
  }
  else if(length(grep("Mrs.", name)) > 0){
    return("Mrs.")
  }
  else if(length(grep("Master.", name)) > 0){
    return("Master.")
  }
  else if(length(grep("Mr.", name)) > 0){
    return("Mr.")
  }
  else{
    return("Other")
  }
}

titles <- NULL
for(i in 1:nrow(data.combined)){
  titles <-c(titles,extractTitle(data.combined[i,"Name"]))
}

data.combined$title <- as.factor(titles)

#since we have survived level at train data set. only use the first 891 data set.
ggplot(data.combined[1:891,],aes(x=title, fill=Survived)) + 
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pcalss") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

#what is the distribution of female to male accross train and test

table(data.combined$Sex)

# Visualize the three way relationship of sex, pclass, and survival 
# compare to analyse
ggplot(data.combined[1:891,], aes(x=Sex, fill=Survived)) + 
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pcalss") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

# look at the distribution of age over entire data set
summary(data.combined$Age)
summary(data.combined[1:891,"Age"])


# Survival rate broken out by sex, pclass, age
ggplot(data.combined[1:891,], aes(x=Age, fill=Survived)) +
  facet_wrap(~Sex + Pclass) +  
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count") 

#Validate the "Master" is a good proxy for male children
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$Age)

#We know Miss is more complicated. Lets examine
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived !="None",], aes(x=Age, fill=Survived))+
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age of 'Miss. ' by Pclass")+
  xlab("Age")+
  ylab("Total Count")

# Ok, may be female children have differnt survival rate,
# could be a feature engineer later
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))

#Move into sibsp variable, summarize the variable
summary(data.combined$SibSp)

#Can we treat it as a factor
length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

# We beleive title is predictive, visualize survival rates by sibsp,pclass,and title
ggplot(data.combined[1:891,],aes(x=SibSp,fill=Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + title) +
  ggtitle("PClass, Title")+
  xlab("Sibsp") +
  ylab("Total Count") +
  ylim(0,300)+
  labs(fill="Survived")

data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,],aes(x=Parch,fill=Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + title) +
  ggtitle("PClass, Title")+
  xlab("Parch") +
  ylab("Total Count") +
  ylim(0,300)+
  labs(fill="Survived")

#Let's try some feature Engineering. Lets create the family size
temp.Sibsp <- c(train$SibSp,test$SibSp)
temp.Parch <- c(train$Parch,test$Parch)
data.combined$Family.size <- as.factor(temp.Sibsp + temp.Parch + 1)

#Visualize to see if it is predictive
ggplot(data.combined[1:891,],aes(x=Family.size,fill=Survived))+
  geom_bar(width = 1) +
  facet_wrap(~Pclass + title) +
  ggtitle("PClass, Title")+
  xlab("Family.size") +
  ylab("Total Count") +
  ylim(0,300)+
  labs(fill="Survived")

# take a look at the ticket variable
str(data.combined$Ticket)

# Based on the huge number of levels ticket is not a factor variable
# It is a string
# Conver it and display first 20.
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

# There is no immediate apparent structure in the data, lets see if we can find some
# We'll start this by looking the first character each
Ticket.first.char <- ifelse(data.combined$Ticket=="", " ",substr(data.combined$Ticket,1,1))
unique(Ticket.first.char)

# We can make a factor for analysis
data.combined$Ticket.first.char <- as.factor(Ticket.first.char)

# First a high label plot of the data
ggplot(data.combined[1:891,],aes(x=Ticket.first.char,fill=Survived))+
  geom_bar() +
  ggtitle("Survival by ticket first character") + 
  xlab("Ticket.first.char") + 
  ylab("Total Count") + 
  ylim(0,350) +
  labs(fill="Survived")


ggplot(data.combined[1:891,],aes(x=Ticket.first.char,fill=Survived))+
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survival by ticket first character") + 
  xlab("Ticket.first.char") + 
  ylab("Total Count") + 
  ylim(0,350) +
  labs(fill="Survived")

ggplot(data.combined[1:891,],aes(x=Ticket.first.char,fill=Survived))+
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Survival by ticket first character") + 
  xlab("Ticket.first.char") + 
  ylab("Total Count") + 
  ylim(0,350) +
  labs(fill="Survived")

# Next - the fare titanic passenger paid
summary(data.combined$Fare)
length(unique(data.combined$Fare))

# Can't make fare a factor.treat it as anumeric and visualize in histogram
ggplot(data.combined,aes(x=Fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Combined fare distribution") + 
  xlab("Fare") + 
  ylab("Total Count")+
  ylim(0,200)

ggplot(data.combined[1:891,],aes(x=Fare,fill=Survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass + title) +
  ggtitle("Combined fare distribution") + 
  xlab("Fare") + 
  ylab("Total Count")+
  ylim(0,40)

#Analyse the data of cabin variable
str(data.combined$Cabin)

# cabin is not really a factor make it string and display first 100
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

# Replace empty cabin with "U"
data.combined[which(data.combined$Cabin == ""),"Cabin"] <-"U"
data.combined$Cabin[1:100]

#Take a look at the first char as factor
Cabin.first.char <- as.factor(substr(data.combined$Cabin,1,1))
str(Cabin.first.char)
levels(Cabin.first.char)


data.combined$Cabin.first.char <- Cabin.first.char

#High level plot
ggplot(data.combined[1:891,],aes(x=Cabin.first.char,fill=Survived)) +
  geom_bar() +
  ggtitle("Survivability of cabin first character") + 
  xlab("Cabin.first.char") + 
  ylab("Total Count")+
  ylim(0,750) + 
  labs(fill="Survived")

ggplot(data.combined[1:891,],aes(x=Cabin.first.char,fill=Survived)) +
  geom_bar() +
  facet_wrap(~Pclass)+
  ggtitle("Survivability of cabin first character") + 
  xlab("Cabin.first.char") + 
  ylab("Total Count")+
  ylim(0,500) + 
  labs(fill="Survived")

ggplot(data.combined[1:891,],aes(x=Cabin.first.char,fill=Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title)+
  ggtitle("Survivability of cabin first character") + 
  xlab("Cabin.first.char") + 
  ylab("Total Count")+
  ylim(0,500) + 
  labs(fill="Survived")

#what about folks with multiple cabin
data.combined$Cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin," "),"Y","N"))

ggplot(data.combined[1:891,],aes(x=Cabin.multiple,fill=Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title)+
  ggtitle("Survivability of using multiple cabin") + 
  xlab("Cabin.multiple") + 
  ylab("Total Count")+
  ylim(0,350) + 
  labs(fill="Survived")

#Does survivability depends upon where you go on board
str(data.combined$Embarked)
levels(data.combined$Embarked)

ggplot(data.combined[1:891,],aes(x=Embarked,fill=Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title)+
  ggtitle("Survivability of using multiple cabin") + 
  xlab("Embarked") + 
  ylab("Total Count")+
  ylim(0,300) + 
  labs(fill="Survived")

######################################################
#
##### Exploratory Modelling
#
#####################################################

library(randomForest)

#train a random forest with the default parameter using title and pclass
rf.train.1 <- data.combined[1:891, c("Pclass","title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

#train a random forest using Pclass,title,sibsp
rf.train.2 <- data.combined[1:891, c("Pclass","title","SibSp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000 )
rf.2
varImpPlot(rf.2)

#train a random forest using Pclass,title,Parch
rf.train.3 <- data.combined[1:891, c("Pclass","title","Parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000 )
rf.3
varImpPlot(rf.3)

#train a random forest using Pclass,title,SibSp,Parch
rf.train.4 <- data.combined[1:891, c("Pclass","title","SibSp","Parch")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000 )
rf.4
varImpPlot(rf.4)

#train a random forest using Pclass,title,Family.size
rf.train.5 <- data.combined[1:891, c("Pclass","title","Family.size")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000 )
rf.5
varImpPlot(rf.5)

#train a random forest using Pclass,title,SibSp,Family.size
rf.train.6 <- data.combined[1:891, c("Pclass","title","SibSp","Family.size")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000 )
rf.6
varImpPlot(rf.6)

#train a random forest using Pclass,title,Parch,Family.size
rf.train.7 <- data.combined[1:891, c("Pclass","title","Parch","Family.size")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000 )
rf.7
varImpPlot(rf.7)

######################################################
#
##### Cross Validation
#
#####################################################

# Before we jump into feature engineering we need to establish a methodology for
# estimating our error rate on our test set (i,e unseen data). This is critical, 
# for without this we are more likely overfit. Lets start with a submission
# of rf.5 to Keggle to see if our OOB error estimate is accurate

# subset our test record and features
test.submit.df <- data.combined[892:1309, c("Pclass","title","Family.size")]

#make Prediction
rf.5.preds <- predict(rf.5,test.submit.df)
table(rf.5.preds)

# Write out a csv file for submission to kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)

write.csv(submit.df,file = "RF_SUB_20160727.csv", row.names = FALSE)

# Our submission score 0.79426, but the OOB predicts we should scores 0.8159
# Let's look into cross validation using the caret package to see if we can get
# more accurate estimate
library(caret)
library(doSNOW)

# Research has shown that 10-fold CV repeated 10-time is the best place to start
# However there are no hard and fast rule - this is where the experience of the
# Data Scientist(i,e the art) come nto play. We will start with 10 fold CV,
# repeated 10 times and how it goes.

# Leverage caret to create 100 total folds, but ensure that the rate of those
# that servived and perished in each fold matches the overall training set
# This is known as startified cross validation and generally provides better
# results

set.seed(2348)
cv.10.fold <- createMultiFolds(rf.label, k = 10, times = 10)

#check startification
table(rf.label)
342/549

table(rf.label[cv.10.fold[[33]]])
308/494

# Setup caret's train control object per above
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                       index = cv.10.fold)

#Set up doSnow package for multicore training. This will be helpful as we'r going
# to be training lot of trees.
cl <- makeCluster(4,type = "SOCK")
registerDoSNOW(cl)

# Set seed for reproductibility and train
set.seed(34324)
rf.5.cv.1 <- train(x=rf.train.5, y=rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.1)

# Shutdown cluster
stopCluster(cl)

# check out result
rf.5.cv.1

# The above is only slightly more pessimistic than the rf.5 OOB prediction
# but not pessimistic enough. Lets try 5 fold CV repeated 10 times
set.seed(5983)
cv.5.fold <- createMultiFolds(rf.label, k = 5, times = 10)
ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10,
                       index = cv.5.fold)
cl <- makeCluster(4,type = "SOCK")
registerDoSNOW(cl)
set.seed(89472)
rf.5.cv.2 <- train(x=rf.train.5, y=rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.2)

# Shutdown cluster
stopCluster(cl)

# check out result
rf.5.cv.2

# 5 Fold cv is better but not good enough. Move to 3 fold CV repeated 10 times
set.seed(37596)
cv.3.fold <- createMultiFolds(rf.label, k = 3, times = 10)
ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10,
                       index = cv.3.fold)
cl <- makeCluster(4,type = "SOCK")
registerDoSNOW(cl)
set.seed(94622)
rf.5.cv.3 <- train(x=rf.train.5, y=rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.3)

# Shutdown cluster
stopCluster(cl)

# check out result
rf.5.cv.3

# Lets use a single decesion tree to better understand what is going on with our features. Obviously Random Forest
# is more powerful than single decesion tree, but single tree has more adventage to understand easily.

library(rpart)
library(rpart.plot)


# Let's use 3 fold CV repeated 10 times
# Create utility function

rpart.cv <- function(seed,training,labels,ctrl){
  cl <- makeCluster(4,type = "SOCK")
  registerDoSNOW(cl)
  set.seed(seed)
  
  #Leverage interface problem for training
  rpart.cv <- train(x=training, y= labels, method = "rpart", tuneLength=30, trControl=ctrl)
  
  # Shutdown cluster
  stopCluster(cl)
  
  return (rpart.cv)
}


# Grab features
features <- c("Pclass","title","Family.size")
rpart.train.1 <-data.combined[1:891,features]


# Run CV and checkout the result
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1,rf.label,ctrl.3)
rpart.1.cv.1

# Plot
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)


# The plot brings some interesting line of investigation
##  If title is Mr. and Other are predicted to perish at an rate 83.2%
##  If title is Master, Miss, Mrs, and in 1st and 2nd Pclass Survival rate 94.9%
##  If title is Master, Miss, Mrs, and in 3rd class and familily size is 5,6,8,11 are predicted to perish at an rate 100%
## If title is Master, Miss, Mrs, and in 3rd class and familily size is not 5,6,8,11 are predicted to survival rate 60.1%

# Both rpart and rf confirms that title is importann
table(data.combined$title)

# Parse out the last name and title
name.split <- str_split(data.combined$Name, ",")
name.split[1] 
last.name <- sapply(name.split, "[", 1)
last.name[1:10]

data.combined$Last.name <- last.name

# Now for title
name.split <- str_split(sapply(name.split,"[", 2), " ")
titles <- sapply(name.split,"[",2)
unique(titles)

# what's up with title "the"
data.combined[which(titles == "the"),]

#Remap titles to be more exact
titles[titles %in% c("Dona.","the")] <- "Lady."
titles[titles %in% c("Ms.","Mlle")] <- "Miss."
titles[titles %in% c("Jonkheer.","Don.")] <- "Sir."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Col.","Capt.","Major.")] <- "Officer."
table(titles)

# what's up with Dr.
data.combined[which(titles == "Dr."),]

# Make title a factor for visualization
data.combined$New.title <- as.factor(titles)


# Visualize new Version of title
ggplot(data.combined[1:891,],aes(x=New.title,fill=Survived)) +
  geom_bar() +
  facet_wrap(~Pclass)+
  ggtitle("Survivability on the basis of New.title") + 
  xlab("New.title") + 
  ylab("Total Count")+
  labs(fill="Survived")

# Collapses title on basis of visual analytics
indexes <- which(data.combined$New.title == "Lady.")
data.combined$New.title[indexes] <- "Mrs."

indexes <- which(data.combined$New.title == "Rev." |
                   data.combined$New.title == "Sir." |
                  data.combined$New.title == "Officer.")
data.combined$New.title[indexes] <- "Mr."

#table(data.combined$New.title)

indexes <- which(data.combined$New.title == "Dr." & data.combined$Sex == "male")
data.combined$New.title[indexes] <- "Mr."

indexes <- which(data.combined$New.title == "Dr." & data.combined$Sex == "female")
data.combined$New.title[indexes] <- "Mrs."

indexes <- which(data.combined$New.title == "Mlle.")
data.combined$New.title[indexes] <- "Miss."


# Visualize new Version of title after Merging title
ggplot(data.combined[1:891,],aes(x=New.title,fill=Survived)) +
  geom_bar() +
  facet_wrap(~Pclass)+
  ggtitle("Survivability on the basis of New.title") + 
  xlab("New.title") + 
  ylab("Total Count")+
  labs(fill="Survived")

# Grab features
features <-c("Pclass","New.title","Family.size")
rpart.train.2 <- data.combined[1:891,features]

# Run CV and checkout the result
rpart.1.cv.2 <- rpart.cv(94622, rpart.train.2,rf.label,ctrl.3)
rpart.1.cv.2

# Plot
prp(rpart.1.cv.2$finalModel, type = 0, extra = 1, under = TRUE)

# Dive in on ist class Mr.
indexes.first.mr <- which(data.combined$New.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr,]
summary(first.mr.df)

# Lets look at surviving 1st class Mr.
summary(first.mr.df[first.mr.df$Survived == 1,])
View(first.mr.df[first.mr.df$Survived == 1,])

# Take a look at the some high fares
indexes <- which(data.combined$Ticket == "PC 17755" | data.combined$Ticket == "PC 17611" | data.combined$Ticket == "113760")
View(data.combined[indexes,])

# Visualize survival of first class Mr. by fare
ggplot(first.mr.df,aes(x = Fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("1st class Mr. survival by fare")

# Engineer feature based on all the passenger with same ticket number
ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$Ticket)

for(i in 1: length(tickets)){
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$Ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1],"Fare"]/ length(party.indexes)
  
  for(k in 1 : length(party.indexes)){
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

data.combined$Ticket.party.size <- ticket.party.size
data.combined$Avg.fare <- avg.fare

# refresh 1st class Mr. data frame
first.mr.df <- data.combined[indexes.first.mr,]
summary(first.mr.df)

#Visualize new features
ggplot(first.mr.df[first.mr.df$Survived != "None",],aes(x = Ticket.party.size, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("1st class Mr. survival by Ticket.party.size")

ggplot(first.mr.df[first.mr.df$Survived != "None",],aes(x = Avg.fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("1st class Mr. survival by Avg.fare")

#Hypothesis - ticket.party.size is highly corelated wth avg.fare
summary(data.combined$Avg.fare)

# One missing value take a look
data.combined[is.na(data.combined$Avg.fare),]

# Get recrds for similar type of passenger
indexes <- with(data.combined, which(Pclass == "3" & title == "Mr." & Family.size == 1 & Ticket != "3701" ))
similar.no.passenger <- data.combined[indexes,]
summary(similar.no.passenger$Avg.fare)


# use Median because it is close to mean and little higher than mean
data.combined[is.na(data.combined$Avg.fare), "Avg.fare"] <- 7.84


# Take adventage caret's  preprocess function to normalize data
preproc.data.combined <- data.combined[,c("Ticket.party.size", "Avg.fare")]
preProc <- preProcess(preproc.data.combined, method = c("center","scale"))

postProc.data.cmbined <- predict(preProc,preproc.data.combined)

# Hypothesis refuted for all data
cor(postProc.data.cmbined$Ticket.party.size,postProc.data.cmbined$Avg.fare)

# How about just first class all up
indexes <- which(data.combined$Pclass == "1")
cor(postProc.data.cmbined$Ticket.party.size[indexes],postProc.data.cmbined$Avg.fare[indexes])

# Lets see our feature engineering make any differences
features <-c("Pclass","New.title", "Ticket.party.size", "Avg.fare")
rpart.train.3 <- data.combined[1:891,features]

# Run CV and checkout the result
rpart.1.cv.3 <- rpart.cv(94622, rpart.train.3,rf.label,ctrl.3)
rpart.1.cv.3

# Plot
prp(rpart.1.cv.3$finalModel, type = 0, extra = 1, under = TRUE)



##################################################################
##
##                    SUBMITTING SCORE AND SOME ANALYSIS       ###
##
##################################################################

# Submit our test record and features

test.submit.df <- data.combined[892:1309, features]

# Make Prediction
rpart.3.preds <- predict(rpart.1.cv.3$finalModel, test.submit.df, type = "class")
table(rpart.3.preds)

# writeout a csv file for submission
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rpart.3.preds)

write.csv(submit.df, "RPART_SUB20160729_1.csv", row.names = FALSE)


## Random Forest Submission with new features

features <-c("Pclass","New.title", "Ticket.party.size", "Avg.fare")
rf.train.temp <- data.combined[1:891, features]

set.seed(1234)
rf.temp <- randomForest(x = rf.train.temp , y = rf.label, ntree = 1000)
rf.temp

test.submit.df <- data.combined[892:1309,features]

# Make Prediction
rf.preds <- predict(rf.temp, test.submit.df)
table(rf.preds)

## writeout a csv file for submission
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.preds)

write.csv(submit.df, "RF_SUB20160729_1.csv", row.names = FALSE)

## If we want to improve our model a good place to start where it is wrong

# First let's explore our collection of features using mutual information to gain  some
# additional insight. Our institution is that the plot of our tree should align well 
# to the defination of mutual information.
#install.packages("infotheo")

library(infotheo)


mutinformation(rf.label,data.combined$Pclass[1:891])
mutinformation(rf.label,data.combined$Sex[1:891])
mutinformation(rf.label,data.combined$SibSp[1:891])
mutinformation(rf.label,data.combined$Parch[1:891])
mutinformation(rf.label,discretize(data.combined$Fare[1:891]))
mutinformation(rf.label,data.combined$Embarked[1:891])
mutinformation(rf.label,data.combined$title[1:891])
mutinformation(rf.label,data.combined$Family.size[1:891])
mutinformation(rf.label,data.combined$Ticket.first.char[1:891])
mutinformation(rf.label,data.combined$Cabin.multiple[1:891])
mutinformation(rf.label,data.combined$New.title[1:891])
mutinformation(rf.label,data.combined$Ticket.party.size[1:891])
mutinformation(rf.label,discretize(data.combined$Avg.fare[1:891]))

# Now let's use the tsne algorithm to create a 2-D representation of our data. Suitable for visualization
# Let start with Woman and boys.

library(Rtsne)
most.correct <- data.combined[data.combined$New.title != "Mr.",]
indexes <- which(most.correct$Survived != "None")

tsne.1 <- Rtsne(most.correct[,features], check_duplicate = FALSE)
ggplot(NULL,aes(x=tsne.1$Y[indexes,1], y=tsne.1$Y[indexes,2], color = most.correct$Survived[indexes])) +
  geom_point() + labs(color = "Survived") + ggtitle("tsne 2d visualization of features for female and boys")



# To get a baseline, let's use conditional mutual information of tsne X and Y features
# for feamale and boys in the 1st and 2nd class. The intution here is that the combination of these 
# features should be higher than any individual feature we looked at above.

condinformation(most.correct$Survived[indexes], discretize(tsne.1$Y[indexes,]))

# As one more comparison we can leverage conditional mutual information using the top two features in our
# tree plot - New.title and Pclass

condinformation(rf.label, data.combined[1:891, c("New.title", "Pclass")])

# Now looks at the adult male since our model has the biggest potential upside for improving(i,e the tree
# predicts incurrectly for the 86 adult male). Lets visualize with the tsne.

misters <- data.combined[data.combined$New.title == "Mr.",]
indexes <- which(most.correct$Survived != "None")

tsne.2 <- Rtsne(misters[,features], check_duplicate = FALSE)
ggplot(NULL,aes(x=tsne.2$Y[indexes,1], y=tsne.2$Y[indexes,2], color = misters$Survived[indexes])) +
  geom_point() + labs(color = "Survived") + ggtitle("tsne 2d visualization of features for Mr.")

# Now conditional feature for adult male
condinformation(misters$Survived[indexes], discretize(tsne.2$Y[indexes,]))

# How about using tsne features all of the training data and using them in our model
tsne.3 <- Rtsne(data.combined[,features], check_duplicate = FALSE)
ggplot(NULL,aes(x=tsne.3$Y[1:891,1], y=tsne.3$Y[1:891,2], color = data.combined$Survived[1:891])) +
  geom_point() + labs(color = "Survived") + ggtitle("tsne 2d visualization on train data")

# Now conditional mutual information for the training data set
condinformation(data.combined$Survived[1:891],discretize(tsne.3$Y[1:891,]))

# Add the tsne feature in our data frame to use it in model
data.combined$Tsne.x <- tsne.3$Y[,1]
data.combined$Tsne.y <- tsne.3$Y[,2]

# Lets see our mutual information engineering make any differences in our model
features <-c("Pclass","New.title", "Ticket.party.size", "Avg.fare","Tsne.x","Tsne.y")
rpart.train.mutual.1 <- data.combined[1:891,features]

# Run CV and checkout the result
rpart.1.cv.mutual.1 <- rpart.cv(84622, rpart.train.mutual.1,rf.label,ctrl.3)
rpart.1.cv.mutual.1

# Plot
prp(rpart.1.cv.mutual.1$finalModel, type = 0, extra = 1, under = TRUE)


# The previous best value was 0.8371493, now the value is 0.8363636. which is less accurate.













