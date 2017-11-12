#Load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

#Add a Survived" var to the test set to allow for combining data sets
test.survived <- data.frame(PassengerId = test[,1], Survived = rep("None", nrow(test)), test[,-1])

#identical(train,test.survived)

#Combine data sets
data.combined <- rbind(train,test.survived)
  
#A bit about R data types (e.g factors)
#str(data.combined)

#change some class into factor class
data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

#Take a look at gross survival rates
table(data.combined$Survived)

# Dis across classes
table(data.combined$Pclass)

#Load up ddplot2 package to use for visualization
library(ggplot2)

#Hypothesis - Rich folks survived at higer rate
train$Pclass <- as.factor(train$Pclass)

ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
#  geom_histogram(binwidth = 0.5) + #nie dzialajaca wersja z tutorialu
  stat_count(width = 0.5) +
#  geom_bar() + #no bandwith customization
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

#Examin
head(as.character(train$Name))

#How many unique names are there acress both train and test?
#length(unique(as.character(data.combined$Name)))

#line of code above gives us two duplicates 

#first, get the duplicate names and store them as vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])


#Next, take look at the recoerds in the combined data set
data.combined[which(data.combined$Name %in% dup.names),]
# %in% oznacza ze sa wewnetrz

#Whats up with miss and mr thing
library(stringr)

# Any correlation with other variables (e.q subsp)?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

# Any correlation with other variables (e.q subsp)?
male <- data.combined[which(train$Sex =="male"),]
male[1:5,]

#Expand upon the relationship between 'Survived' and 'Pclass' by adding the new 'Title' variable to the dataset
#data set and then explore a potential 3-demensional relationship

#Create a utility function to help with title extraction
extractTitle <- function(name) {
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) > 0) {
    return("Miss.")
  }else if (length(grep("Master.", name)) > 0) {
    return("Master.")
  }else if (length(grep("Mrs.", name)) > 0) {
    return("Mrs.") 
  }else if (length(grep("Mr.", name)) > 0){
    return("Mr.")
  }else {
    return("Other")
  }
}


titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"Name"]))
  #"name" would not work because column is called "Name"
}
data.combined$title <- as.factor(titles)  

#Since we only have survived labes for the train set, only use the first 891 rows
ggplot(data.combined[1:891,], aes(x=title,fill = Survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

#dist of sex
table(data.combined$Sex)

#sex graph
ggplot(data.combined[1:891,], aes(x=Sex,fill = Survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

#dist of age
summary(data.combined$Age)
summary(data.combined[1:891,]$Age)

#age,pclass,sex graph 
ggplot(data.combined[1:891,], aes(x=Age,fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  #stat_count(width = 5) +
  #geom_bar(aes(fill = Survived)) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")
#  labs(fill = "Survived")

#valideate that master is a good proxy for male child
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$Age)

#what about "Miss"
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$Age)

#miss plot by title
ggplot(misses[misses$Survived != "None",], aes(x=Age,fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.'") +
  xlab("Age") +
  ylab("Total Count") 

#fem children have diff sur rate
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch ==0),]
summary(misses.alone$Age)
length(which(misses.alone$Age<= 14.5))

summary(data.combined$SibSp)

length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

# belived that title is predictive. Visualie survival reates by sipbsp, pclass and title
ggplot(data.combined[1:891,], aes(x=SibSp, fill = Survived)) +
  facet_wrap(~Pclass + title) +
  #geom_histogram(binwidth = 1) +
  stat_count(width = 0.5) +
  ggtitle("SibSp with classes") +
  xlab("Amout of siblings or spauses on ship") +
  ylab("Total Count") +
  ylim(0,300) 
#  labs(fill = "Survived")


data.combined$Parch <- as.factor(data.combined$Parch)

# belived that title is predictive. Visualie survival reates by sipbsp, pclass and title
ggplot(data.combined[1:891,], aes(x=Parch, fill = Survived)) +
  facet_wrap(~Pclass + title) +
  #geom_histogram(binwidth = 1) +
  stat_count(width = 0.5) +
  ggtitle("Parch with classes") +
  xlab("Amout of parents or children on ship") +
  ylab("Total Count") +
  ylim(0,300) 
#  labs(fill = "Survived")


#lets try some feature engineering what aobut creating a family size feature
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch +1)

#family size
ggplot(data.combined[1:891,], aes(x=family.size, fill = Survived)) +
  #geom_histogram(binwidth = 1) +
  facet_wrap(~Pclass + title) +
  stat_count(width = 0.5) +
  ggtitle("family size") +
  xlab("family size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

#take a look at tje ticket variable
str(data.combined$Ticket)

data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

#There is no immediately apparent structure in the data, lets see if we can find some.
# Well start with taking a look at just tje forst char for each
Ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket,1,1))
unique(Ticket.first.char)

# factorize 1st letter of ticets
data.combined$Ticket.first.char <- as.factor(Ticket.first.char)


ggplot(data.combined[1:891,], aes(x=Ticket.first.char, fill = Survived)) +
  geom_bar() +
  #stat_count(width = 0.5) +
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill= "Survived")


ggplot(data.combined[1:891,], aes(x=Ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  #stat_count(width = 0.5) +
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill= "Survived")

ggplot(data.combined[1:891,], aes(x=Ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  #stat_count(width = 0.5) +
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill= "Survived")
  
# nest up the fares titanic passangers paid
summary(data.combined$Fare)
length(unique(data.combined$Fare))



ggplot(data.combined, aes(x= Fare)) +
  geom_histogram(binwidth = 5) +
  #stat_count(width = 0.5) +
  ggtitle("Comb fare dist") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,450) 


ggplot(data.combined[1:891,], aes(x= Fare, fill = Survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass + title) +
  #stat_count(width = 0.5) +
  ggtitle("Comb , fare dist") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,50) + 
  labs(fill = "Survived")


#anas of cab var
str(data.combined$Cabin)
  
#Cab is not a factor make it into string and disp 100
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]


#replace empty cab with "U"
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]



#Take fist char as just the first char as factor
cabin.first.char <- as.factor(substr(data.combined$Cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)

#add to combined data set plot
data.combined$cabin.first.char <- cabin.first.char


ggplot(data.combined[1:891,], aes(x= cabin.first.char, fill = Survived)) +
  geom_bar() +
  #facet_wrap(~Pclass) +
  #stat_count(width = 0.5) +
  ggtitle("sur on cabs") +
  xlab("cab first char") +
  ylab("Total Count") +
  ylim(0,750) + 
  labs(fill = "Survived")


ggplot(data.combined[1:891,], aes(x= cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  #stat_count(width = 0.5) +
  ggtitle("sur on cabs") +
  xlab("cab first char") +
  ylab("Total Count") +
  ylim(0,350) + 
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x= cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  #stat_count(width = 0.5) +
  ggtitle("sur on cabs") +
  xlab("cab first char") +
  ylab("Total Count") +
  ylim(0,500) + 
  labs(fill = "Survived")

# what about miltiple cabs
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))


ggplot(data.combined[1:891,], aes(x= cabin.multiple, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  #stat_count(width = 0.5) +
  ggtitle("Pclass, title") +
  xlab("cab mult") +
  ylab("Total Count") +
  ylim(0,350) + 
  labs(fill = "Survived")


#does it matter where you got onboard Titanic
str(data.combined$Embarked)
levels(data.combined$Embarked)

ggplot(data.combined[1:891,], aes(x= Embarked, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  #stat_count(width = 0.5) +
  ggtitle("Pclass, title") +
  xlab("cab mult") +
  ylab("Total Count") +
  ylim(0,350) + 
  labs(fill = "Survived")

# Train a Random Forest with the default parameterters using pclass and title
rf.train.1 <- data.combined[1:891, c("Pclass","title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)


rf.train.2 <- data.combined[1:891, c("Pclass","title" , "SibSp")]


set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)


rf.train.3 <- data.combined[1:891, c("Pclass","title" , "Parch")]


set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)



rf.train.4 <- data.combined[1:891, c("Pclass","title"  , "SibSp", "Parch")]


set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)


rf.train.5 <- data.combined[1:891, c("Pclass","title" , "family.size")]


set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)


rf.train.6 <- data.combined[1:891, c("Pclass","title" ,"SibSp", "family.size")]


set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)



rf.train.7 <- data.combined[1:891, c("Pclass","title" , "Parch","family.size")]


set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)

#Subset our test records and features
test.submit.df <- data.combined[892:1309, c("Pclass", "title", "family.size")]

#Make a predictions
rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)


#Write out a CSV file for subbmission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)

write.csv(submit.df, file = "RF_SUB_20170905_1.cvs",row.names = FALSE)



library(caret)
library(doSNOW)

set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k =10, times = 10)

#Check stratification
table(rf.label)
342/549

table(rf.label[cv.10.folds[[33]]])
308/494

ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.10.folds)


install.packages('e1071', dependencies=TRUE)

#doMC works only on Linux and Mac
cl <- makeCluster(6,type = "SOCK")
registerDoSNOW(cl)

#Set seed for reproducibility and train
set.seed(343)

rf.5.cv.1 <- train(x = rf.train.5 , y = rf.label, method = "rf", tuneLenght = 3, ntree = 1000, trControl = ctrl.1)
                   
#Shutdown cluster
stopCluster(cl)


#Check out results
rf.5.cv.1

#10 fold validation predicts better results than reality. Using smaller part of our data can help with that.
#With 5 fold ... we use less data to train so we have more data to check on it.
#5(parameter "k" in trainControl) fold isnt better 
#using same proportions of learn and train data for crossvalidation as in extrenal test gives best results 

library(rpart)
library(rpart.plot)


set.seed(2348)
cv.3.folds <- createMultiFolds(rf.label, k =3, times = 10)

ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = cv.3.folds)

cl <- makeCluster(6,type = "SOCK")
registerDoSNOW(cl)

#Set seed for reproducibility and train
set.seed(343)

rf.5.cv.3 <- train(x = rf.train.5 , y = rf.label, method = "rf", tuneLenght = 3, ntree = 1000, trControl = ctrl.3)

#Shutdown cluster
stopCluster(cl)


#Check out results
rf.5.cv.3




# Install and load packages
#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Per video #5, let's use 3-fold CV repeated 10 times 

# Create utility function
rpart.cv <- function(seed, training, labels, ctrl) {
  cl <- makeCluster(6, type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  # Leverage formula interface for training
  rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30, 
                    trControl = ctrl)
  
  #Shutdown cluster
  stopCluster(cl)
  
  return (rpart.cv)
}

# Grab features
features <- c("Pclass", "title", "family.size")
rpart.train.1 <- data.combined[1:891, features]

# Run CV and check out results
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1

# Plot
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)
rpart.1.cv.1$finalModel


library("rplos")
library("httr")
library("stringr")

table(data.combined$title)

data.combined[1:25, "name"]

name.splits <- str_split(data.combined$Name, ",")
name.splits[1]
last.names <- sapply(name.splits, "[", 1)
last.names[1:10]

#Add last names to dataframe
data.combined$last.name <- last.names

#Now for titles
name.splits <- str_split(sapply(name.splits, "[", 2), " ")
titles <- sapply(name.splits, "[", 2)
unique(titles)

#whats up with "the"
data.combined[which(titles == "the"),]

# Re-map titles to be more exact
titles[titles %in% c("Dona.", "the")] <- "Lady."
titles[titles %in% c("Ms.", "Mlle.")] <- "Miss."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Capt.", "Major.")] <- "Officer"
table(titles)

# Make title a factor
data.combined$new.title <- as.factor(titles)

# Visualize new version of title
ggplot(data.combined[1:891,], aes(x = new.title, fill = survived)) +
  geom_bar() +
  facet_wrap(~ pclass) + 
  ggtitle("Surival Rates for new.title by pclass")


# Collapse titles based on visual analysis
indexes <- which(data.combined$new.title == "Lady.")
data.combined$new.title[indexes] <- "Mrs."

indexes <- which(data.combined$new.title == "Dr." | 
                   data.combined$new.title == "Rev." |
                   data.combined$new.title == "Sir." |
                   data.combined$new.title == "Officer")
data.combined$new.title[indexes] <- "Mr."

# Visualize 
ggplot(data.combined[1:891,], aes(x = new.title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass) +
  ggtitle("Surival Rates for Collapsed new.title by pclass")

features <- c("Pclass", "new.title", "family.size")
rpart.train.2 <- data.combined[1:891, features]


#run cv
rpart.2.cv.1 <- rpart.cv(94622, rpart.train.2, rf.label, ctrl.3)
rpart.2.cv.1


#plot
prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)



#dive in first class mister
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)

#one female?
first.mr.df[first.mr.df$Sex == "female",]

#update new title
indexes <- which(data.combined$new.title == "Mr." &
                   data.combined$Sex == "female")
data.combined$new.title[indexes] <- "Mrs."

#check for gender slip-ups
length(which(data.combined$Sex == "female"&
               (data.combined$new.title == "Master." |
                  data.combined$new.title == "Mr.")))

#refresh data frame
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]

#lets look at surviving 1st class "Mr."
Summary(first.mr.df[first.mr.df$Survived == "1",])
View(first.mr.df[first.mr.df$Survived == "1",])



#take a look at some high fares
indexes <- which(data.combined$Ticket == "PC 17755" |
                   data.combined$Ticket == "PC 17611" |
                   data.combined$Ticket == "113760" )
View(data.combined[indexes,])


#high fare survival rate with "Mr."
ggplot(first.mr.df, aes(x = Fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("1st Class 'Mr.' Survival raates by fare")



#Engineer futures based on all the passengers with te same ticket
ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$Ticket)

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$Ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1], "Fare"] / length(party.indexes)
  
  for (k in 1:length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- avg.fare



#refresh 1st class "Mr." data
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)



ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x = ticket.party.size, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by ticket.party.size")


ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x = avg.fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by avg.fare")

#hypothesis - ticket.party.size is higly correlated with avg.fare
summary(data.combined$avg.fare)

# one missing value take a look
data.combined[is.na(data.combined$avg.fare), ]


# Get records for similar passengers and summarize avg.fares
indexes <- with(data.combined, which(Pclass == "3" & title == "Mr." & family.size == 1 &
                                       Ticket != "3701"))
similar.na.passengers <- data.combined[indexes,]
summary(similar.na.passengers$avg.fare)


# Use median since close to mean and a little higher than mean
data.combined[is.na(avg.fare), "avg.fare"] <- 7.840

#leverage carets preProcess function to normalized data
preproc.data.combined <- data.combined[, c("ticket.party.size", "avg.fare")]
preProc <- preProcess(preproc.data.combined, method = c("center", "scale"))

postproc.data.combined <- predict(preProc, preproc.data.combined)


#hypothesis refuted for all data (testing correlation -1 negetive coralated +1 positive corralated 0 no corealtion)
cor(postproc.data.combined$ticket.party.size, postproc.data.combined$avg.fare)

#how about for just 1st class all-up?
indexes <- which(data.combined$Pclass == "1")
cor(postproc.data.combined$ticket.party.size[indexes],
    postproc.data.combined$avg.fare[indexes])
#hypothesis refuted again


#ok, lets see if our feature engineering has made any difference
features <- c("Pclass", "new.title","family.size", "ticket.party.size","avg.fare")
rpart.train.3 <- data.combined[1:891, features]


#run CV and check out results
rpart.3.cv.1 <- rpart.cv(94622, rpart.train.3, rf.label, ctrl.3)
rpart.3.cv.1


#plot
prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = TRUE)



#
# Rpart scores 0.80383
#
# Subset our test records and features
test.submit.df <- data.combined[892:1309, features]

# Make predictions
rpart.3.preds <- predict(rpart.3.cv.1$finalModel, test.submit.df, type = "class")
table(rpart.3.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rpart.3.preds)

write.csv(submit.df, file = "RPART_SUB_20160619_1.csv", row.names = FALSE)


#
# Random forest scores 0.80861
#
features <- c("Pclass", "new.title", "ticket.party.size", "avg.fare")
rf.train.temp <- data.combined[1:891, features]

library(randomForest)

set.seed(1234)
rf.temp <- randomForest(x = rf.train.temp, y = rf.label, ntree = 1000)
rf.temp


test.submit.df <- data.combined[892:1309, features]

# Make predictions
rf.preds <- predict(rf.temp, test.submit.df)
table(rf.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.preds)

write.csv(submit.df, file = "RF_SUB_20160619_1.csv", row.names = FALSE)



#
# to improve our mode focus on where it gets things wrong!



# First lets explore our collection of features using mutual information to
# gain some additional insight. Our intuition is that the plot of our tree
# should align well to the definition of mutual information.
#install.packages("infotheo")
library(infotheo)

mutinformation(rf.label, data.combined$Pclass[1:891])
mutinformation(rf.label, data.combined$Sex[1:891])
mutinformation(rf.label, data.combined$SibSp[1:891])
mutinformation(rf.label, data.combined$Parch[1:891])
mutinformation(rf.label, discretize(data.combined$Fare[1:891]))
mutinformation(rf.label, data.combined$Embarked[1:891])
mutinformation(rf.label, data.combined$title[1:891])
mutinformation(rf.label, data.combined$family.size[1:891])
mutinformation(rf.label, data.combined$Ticket.first.char[1:891])
mutinformation(rf.label, data.combined$cabin.multiple[1:891])
mutinformation(rf.label, data.combined$new.title[1:891])
mutinformation(rf.label, data.combined$ticket.party.size[1:891])
mutinformation(rf.label, discretize(data.combined$avg.fare[1:891]))





# OK now lets leverage the tsne algorithm to create a 2-D representation of our data 
# suitable for visualization starting with folks our model gets right very often - folks
# with titles other than "Mr."
#install.packages("Rtsne")
library(Rtsne)
most.correct <- data.combined[data.combined$new.title != "Mr.",]
indexes <- which(most.correct$Survived != "None")



set.seed(984357)
tsne.1 <- Rtsne(most.correct[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.1$Y[indexes, 1], y = tsne.1$Y[indexes, 2], 
                 color = most.correct$Survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for new.title Other than 'Mr.'")





# To get a baseline lets use conditional mutual information on the tsne X and
# Y features for females and boys in 1st and 2nd class. The intuition here is that
# the combination of these features should be higher than any individual feature
# we looked at above
condinformation(most.correct$Survived[indexes], discretize(tsne.1$Y[indexes,]))


# As one more comparison, we can leverage conditional mutual information using
# the top two features used in our tree plot - new.title and pclass
condinformation(rf.label, data.combined[1:891, c("new.title", "Pclass")])


# OK, now lets take a look at adult males since our model has the biggest 
# potential upside for improving
misters <- data.combined[data.combined$new.title == "Mr.",]
indexes <- which(misters$Survived != "None")

tsne.2 <- Rtsne(misters[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.2$Y[indexes, 1], y = tsne.2$Y[indexes, 2], 
                 color = misters$Survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for new.title of 'Mr.'")




# Now conditional mutual information for tsne features for adult males
condinformation(misters$Survived[indexes], discretize(tsne.2$Y[indexes,]))


#
# Idea - How about creating tsne featues for all of the training data and
# using them in our model?
#
tsne.3 <- Rtsne(data.combined[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.3$Y[1:891, 1], y = tsne.3$Y[1:891, 2], 
                 color = data.combined$Survived[1:891])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for all Training Data")

# Now conditional mutual information for tsne features for all training
condinformation(data.combined$Survived[1:891], discretize(tsne.3$Y[1:891,]))

# Add the tsne features to our data frame for use in model building
data.combined$tsne.x <- tsne.3$Y[,1]
data.combined$tsne.y <- tsne.3$Y[,2]















features <- c("Pclass", "new.title", "ticket.party.size", "avg.fare", "tsne.x", "tsne.y")
rf.train.temp <- data.combined[1:891, features]

library(randomForest)

set.seed(1234)
rf.temp <- randomForest(x = rf.train.temp, y = rf.label, ntree = 1000)
rf.temp


test.submit.df <- data.combined[892:1309, features]

# Make predictions
rf.preds <- predict(rf.temp, test.submit.df)
table(rf.preds)



# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.preds)

write.csv(submit.df, file = "RF_SUB_20160619_with_tsne.csv", row.names = FALSE)



# Grab features
features <- c("Pclass", "new.title", "ticket.party.size", "avg.fare", "tsne.x", "tsne.y")
rpart.train.6 <- data.combined[1:891, features]

# Run CV and check out results
rpart.1.cv.6 <- rpart.cv(94622, rpart.train.6, rf.label, ctrl.3)
rpart.1.cv.6

# Plot
prp(rpart.1.cv.6$finalModel, type = 0, extra = 1, under = TRUE)
rpart.1.cv.6$finalModel




# Grab features
features <- c("Pclass", "new.title", "ticket.party.size", "avg.fare", "tsne.y")
rpart.train.6 <- data.combined[1:891, features]

# Run CV and check out results
rpart.1.cv.6 <- rpart.cv(94622, rpart.train.6, rf.label, ctrl.3)
rpart.1.cv.6

# Plot
prp(rpart.1.cv.6$finalModel, type = 0, extra = 1, under = TRUE)
rpart.1.cv.6$finalModel






data.combined.male.age<- which(data.combined[1:891,]$new.title == "Mr.")

summary(data.combined.male.age$Age)
summary(data.combined.male.age$Age)




first.try <- data.combined[data.combined.male.age,]


summary(first.try$Age)



#age,pclass,sex graph 
ggplot(first.try, aes(x=Age,fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  #stat_count(width = 5) +
  #geom_bar(aes(fill = Survived)) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")
#  labs(fill = "Survived")



