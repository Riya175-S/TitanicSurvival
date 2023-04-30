df_train <-read.csv("train (2).csv", stringsAsFactors = FALSE)
df_test <-read.csv("test (2).csv", stringsAsFactors = FALSE)
df_train
df_test
ncol(df_train)
ncol(df_test)
names(df_train)
names(df_test)

df_train$Survived

#filling the values of survived column in the training dataset
df_test$Survived <-NA

#now testing for the same
ncol(df_test)
#the output is now showing 12 columns

df_train$IsTrainSet <-TRUE
df_test$IsTrainSet <- FALSE

#performing a vertical joint
df_full <- rbind(df_train, df_test)

tail(df_full)

table(df_full$IsTrainSet)
names(df_full)

table(df_full$Embarked)
#Missing values are found

df_full[df_full$Embarked=='',"Embarked"] <-'S'
table(df_full$Embarked)

table(is.na(df_full$Age))
#there is a lot of missing values
#replacing missing values with median now
age.median <- median(df_full$Age, na.rm = TRUE)
df_full[is.na(df_full$Age), "Age"] <-age.median

table(is.na(df_full$Age)) #no missing values present 

is.na(df_full$Fare)
table(is.na(df_full$Fare))
#only 1 missing value found in fare
median(df_full$Fare, na.rm = TRUE)
fare.median <- median(df_full$Fare, na.rm = TRUE)
df_full[is.na(df_full$Fare), "Fare"] <-fare.median
table(is.na(df_full$Fare))
#All_missing_values_are_now_removed

#categorical casting
str(df_full)
df_full$Pclass <- as.factor(df_full$Pclass)
df_full$Sex <- as.factor(df_full$Sex)
df_full$Embarked <- as.factor(df_full$Embarked)
str(df_full)
df_full$Survived <- as.factor(df_full$Survived)

#splitting data back to train and test but data is cleaned now
df_train <-df_full[df_full$IsTrainSet==TRUE,]
df_test <-df_full[df_full$IsTrainSet==FALSE,]
tail(df_train)
tail(df_test)
str(df_train)

survived.equation <- "Survived~ Pclass+Sex+Age+SibSp + Parch + Fare+Embarked"
survived.formula <-as.formula(survived.equation)
install.packages("randomForest")
library(randomForest)
df.model =randomForest(formula= survived.formula,data=df_train, ntree=500, mtry=3, nodesize=0.01*nrow(df_test))

features.equation <- "Pclass+Sex+Age+SibSp + Parch + Fare+Embarked"
Survived <- predict(df.model, newdata = df_test)
PassengerId <- df_test$PassengerId
Output <- as.data.frame(PassengerId)
Output$Survived <-Survived
tail(Output)
write.csv(Output, file="Submission.csv", row.names=FALSE)

#applying regression
install.packages("gmodels")
library(gmodels)
table(df_train$Survived)

#cor(df_train[c("Pclass","Sex","Age","SibSp","Parch","Fare","Embarked")])
cor(df_train[c("Pclass","Age","SibSp","Parch","Fare")])
install.packages("psych")
summary(df_train)
library(psych)
