#installing necessary packeges
library(tidyverse)
library(dplyr)

#importing the dataset
library(readxl)
heart_2020_cleaned <- read.csv("C:/Users/USER/Desktop/heart_2020_cleaned.csv")
View(heart_2020_cleaned)

head(heart_2020_cleaned)

summary(heart_2020_cleaned)
glimpse(heart_2020_cleaned)

#checking for missing values

paste("Number of missing values:",sum(is.na(heart_2020_cleaned))) #Check missing values
colnames(heart_2020_cleaned)

#Data Cleaning for ‘HeartDisease’,‘Smoking’,‘AlcoholDrinking’,‘Stroke’,‘PhysicalHealth’,‘MentalHealth’,‘DiffWalking,’Sex’

heart_2020_cleaned$BMI<-as.numeric(heart_2020_cleaned$BMI)
heart_2020_cleaned<-filter(heart_2020_cleaned, BMI<=70) #The dataset now only contains BMI of between 0 to 70

#converting character data to factor
heart_2020_cleaned["BMI"] = cut(heart_2020_cleaned$BMI, c(0, 18.5, 25, 30 ,Inf), c("Underweight", "Healthy",  "Overweight", "Obese"), include.lowest=TRUE)

summary(heart_2020_cleaned)

#Data Cleaning for ‘AgeCategory’,‘Race’,‘Diabetic’,‘PhysicalActivity’,‘GenHealth’,‘SleepTime’,‘Asthma’,‘KidneyDisease’,‘SkinCancer’

heart_2020_cleaned$AgeCategory <-as.factor(heart_2020_cleaned$AgeCategory)#converting character data to factor
heart_2020_cleaned$Race <- as.factor(heart_2020_cleaned$Race)#converting character data to factor

colnames(heart_2020_cleaned)[which(names(heart_2020_cleaned)=="GenHealth")]<-"GeneralHealth" # Check variables
heart_2020_cleaned$GeneralHealth <- factor(heart_2020_cleaned$GeneralHealth, levels=c('Poor','Fair','Good','Very good','Excellent'), ordered = TRUE) #converting character data to factor

overall_count<-count(heart_2020_cleaned)
sleeptime_count<-count(filter(heart_2020_cleaned, (SleepTime>20 | SleepTime<3))) #Total SleepTime that are less than 3 hours and more than 20 hours
round((sleeptime_count/overall_count*100),2) #SleepTime that are less than 3 hours and more than 20 hours only contributed 0.43% to the entire dataset

heart_2020_cleaned$SleepTime<-as.numeric(heart_2020_cleaned$SleepTime)#Convert character to numeric
heart_2020_cleaned<-filter(heart_2020_cleaned, SleepTime>3)
heart_2020_cleaned<-filter(heart_2020_cleaned, SleepTime<=20) #The dataset now only contains SleepTime of between 3 to 20 hours

head(heart_2020_cleaned)

#Exploratory Analysis of the Dataset

# Pivot data longer
heart_data_long <- pivot_longer(heart_data, cols = everything(), names_to = "variable", values_to = "value")


heart_data_long <- pivot_longer(heart_data, cols = everything(), names_to = "variable", values_to = "value")
ggplot(heart_data_long, aes(x = value)) + 
  geom_bar(fill = "green") + 
  facet_wrap(~ variable, scales = 'free_x') +
  theme_minimal()

#histogram plot for numerical variables

heart_data_long <- heart_2020_cleaned %>%
  select(BMI, PhysicalHealth, MentalHealth, SleepTime) %>%
  mutate(BMI = as.numeric(as.character(BMI))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")


ggplot(heart_data_long, aes(x = value)) + 
  geom_histogram(bins = 10, fill = "brown") + 
  facet_wrap(~variable, scales = 'free_x') +
  theme_minimal()

summary(heart_2020_cleaned)
# Changing the data types for the all columns
heart_2020_cleaned$HeartDisease <- ifelse(heart_2020_cleaned$HeartDisease=="Yes", 1, 0)
heart_2020_cleaned$BMI <-as.numeric(heart_2020_cleaned$BMI)
heart_2020_cleaned$Smoking <- ifelse(heart_2020_cleaned$Smoking=="Yes", 1, 0)
heart_2020_cleaned$AlcoholDrinking <- ifelse(heart_2020_cleaned$AlcoholDrinking=="Yes", 1, 0)
heart_2020_cleaned$Stroke <- ifelse(heart_2020_cleaned$Stroke=="Yes", 1, 0)
heart_2020_cleaned$PhysicalHealth<-as.numeric(heart_2020_cleaned$PhysicalHealth) 
heart_2020_cleaned$MentalHealth <-as.numeric(heart_2020_cleaned$MentalHealth) 
heart_2020_cleaned$DiffWalking <- ifelse(heart_2020_cleaned$DiffWalking=="Yes", 1, 0)
heart_2020_cleaned$Sex <- ifelse(heart_2020_cleaned$Sex=="Male", 1, 0)
heart_2020_cleaned$AgeCategory <- as.numeric(heart_2020_cleaned$AgeCategory)
heart_2020_cleaned$Race <- as.numeric(heart_2020_cleaned$Race)
heart_2020_cleaned$Diabetic<-replace(heart_2020_cleaned$Diabetic, heart_2020_cleaned$Diabetic=="Yes (during pregnancy)", "Yes")
heart_2020_cleaned$Diabetic<-replace(heart_2020_cleaned$Diabetic, heart_2020_cleaned$Diabetic=="No, borderline diabetes", "Yes")
heart_2020_cleaned$Diabetic<- ifelse(heart_2020_cleaned$Diabetic=="Yes", 1, 0)
heart_2020_cleaned$PhysicalActivity <- ifelse(heart_2020_cleaned$PhysicalActivity=="Yes", 1, 0)
heart_2020_cleaned$GeneralHealth <-as.numeric(heart_2020_cleaned$GeneralHealth)
heart_2020_cleaned$Asthma <- ifelse(heart_2020_cleaned$Asthma=="Yes", 1, 0)
heart_2020_cleaned$KidneyDisease <- ifelse(heart_2020_cleaned$KidneyDisease=="Yes", 1, 0)
heart_2020_cleaned$SkinCancer <- ifelse(heart_2020_cleaned$SkinCancer=="Yes", 1, 0)
library(dplyr)

glimpse(heart_2020_cleaned)

heartdf <- heart_2020_cleaned %>%
  mutate_all(as.numeric)

Heart_CorMat <- cor(heartdf, method = "spearman")

glimpse(heartdf)

#Using corrplot library to plot the correlation matrix in R


library(corrplot)


# Select numeric columns from the heartdf data frame
numeric_cols <- sapply(heartdf, is.numeric)
numeric_data <- heartdf[, numeric_cols]

# Compute the correlation matrix
cor_matrix <- cor(numeric_data, method = "spearman")

# Load the corrplot package
library(corrplot)

# Visualize the correlation matrix
corrplot(cor_matrix)


#checking if any variables are strongly correlated
sum(Heart_CorMat$correlations[,] > 0.7 , na.rm=TRUE)
sum(Heart_CorMat$correlations[,] < -0.7, na.rm=TRUE)


#Step-wise Regression

base.mod <- lm(HeartDisease ~ 1 , data= heart_2020_cleaned)  # base intercept only model
all.mod <- lm(HeartDisease ~ . , data= heart_2020_cleaned) # full model with all predictors
stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 0, steps = 1000)  # perform step-wise algorithm
shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  # remove intercept 
print(shortlistedVars)

shortlistedVars<-shortlistedVars[1:10] # get list of variables
shortlistedVars


#Loading the modelling packages

library(caret)

#ROSE - Random Over Sampling to handle imbalance classes

install.packages("ROSE")

library(ROSE)

#Create possibly balanced samples by random under-sampling using ovun.sample

balanced_sample<-NULL
tmp<-ovun.sample(HeartDisease ~ ., data = heart_2020_cleaned, method = "under", p = 0.5, seed = 5)$data
balanced_sample<-rbind(balanced_sample, tmp)

tmp <- ovun.sample(HeartDisease ~ ., data = heart_2020_cleaned, method = "under", p = 0.7, seed = 5)$data
library(caret)

balanced_sample <- downSample(x = heart_2020_cleaned[, -1], y = heart_2020_cleaned$HeartDisease, method = "under")

glimpse(balanced_sample)

summary(balanced_sample$HeartDisease)

#Partitioning the dataset
#Set partition for train and test data

set.seed(123)
training.samples <- balanced_sample$HeartDisease %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- balanced_sample[training.samples, ]
test.data <- balanced_sample[-training.samples, ]

# Logistics Regression
model  <- glm(HeartDisease ~. , data = train.data[c("HeartDisease",shortlistedVars)], family = binomial)
summary(model)

#Predicting probabilities
probabilities <- model %>% predict(test.data, type = "response")
head(probabilities)

summary(probabilities)

predicted.classes <- ifelse(probabilities > 0.5, "1", "0")#If the Probabilities is greater than 0.5, output 1, otherwise output 0

confusion_mtx = table(test.data[, 1], predicted.classes)
confusionMatrix(confusion_mtx)

library(ggplot2)

#Using randomForest for the evaluation

install.packages("randomForest")
library(randomForest)

train.data$HeartDisease<-as.character(train.data$HeartDisease) #converting numeric train data to character
train.data$HeartDisease<-as.factor(train.data$HeartDisease) #converting numeric train data to factor

classifier_RF <- randomForest(x = train.data[shortlistedVars], 
                              y = train.data$HeartDisease,
                              ntree = 500,
                              importance=TRUE)
classifier_RF

y_pred = predict(classifier_RF, newdata = test.data[-1])
# Confusion Matrix
confusion_mtx = table(test.data[, 1], y_pred)
confusionMatrix(confusion_mtx)

# Plotting model
plot(classifier_RF)

# Importance plot
importance(classifier_RF)

# Variable importance plot
varImpPlot(classifier_RF)



base.mod <- lm(HeartDisease ~ 1 , data= heart_2020_cleaned)  # base intercept only model
all.mod <- lm(HeartDisease ~ . , data= heart_2020_cleaned) # full model with all predictors
stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 0, steps = 1000)  # perform step-wise algorithm
shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  # remove intercept 
print(shortlistedVars)


balanced_sample<-NULL
tmp<-ovun.sample(HeartDisease ~ ., data = heart_2020_cleaned, method = "under", p = 0.5, seed = 5)$data
balanced_sample<-rbind(balanced_sample, tmp)

glimpse(balanced_sample)
summary(balanced_sample$HeartDisease)
set.seed(123)
training.samples <- balanced_sample$HeartDisease %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- balanced_sample[training.samples, ]
test.data <- balanced_sample[-training.samples, ]
model  <- glm(HeartDisease ~. , data = train.data[c("HeartDisease",shortlistedVars)], family = binomial)
summary(model)

ggplot(df, aes(x = Probabilities, y = Actual)) + 
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic Regression Model") +
  xlab("Predicted Probabilities") +
  ylab("Actual Values")
