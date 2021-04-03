# Titanic Data: Main
# BUSA3020 Advanced Analytics Techniques
# Student Name: Cyrus Kwan
# Student ID: 45200165
# Last Modified: 3/04/2021
# Accessible via: https://github.com/MQCyrusKwan/BUSA3020-Assessment-2---Predictive-Analysis

# ------------------------------------------------------------------------------------------|
# DATA PREPARATION:

# Install package manager
if(find.package("pacman") == FALSE){
    install.packages("pacman")
}

# Load packages
# Load with conformation: require()
# Load without conformaiton: library()
require(pacman)
pacman::p_load(pacman, dplyr, ggplot2, VIM, randomForest, nnet, neuralnet, pROC)

# Source functions from other R scripts
source(file = "Titanic_Analysis.r")
source(file = "Titanic_Features.r")
source(file = "Titanic_Algorithms_Testing.r")

# Data from: https://ilearn.mq.edu.au/mod/resource/view.php?id=6351154
titanic_data <- import_data(file_path = "TitanicData_AllPassengers.csv")

# ------------------------------------------------------------------------------------------|
# DESCRIPTIVE ANALYSIS:
head(titanic_data)
str(titanic_data)
summary(titanic_data)

# ------------------------------------------------------------------------------------------|
# DATA TREATMENT:

# Creates new dataframe that uses KNN imputation
# Fills out "Age", "Passenger Fare", and "Port of Embarkation" columns
titanic_cleaned <- kNN(
    data = titanic_data, 
    variable = c("Age", "Passenger.Fare", "Port.of.Embarkation"))
titanic_cleaned <- subset(titanic_cleaned, 
                          select = -c(Age_imp, 
                                      Passenger.Fare_imp,
                                      Port.of.Embarkation_imp))

# Extract title
titanic_cleaned$Title <- extract_title(titanic_cleaned$Name)

# > uncommon titles are summed into 'Rare'
titanic_cleaned$Normal.Title <- titanic_cleaned$Title
titanic_cleaned$Normal.Title[titanic_cleaned$Normal.Title != 'Mr' & 
                             titanic_cleaned$Normal.Title != 'Miss' & 
                             titanic_cleaned$Normal.Title != 'Mrs' & 
                             titanic_cleaned$Normal.Title != 'Master'] <- 'Rare'

# Family size
titanic_cleaned$Family.Size <- 
    titanic_cleaned$No.of.Siblings.or.Spouses.on.Board + 
    titanic_cleaned$No.of.Parents.or.Children.on.Board

# Creates new column containing the amount of cabins the passenger had
titanic_cleaned$nCabins <- list_sum_substr(titanic_data$Cabin)

# ------------------------------------------------------------------------------------------|
# ANALYSIS:

## COMPARING ORIGINAL TO CLEANED
# Histograms of Age
hist_omitting_NA(titanic_data$Age)
hist(titanic_cleaned$Age)

# Histograms of Passanger Fare
hist_omitting_NA(titanic_data$Passenger.Fare)
hist(titanic_cleaned$Passenger.Fare)

# Histograms of Port of Embarkation
plot_omitting_NA(titanic_data$Port.of.Embarkation)
plot_omitting_NA(titanic_cleaned$Port.of.Embarkation)

# Histogram of Number of Cabins
hist(titanic_cleaned$nCabins)

## EXPLORATORY ANALYSIS

# Survival Rate and Age Group:
# > Takes the column titanic_cleaned$Age and creates a new object storing each age into
# > its corresponding index and changing the value into an age group of 10
age_groups <- cut(titanic_cleaned$Age, 
                  hist(titanic_cleaned$Age, 
                  breaks = 10, 
                  plot = FALSE)$breaks)

# > Creates a new dataframe with columns; survival rate, and age group
age_survival_rate <- rate_by(titanic_cleaned$Survived.dum, age_groups)

# > Plots Survival Rate by Age Group
age_survival_rate %>%
    ggplot(aes(x = groups, y = rate)) +
    geom_bar(stat = "identity") +
    ggtitle("Survival Rate by Age Group")

# Survival Rate and Sex:
# > Creates a new dataframe with columns; survival rate, and sex
sex_survival_rate <- rate_by(titanic_cleaned$Survived.dum, titanic_cleaned$Sex)

# > Plots Survival Rate by Sex
sex_survival_rate %>%
    ggplot(aes(x = groups, y = rate)) +
    geom_bar(stat = "identity") +
    ggtitle("Survival Rate by Sex")

# Survival Rate and Passenger Class:
# > Creates a new dataframe with columns; survival rate, and passenger class
class_survival_rate <- rate_by(titanic_cleaned$Survived.dum, titanic_cleaned$Passenger.Class)

# > Plots Survival Rate by Passenger Class
class_survival_rate %>%
    ggplot(aes(x = groups, y = rate)) +
    geom_bar(stat = "identity") +
    ggtitle("Survival Rate by Passenger Class")

# Survival and Title:
# > Plots Survival by Title
titanic_cleaned %>%
    ggplot(aes(x = Title, fill = factor(Survived))) +
    geom_histogram(stat = "count") +
    ggtitle("Survival and Title") +
    coord_flip()

# > Plots Survival by Adjusted Titles
titanic_cleaned %>%
    ggplot(aes(x = Passenger_Title, fill = factor(Survived))) +
    geom_histogram(stat = "count") +
    ggtitle("Survival and Title") +
    coord_flip()

# > Plots Survival Rate and Adjusted Titles
# > Creates a new dataframe with columns; survival rate, and passenger class
title_survival_rate <- rate_by(titanic_cleaned$Survived.dum, Passenger_Title)

title_survival_rate %>%
    ggplot(aes(x = groups, y = rate)) +
    geom_bar(stat = "identity") +
    ggtitle("Survival Rate by Title")

# Survival and Family Size:
# > Plot of No. Parents or Children on Board
titanic_cleaned %>%
    ggplot(aes(x = No.of.Parents.or.Children.on.Board, fill = factor(Survived))) +
    geom_histogram(stat = "count") +
    ggtitle("Survival and No. Parents or Children on Board") +
    coord_flip()

# > Plot of No. Siblings or Spouses on Board
titanic_cleaned %>%
    ggplot(aes(x = No.of.Siblings.or.Spouses.on.Board, fill = factor(Survived))) +
    geom_histogram(stat = "count") +
    ggtitle("Survival and No. Siblings or Spouses on Board") +
    coord_flip()

# > Plot of Family Size
titanic_cleaned %>%
    ggplot(aes(x = Family.Size, fill = factor(Survived))) +
    geom_histogram(stat = "count") +
    ggtitle("Survival and Family Size") +
    coord_flip()

# > Plot of Survival Rate and Family Size
family_survival_rate <- rate_by(titanic_cleaned$Survived.dum, titanic_cleaned$Family.Size)

family_survival_rate %>%
    ggplot(aes(x = groups, y = rate)) +
    geom_bar(stat = "identity") +
    ggtitle("Survival Rate by Family Size") +
    scale_x_continuous(breaks = 0:10)

# Survival and Number of Cabins
# > Plot of Survival and Number of Cabins
titanic_cleaned %>%
    ggplot(aes(x = nCabins, fill = factor(Survived))) +
    geom_histogram(stat = "count") +
    ggtitle("Survival and Number of Cabins") +
    coord_flip()

# > Plot of Survival Rate and Number of Cabins
cabin_survival_rate <- rate_by(titanic_cleaned$Survived.dum, titanic_cleaned$nCabins)

cabin_survival_rate %>%
    ggplot(aes(x = groups, y = rate)) +
    geom_bar(stat = "identity") +
    ggtitle("Survival Rate by Number of Cabins")

# Survival and Passenger Fare
# > Plot of Survival and Number of Cabins
titanic_cleaned %>%
    ggplot(aes(x = Passenger.Fare, fill = factor(Survived))) +
    geom_histogram() +
    ggtitle("Survival and Passenger Fare (pounds)")

# > Plot of Survival Rate and Passenger Fare
# > Takes the column titanic_cleaned$Age and creates a new object storing each age into
# > its corresponding index and changing the value into an age group of 10
fare_groups <- cut(titanic_cleaned$Passenger.Fare, c(-0.01,10,20,40,80,160,320,640))

fare_survival_rate <- rate_by(titanic_cleaned$Survived.dum, fare_groups)

fare_survival_rate %>%
    ggplot(aes(x = groups, y = rate)) +
    geom_bar(stat = "identity") +
    ggtitle("Survival Rate by Passenger Fare (pounds)")

# Survival and Port of Embarkation
# > Plot of Survival and Port of Embarkation
titanic_cleaned %>%
    ggplot(aes(x = Port.of.Embarkation, fill = factor(Survived))) +
    geom_histogram(stat = "count") +
    ggtitle("Survival and Port of Embarkation")

# > Plot of Survival Rate and Port of Embarkation
port_survival_rate <- rate_by(titanic_cleaned$Survived.dum, titanic_cleaned$Port.of.Embarkation)

port_survival_rate %>%
    ggplot(aes(x = groups, y = rate)) +
    geom_bar(stat = "identity") +
    ggtitle("Survival Rate by Port of Embarkation")

# ------------------------------------------------------------------------------------------|
# DATA SAMPLING:

# Code columns as dummy variables
titanic_cleaned$Survived.dum <- code_column_as_dummy(titanic_cleaned$Survived)-1
titanic_cleaned$Passenger.Class.dum <- code_column_as_dummy(titanic_cleaned$Passenger.Class)
titanic_cleaned$Sex.dum <- code_column_as_dummy(titanic_cleaned$Sex)
titanic_cleaned$Normal.Title.dum <- code_column_as_dummy(titanic_cleaned$Normal.Title)

# Dropping variables:
# > Removed Life Boat as it is assumed that the passenger survived if they
# > were on a life boat
# > 'Life Boat', 'Ticket Number', and 'Cabin' don't seem to have any correlation to survival
# > 'Number of Siblings or Spouses on Board', and 'Number of Parents or Children on Board' can
# > also be removed since they appear to have the same correlation as each other which we
# > represented with 'Family Size'
# > Names vary so much that they cannot be categorized
# > Splitting rare titles can lead to model errors and Normal.Title achieves a similar outcome
titanic_cleaned <- titanic_cleaned %>%
    select(-c(Life.Boat, 
              Ticket.Number, 
              Cabin, 
              No.of.Siblings.or.Spouses.on.Board, 
              No.of.Parents.or.Children.on.Board,
              Name,
              Title))

# > Writes titanic_cleaned to 'titanic-cleaned.csv'
write.csv(titanic_cleaned, file = "titanic-cleaned.csv")

# Sampling:
# > Samples the cleaned titanic data into training and testing sets
# > The sample will be split as 70% - 30% respectively
# > create a unique id for each case
titanic_cleaned <- titanic_cleaned %>%
    mutate(id = row_number())

# > create training set
train <- titanic_cleaned %>%
    sample_frac(0.7)

# > create test set
test <- anti_join(titanic_cleaned, train, by = 'id')

# > drop the "id" variable
train <- subset(train, select = -c(id))
test <- subset(test, select = -c(id))

# > save training set
write.csv(train, file = "titanic-train.csv")

# > save test set
write.csv(test, file = "titanic-test.csv")

# ------------------------------------------------------------------------------------------|
# DATA MODELLING:

# Imports written datasets such that our training and testing sets remain consistent
# for further algorithm evaluation
train <- import_data("titanic-train.csv")
test <- import_data("titanic-test.csv")

# > Remove redundant index column
train <- train %>% select(-X)
test <- test %>% select(-X)

# > Some models only work on numeric values
numeric_train <- select_if(train, is.numeric)
numeric_test <- select_if(test, is.numeric)

# > Remove dummy variables from potentially skewing some models
class_train <- train%>% select(-Survived.dum, -Passenger.Class.dum, -Sex.dum, -Normal.Title.dum)
class_test <- test%>% select(-Survived.dum, -Passenger.Class.dum, -Sex.dum, -Normal.Title.dum)

# All models will use numeric sets so no variables are weighed multiple times
# Random Forest
rf_model <- randomForest(as.factor(Survived)~., data=class_train, ntree = 12)
rf_prediction <- predict(rf_model, newdata=class_test, type="response")

titanic_rf <- model_df(rf_prediction, test$Survived)

# Multinomial Logistic Regression:
mlr_model <- multinom(formula=Survived~., data=class_train)
mlr_probability <- predict(mlr_model, newdata=class_test, type="probs")
mlr_prediction <- predict(mlr_model, newdata=class_test, type="class")

titanic_mlr <- model_df(mlr_prediction, test$Survived)

# Neural Network:
nn_model <- neuralnet(Survived.dum~., data=numeric_train, hidden=c(2,1))
nn_probability <- predict(nn_model, newdata=numeric_test, type="response")
nn_prediction <- ifelse(nn_probability > 0.5, "Yes", "No")

titanic_nn <- model_df(nn_prediction, test$Survived)

# ------------------------------------------------------------------------------------------|
# MODEL EVALUATION:

#Random Forest
rf_matrix <- table(titanic_rf)
rf_matrix
model_evaluation(titanic_rf)
plot(rf_model)

# Multinomial Logistic Regression:
mlr_matrix <- table(titanic_mlr)
mlr_matrix
model_evaluation(titanic_mlr)

# Neural Network:
nn_matrix <- table(titanic_nn)
nn_matrix
model_evaluation(titanic_nn)
plot(nn_model)