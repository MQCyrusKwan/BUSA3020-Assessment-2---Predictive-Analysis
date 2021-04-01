# Titanic Data: Main
# BUSA3020 Advanced Analytics Techniques
# Student Name: Cyrus Kwan
# Student ID: 45200165
# Last Modified: 1/04/2021
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
pacman::p_load(pacman, dplyr, ggplot2, VIM)

# Source functions from other R scripts
source(file = "Titanic_Analysis.r")
source(file = "Titanic_Features.r")

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

# Code column as dummy
titanic_cleaned$Survived.dum <- code_column_as_dummy(titanic_data$Survived)-1

# Extract title
titanic_cleaned$Title <- extract_title(titanic_cleaned$Name)

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
# > uncommon titles are summed into 'Rare'
Passenger_Title <- titanic_cleaned$Title
Passenger_Title[Passenger_Title != 'Mr' & 
                Passenger_Title != 'Miss' & 
                Passenger_Title != 'Mrs' & 
                Passenger_Title != 'Master'] <- 'Rare'

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

# Dropping variables:
# > Removed Life Boat as it is assumed that the passenger survived if they
# > were on a life boat
# > 'Life Boat', 'Ticket Number', and 'Cabin' don't seem to have any correlation to survival
# > 'Number of Siblings or Spouses on Board', and 'Number of Parents or Children on Board' can
# > also be removed since they appear to have the same correlation as each other which we
# > represented with 'Family Size'
titanic_cleaned <- titanic_cleaned %>%
    select(-c(Life.Boat, 
              Ticket.Number, 
              Cabin, 
              No.of.Siblings.or.Spouses.on.Board, 
              No.of.Parents.or.Children.on.Board))

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


# ------------------------------------------------------------------------------------------|
# MODEL EVALUATION: