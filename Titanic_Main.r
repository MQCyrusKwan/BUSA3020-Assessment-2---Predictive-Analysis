# Titanic Data: Main
# BUSA3020 Advanced Analytics Techniques
# Student Name: Cyrus Kwan
# Student ID: 45200165
# Last Modified: 31/03/2021
# Accessible via: https://github.com/MQCyrusKwan/BUSA3020-Assessment-2---Predictive-Analysis

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
source(file = "Titanic_Cleaning.r")

# Data from: https://ilearn.mq.edu.au/mod/resource/view.php?id=6351154
titanic_data <- import_data(file_path = "TitanicData_AllPassengers.csv")

# ------------------------------------------------------------------------------------------|
# DATA CLEANING:

# Removed Life Boat as it is assumed that the passenger survived if they
# were on a life boat
titanic_data <- titanic_data %>% select(-Life.Boat)

# Creates new dataframe that uses KNN imputation
# Fills out "Age", "Passenger Fare", and "Port of Embarkation" columns
titanic_KNN <- kNN(
    data = titanic_data, 
    variable = c("Age", "Passenger.Fare", "Port.of.Embarkation"))

# Creates new column containing the amount of cabins the passenger had
titanic_KNN$nCabins <- list_sum_substr(titanic_data$Cabin)

# ------------------------------------------------------------------------------------------|
# ANALYSIS:

# Histograms of Age
hist_omitting_NA(titanic_data$Age)
hist(titanic_KNN$Age)

# Histograms of Passanger Fare
hist_omitting_NA(titanic_data$Passenger.Fare)
hist(titanic_KNN$Passenger.Fare)

# Histograms of Port of Embarkation
plot_omitting_NA(titanic_data$Port.of.Embarkation)
plot_omitting_NA(titanic_KNN$Port.of.Embarkation)

# Histogram of Number of Cabins
hist(titanic_KNN$nCabins)

# ------------------------------------------------------------------------------------------|
# ALGORITHMS: