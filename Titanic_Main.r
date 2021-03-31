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
pacman::p_load(pacman, dplyr, ggplot2)

# Source functions from other R scripts
source("Titanic_Analysis.r")

# Data from: https://ilearn.mq.edu.au/mod/resource/view.php?id=6351154
titanic_data <- import_data(file_path = "TitanicData_AllPassengers.csv")

# Show first several rows of data
head(titanic_data)

# Structure of the data
str(titanic_data)

# Summary of data
summary(titanic_data)