# Titanic Data: Analysis
# BUSA3020 Advanced Analytics Techniques
# Student Name: Cyrus Kwan
# Student ID: 45200165
# Last Modified: 31/03/2021
# Accessible via: https://github.com/MQCyrusKwan/BUSA3020-Assessment-2---Predictive-Analysis

import_data <- function(file_path){
    # initializes a new data frame containing values from the file path
    # removes the last row of the dataframe
    my_data <- head(
        x = read.csv(
            file = file_path, 
            header = TRUE, 
            na.strings = ''), 
        n = -1
        )
    return(my_data)
}

code_column_as_dummy <- function(column){
    # Codes the parsed column as a dummy variable
    new_dummy <- as.numeric(
        as.factor(column)
        )
    return(new_dummy)
}

list_NA <- function(dataframe){
    # Returns a list column names that contain NA values from a parsed data frame
    new_list_NA <- colnames(dataframe)[apply(dataframe, 2, anyNA)]
    return(new_list_NA)
}