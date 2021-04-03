# Titanic Data: Analysis
# BUSA3020 Advanced Analytics Techniques
# Student Name: Cyrus Kwan
# Student ID: 45200165
# Last Modified: 3/04/2021
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
    new_dummy <- as.numeric(as.factor(column))
    return(new_dummy)
}

list_NA <- function(dataframe){
    # Returns a list column names that contain NA values from a parsed data frame
    new_list_NA <- colnames(dataframe)[apply(dataframe, 2, anyNA)]
    return(new_list_NA)
}

plot_omitting_NA <- function(column){
    # Returns bar chart of the parsed column excluding NA data
    # Best used for categorical data
    new_plot <- plot(
        as.factor(
            na.omit(column)
            ),
            main = paste("Plot of ", toString(substitute(column))
            )
        )

    return(new_plot)
}

hist_omitting_NA <- function(column){
    # Returns histogram of the parsed column excluding NA data
    # Best used for numerical data
    new_hist <- hist(
        na.omit(column),
        main = paste("Histogram of ", toString(substitute(column)))
        )
    return(new_hist)
}

rate_by <- function(target, category){
    # Rates the target dummy variable by its corresponding category
    groups = sort(unique(category))
    temp_df <- data.frame(t = target, c =category)
    rate_df <- data.frame(rate = c(1:length(groups)), groups)
    for(column in sort(unique(category))){
        target_positive = table(temp_df$t[which(temp_df$c == column)])
        if(length(target_positive) < 2){
            rate_df$rate[which(rate_df$groups == column)] = 0
        } else{
            rate_df$rate[which(rate_df$groups == column)] = target_positive[["1"]]/sum(target_positive)
        }
    }
    return(rate_df)
}