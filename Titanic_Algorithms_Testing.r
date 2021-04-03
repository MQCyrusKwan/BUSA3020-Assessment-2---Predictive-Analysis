# Titanic Data: Algorithms & Testing
# BUSA3020 Advanced Analytics Techniques
# Student Name: Cyrus Kwan
# Student ID: 45200165
# Last Modified: 3/04/2021
# Accessible via: https://github.com/MQCyrusKwan/BUSA3020-Assessment-2---Predictive-Analysis

# Install package manager
if(find.package("pacman") == FALSE){
    install.packages("pacman")
}

# Load packages
# Load with conformation: require()
# Load without conformation: library()
require(pacman)
pacman::p_load(randomForest)

model_df <- function(prediction_data, actual_data){
    new_df <- data.frame(prediction = prediction_data, actual = actual_data)
    return(new_df)
}

model_evaluation <- function(model_df){
    matrix <- confusion_matrix(model_df)

    true_pos <- matrix[2,2]
    true_neg <- matrix[1,1]
    false_pos <- matrix[2,1]
    false_neg <- matrix[1,2]

    prec <- true_pos/(true_pos+false_pos)
    rec <- true_pos/(true_pos+false_neg)

    f1 <- 2*(prec*rec)/(prec+rec)
    ca <- sum(diag(table(model_df))/sum(table(model_df)))

    scores_df <- data.frame(precision=prec, recall=rec, f1_score=f1, accuracy=ca)

    return(scores_df)
}

confusion_matrix <- function(model_df){
    table(model_df)
}