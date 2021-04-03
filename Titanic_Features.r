# Titanic Data: Features
# BUSA3020 Advanced Analytics Techniques
# Student Name: Cyrus Kwan
# Student ID: 45200165
# Last Modified: 3/04/2021
# Accessible via: https://github.com/MQCyrusKwan/BUSA3020-Assessment-2---Predictive-Analysis

list_sum_substr <- function(column){
    # Returns the count of substrings for each row in the parsed column
    container <- column
    for(i in 1:length(column)){
        if(is.na(column[i])){
            container[i] = 0
        } else{
            ls_substrings = strsplit(column[i], " ")
            container[i] = lengths(ls_substrings)
        }
    }
    return(as.numeric(container))
}

extract_title <- function(column){
    # Extracts the title of the passenger from their name
    title <- column
    for(i in 1:length(column)){
        last_name <- strsplit(column[i], ", ")[[1]][2]
        title[i] <- strsplit(last_name, ". ")[[1]][1]
    }
    return(title)
}