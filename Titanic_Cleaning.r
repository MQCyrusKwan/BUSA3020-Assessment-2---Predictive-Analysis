# Titanic Data: Cleaning
# BUSA3020 Advanced Analytics Techniques
# Student Name: Cyrus Kwan
# Student ID: 45200165
# Last Modified: 31/03/2021
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