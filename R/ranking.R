##############################################################################
# Copyright (C) 2024                                                         #
#                                                                            #
# This code is free software: you can redistribute it and/or modify it under #
# the terms of the GNU General Public License as published by the Free       #
# Software Foundation, either version 3 of the License, or (at your option)  #
# any later version. This code is distributed in the hope that it will be    #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General   #
# Public License for more details.                                           #
#                                                                            #
# Prof. Elaine Cecilia Gatto | Prof. Ricardo Cerri | Prof. Mauri Ferrandin   #
#                                                                            #
# Federal University of São Carlos - UFSCar - https://www2.ufscar.br         #
# Campus São Carlos - Computer Department - DC - https://site.dc.ufscar.br   #
# Post Graduate Program in Computer Science - PPGCC                          #
# http://ppgcc.dc.ufscar.br - Bioinformatics and Machine Learning Group      #
# BIOMAL - http://www.biomal.ufscar.br                                       #
#                                                                            #
##############################################################################


##############################################################################
# WORSKSPACE
##############################################################################
FolderRoot = "~/Multi-Label-Friedman-Nemenyi"
FolderScripts = "~/Multi-Label-Friedman-Nemenyi/R"



##############################################################################
#
##############################################################################
setwd(FolderScripts)
source("libraries.R")
source("utils.R")



##############################################################################
#
# Function to generate various types of rankings for each row in a dataset.
#
# This function computes rankings for each row of a dataset using different 
# tie-breaking methods. It generates rankings in both the original and reverse 
# order (where higher ranks become lower) and returns the results in a list.
#
# Parameters:
# - data: A data frame where each row represents a set of values to be ranked.
#   The columns of the data frame are considered as the different items to rank.
#
# Returns:
# - result: A list containing multiple data frames with rankings calculated 
#   using different tie-breaking methods. The list includes:
#     - rank.first.0: Rankings with "first" tie-breaking method, in original order
#     - rank.last.0: Rankings with "last" tie-breaking method, in original order
#     - rank.average.0: Rankings with "average" tie-breaking method, in original order
#     - rank.random.0: Rankings with "random" tie-breaking method, in original order
#     - rank.min.0: Rankings with "min" tie-breaking method, in original order
#     - rank.max.0: Rankings with "max" tie-breaking method, in original order
#     - rank.first.1: Rankings with "first" tie-breaking method, in reverse order
#     - rank.last.1: Rankings with "last" tie-breaking method, in reverse order
#     - rank.average.1: Rankings with "average" tie-breaking method, in reverse order
#     - rank.random.1: Rankings with "random" tie-breaking method, in reverse order
#     - rank.min.1: Rankings with "min" tie-breaking method, in reverse order
#     - rank.max.1: Rankings with "max" tie-breaking method, in reverse order
#
##############################################################################
generate.ranking <- function(data) {
  
  # Initialize list to store ranking results
  result <- list()
  
  # Get the number of columns and rows in the data
  num.columns <- ncol(data)
  num.rows <- nrow(data)
  
  # Initialize data frames to store rankings with different tie-breaking methods
  rank.first.0 <- data.frame()
  rank.last.0 <- data.frame()
  rank.average.0 <- data.frame()
  rank.random.0 <- data.frame()
  rank.min.0 <- data.frame()
  rank.max.0 <- data.frame()
  
  # Calculate rankings for each row using various tie-breaking methods
  for (i in 1:num.rows) {
    rf <- rank(data[i, ], ties.method = "first")    # Rank with first occurrence wins
    rl <- rank(data[i, ], ties.method = "last")     # Rank with last occurrence wins
    rav <- rank(data[i, ], ties.method = "average")  # Rank with average of ranks for ties
    ran <- rank(data[i, ], ties.method = "random")   # Rank with random tie-breaking
    rma <- rank(data[i, ], ties.method = "max")      # Rank with maximum rank for ties
    rmi <- rank(data[i, ], ties.method = "min")      # Rank with minimum rank for ties
    
    # Append the results to respective data frames
    rank.first.0 <- rbind(rank.first.0, rf)
    rank.last.0 <- rbind(rank.last.0, rl)
    rank.average.0 <- rbind(rank.average.0, rav)
    rank.random.0 <- rbind(rank.random.0, ran)
    rank.max.0 <- rbind(rank.max.0, rma)
    rank.min.0 <- rbind(rank.min.0, rmi)
  }
  
  # Set column names for the ranking data frames
  colnames(rank.first.0) <- colnames(data)
  colnames(rank.last.0) <- colnames(data)
  colnames(rank.average.0) <- colnames(data)
  colnames(rank.random.0) <- colnames(data)
  colnames(rank.max.0) <- colnames(data)
  colnames(rank.min.0) <- colnames(data)
  
  # Initialize data frames to store transformed rankings (reverse order)
  rank.first.1 <- data.frame()
  rank.last.1 <- data.frame()
  rank.average.1 <- data.frame()
  rank.random.1 <- data.frame()
  rank.min.1 <- data.frame()
  rank.max.1 <- data.frame()
  
  # Transform the rankings to reverse order (highest rank becomes lowest)
  for (i in 1:num.rows) {
    rf <- (num.columns - rank.first.0[i, ]) + 1
    rl <- (num.columns - rank.last.0[i, ]) + 1
    rav <- (num.columns - rank.average.0[i, ]) + 1
    ran <- (num.columns - rank.random.0[i, ]) + 1
    rma <- (num.columns - rank.max.0[i, ]) + 1
    rmi <- (num.columns - rank.min.0[i, ]) + 1
    
    # Append the results to respective data frames
    rank.first.1 <- rbind(rank.first.1, rf)
    rank.last.1 <- rbind(rank.last.1, rl)
    rank.average.1 <- rbind(rank.average.1, rav)
    rank.random.1 <- rbind(rank.random.1, ran)
    rank.max.1 <- rbind(rank.max.1, rma)
    rank.min.1 <- rbind(rank.min.1, rmi)
  }
  
  # Set column names for the transformed ranking data frames
  colnames(rank.first.1) <- colnames(data)
  colnames(rank.last.1) <- colnames(data)
  colnames(rank.average.1) <- colnames(data)
  colnames(rank.random.1) <- colnames(data)
  colnames(rank.max.1) <- colnames(data)
  colnames(rank.min.1) <- colnames(data)
  
  # Truncate average rankings to integer values
  rank.average.0 <- trunc(rank.average.0, 0)
  rank.average.1 <- trunc(rank.average.1, 0)
  
  # Store all ranking results in the result list
  result$rank.first.0 <- rank.first.0
  result$rank.last.0 <- rank.last.0
  result$rank.average.0 <- rank.average.0
  result$rank.random.0 <- rank.random.0
  result$rank.max.0 <- rank.max.0
  result$rank.min.0 <- rank.min.0
  
  result$rank.first.1 <- rank.first.1
  result$rank.last.1 <- rank.last.1
  result$rank.average.1 <- rank.average.1
  result$rank.random.1 <- rank.random.1
  result$rank.max.1 <- rank.max.1
  result$rank.min.1 <- rank.min.1
  
  # Return the result list containing all ranking data frames
  return(result)
}






##############################################################################
#
# Function to generate rankings for CSV files and save the results.
#
# This function processes a list of CSV files, generates rankings based on
# the provided columns, and saves the results in a specified destination folder.
# It handles different types of ranking based on the provided measure values.
#
# Parameters:
# - type: A vector of column indices or names to select from the CSV files
#   for ranking. This determines which columns are included in the ranking.
# - source_folder: The folder path where the input CSV files are located.
# - destination_folder: The folder path where the ranking results will be saved.
#   Default is set to 'Folders', which should be defined in the user's 
# environment.
# - file_names: A vector of file names (without path) for the CSV files to be 
#   processed.
# - names_list: A vector of names corresponding to the measures for each 
# CSV file.
#
# Returns:
# - result: A list containing ranking results for each file. The list has an 
#   entry for each file, with the rankings stored as data frames.
#
##############################################################################
generates.rank.again <- function(type, 
                                 source_folder, 
                                 destination_folder = Folders, 
                                 file_names, 
                                 names_list) {
  
  # Initialize list to store ranking results
  result <- list()
  
  index <- 1
  while (index <= length(file_names)) {
    
    # Construct the file path
    file_path <- paste(source_folder, "/", file_names[index], sep = "")
    
    # Read the CSV file and format the dataframe
    data <- data.frame(read.csv2(file_path))
    
    # Replace NA values with zero
    data <- data %>% replace(is.na(.), 0)
    
    # Remove the first column
    data <- data[, -1]
    
    # Select specific columns based on the 'type' argument
    data <- data[, type]
    
    # Generate rankings for the data
    rankings <- generate.ranking(data)
    
    # Filter the 'measures' data frame for the current 'names_list' value
    measure <- filter(measures, measures$names == names_list[index])
    
    # Create file name for saving the rankings
    file_name <- paste(names_list[index], "-ranking.csv", sep = "")  
    
    # Create directory to store results if it does not exist
    root_folder <- paste(FolderRoot, "/Rankings", sep = "")
    if (!dir.exists(root_folder)) {
      dir.create(root_folder)
    }
    
    destination_path <- paste(root_folder, "/", destination_folder, sep = "")
    if (!dir.exists(destination_path)) {
      dir.create(destination_path)
    }
    
    # Save the appropriate ranking based on the measure value
    if (measure$values == 1) {
      cat("\n1 \t", names_list[index])
      setwd(destination_path)
      write.csv(rankings$rank.average.1, file_name, row.names = FALSE)
      result$rank[[index]] <- rankings$rank.average.1
      
    } else {
      cat("\n0 \t", names_list[index])
      setwd(destination_path)
      write.csv(rankings$rank.average.0, file_name, row.names = FALSE)  
      result$rank[[index]] <- rankings$rank.average.0
    }
    
    # Move to the next file
    index <- index + 1
    gc()  # Call garbage collection to free up memory
    
  }
  
  # Return the result list containing all ranking data
  return(result)
}




##############################################################################
#
# Function to generate rankings for all measures in a set of CSV files.
#
# This function processes a list of CSV files, computes rankings based on
# various measures, and saves the results in specified directories. It
# handles different tie-breaking methods and saves both individual and 
# aggregated ranking results. The function also generates mean rankings 
# for both "0" and "1" categories, and stores these in separate files.
#
# Parameters:
# - folder_names_csv: A vector of file names (without path) for the CSV files
#   to be processed.
# - measure_names: A vector of names corresponding to the measures to be 
#   used for filtering rankings.
# - my_methods: A vector of method names that will be used as column names 
#   for the DataFrame created from the CSV files.
# - folders: A list containing folder paths for different stages of saving 
#   results. This should include:
#   - FolderCSVs: Path where the input CSV files are located.
#   - FolderRankings: Path where individual ranking results should be saved.
#   - FolderAllRankings: Path where all ranking averages should be saved.
#   - FolderMediaRankings: Path where mean ranking results should be saved.
# - results_mm: A list containing results with measures data, which includes
#   information needed to determine which ranking method to apply.
#
# Returns:
# - result: A list that currently is empty but can be extended to include
#   additional results or status information if needed.
#
##############################################################################
ranking.for.all.measures <- function(folder_names_csv, measure_names,
                                     my_methods, folders, results_mm) {
  
  # Initialize list to store results
  result <- list()
  
  # Initialize data frames to store combined results
  temp_0 <- data.frame(0)
  temp_1 <- data.frame(0)
  
  # Extract measures data from the results
  measures <- results_mm$measures
  
  # Process each CSV file
  for (i in 1:length(folder_names_csv)) {
    
    # Construct the file path
    file_path <- paste(folders$FolderCSVs, "/", folder_names_csv[i], sep = "")
    
    # Read the CSV file and format the dataframe
    data <- data.frame(read.csv(file_path))
    
    # Remove the first column
    data <- data[, -1]
    
    # Replace NA values with zero
    data[is.na(data)] <- 0
    
    # Set column names based on provided methods
    colnames(data) <- my_methods
    
    # Generate rankings for the data
    rankings <- generate_ranking(data)
    
    # Filter the 'measures' data frame for the current measure name
    measure_value <- filter(measures, measures$names == measure_names[i])
    
    # Create file name for saving the rankings
    file_name <- paste(measure_names[i], "-ranking.csv", sep = "")
    
    # Save the ranking data based on the measure value
    if (measure_value$values == 1) {
      cat("\n1 \t", measure_names[i])
      setwd(folders$FolderRankings)
      write.csv(rankings$rank_average_0, file_name, row.names = FALSE)
    } else {
      cat("\n0 \t", measure_names[i])
      setwd(folders$FolderRankings)
      write.csv(rankings$rank_average_1, file_name, row.names = FALSE)
    }
    
    # Save all ranking averages with prefix "0-"
    setwd(folders$FolderAllRankings)
    all_rankings_0 <- paste("0-", file_name, sep = "")
    write.csv(rankings$rank_average_0, all_rankings_0, row.names = FALSE)
    
    # Save all ranking averages with prefix "1-"
    all_rankings_1 <- paste("1-", file_name, sep = "")
    write.csv(rankings$rank_average_1, all_rankings_1, row.names = FALSE)
    
    # Save the mean of average rankings for "0" and "1"
    setwd(folders$FolderMediaRankings)
    mean_rankings_0 <- data.frame(apply(rankings$rank_average_0, 2, mean))
    colnames(mean_rankings_0) <- "mean"
    mean_file_0 <- paste("mean-", file_name, sep = "")
    write.csv(mean_rankings_0, mean_file_0)
    temp_0 <- cbind(temp_0, mean_rankings_0)
    
    mean_rankings_1 <- data.frame(apply(rankings$rank_average_1, 2, mean))
    colnames(mean_rankings_1) <- "mean"
    mean_file_1 <- paste("1-mean-", file_name, sep = "")
    write.csv(mean_rankings_1, mean_file_1)
    temp_1 <- cbind(temp_1, mean_rankings_1)
    
    # Move to the next file
    gc()  # Call garbage collection to free up memory
  }
  
  # Return the result list (currently empty, but can be extended)
  return(result)
}


##############################################################################
#
##############################################################################

