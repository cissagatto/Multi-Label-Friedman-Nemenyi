##############################################################################
# Copyright (C) 2024                                                         #
#                                                                            #
# CC BY-NC-SA 4.0                                                            #
#                                                                            #
# Canonical URL https://creativecommons.org/licenses/by-nc-sa/4.0/           #
# Attribution-NonCommercial-ShareAlike 4.0 International CC BY-NC-SA 4.0     #
#                                                                            #
# Prof. Elaine Cecilia Gatto | Prof. Ricardo Cerri | Prof. Mauri Ferrandin   #
#                                                                            #
# Federal University of São Carlos - UFSCar - https://www2.ufscar.br         #
# Campus São Carlos - Computer Department - DC - https://site.dc.ufscar.br   #
# Post Graduate Program in Computer Science - PPGCC                          # 
# http://ppgcc.dc.ufscar.br - Bioinformatics and Machine Learning Group      #
# BIOMAL - http://www.biomal.ufscar.br                                       #
#                                                                            #
# You are free to:                                                           #
#     Share — copy and redistribute the material in any medium or format     #
#     Adapt — remix, transform, and build upon the material                  #
#     The licensor cannot revoke these freedoms as long as you follow the    #
#       license terms.                                                       #
#                                                                            #
# Under the following terms:                                                 #
#   Attribution — You must give appropriate credit , provide a link to the   #
#     license, and indicate if changes were made . You may do so in any      #
#     reasonable manner, but not in any way that suggests the licensor       #
#     endorses you or your use.                                              #
#   NonCommercial — You may not use the material for commercial purposes     #
#   ShareAlike — If you remix, transform, or build upon the material, you    #
#     must distribute your contributions under the same license as the       #
#     original.                                                              #
#   No additional restrictions — You may not apply legal terms or            #
#     technological measures that legally restrict others from doing         #
#     anything the license permits.                                          #
#                                                                            #
##############################################################################


##############################################################################
# WORSKSPACE
##############################################################################
FolderRoot = "~/MultiLabelFriedmanNemenyi"
FolderScripts = "~/MultiLabelFriedmanNemenyi/R"



##############################################################################
#
##############################################################################
# setwd(FolderScripts)
# source("libraries.R")
# source("utils.R")



################################################################################
#' Generate Various Types of Rankings for Each Row in a Dataset
#'
#' This function computes rankings for each row of a dataset using different 
#' tie-breaking methods. It generates rankings in both the original and reverse 
#' order (where higher ranks become lower) and returns the results in a list.
#'
#' @param data A data frame where each row represents a set of values to be ranked.
#'   The columns of the data frame are considered as the different items to rank.
#'
#' @return A list containing multiple data frames with rankings calculated 
#'   using different tie-breaking methods. The list includes:
#'   \describe{
#'     \item{rank.first.0}{Rankings with "first" tie-breaking method, in original order.}
#'     \item{rank.last.0}{Rankings with "last" tie-breaking method, in original order.}
#'     \item{rank.average.0}{Rankings with "average" tie-breaking method, in original order.}
#'     \item{rank.random.0}{Rankings with "random" tie-breaking method, in original order.}
#'     \item{rank.min.0}{Rankings with "min" tie-breaking method, in original order.}
#'     \item{rank.max.0}{Rankings with "max" tie-breaking method, in original order.}
#'     \item{rank.first.1}{Rankings with "first" tie-breaking method, in reverse order.}
#'     \item{rank.last.1}{Rankings with "last" tie-breaking method, in reverse order.}
#'     \item{rank.average.1}{Rankings with "average" tie-breaking method, in reverse order.}
#'     \item{rank.random.1}{Rankings with "random" tie-breaking method, in reverse order.}
#'     \item{rank.min.1}{Rankings with "min" tie-breaking method, in reverse order.}
#'     \item{rank.max.1}{Rankings with "max" tie-breaking method, in reverse order.}
#'   }
#'
#' @examples
#' # Example data frame
#' df <- data.frame(A = c(3, 1, 4), B = c(2, 5, 6), C = c(7, 8, 9))
#' 
#' # Generate rankings
#' rankings <- generate.ranking(df)
#' 
#' # View the rankings in original order
#' print(rankings$rank.first.0)
#' 
#' # View the rankings in reverse order
#' print(rankings$rank.first.1)
#'
#' @export
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




################################################################################
#' Generate Rankings for CSV Files and Save Results
#'
#' This function processes a list of CSV files, generates rankings based on
#' specified columns, and saves the results in a destination folder. It handles
#' different types of ranking based on provided measure values.
#'
#' @param type A vector of column indices or names to select from the CSV files
#'   for ranking. This determines which columns are included in the ranking.
#' @param source.folder The folder path where the input CSV files are located.
#' @param destination.folder The folder path where the ranking results will be saved.
#'   Default is set to 'Folders', which should be defined in the user's environment.
#' @param file.names A vector of file names (without path) for the CSV files to be 
#'   processed.
#' @param names.list A vector of names corresponding to the measures for each 
#'   CSV file.
#'
#' @return A list containing ranking results for each file. The list has an 
#'   entry for each file, with the rankings stored as data frames.
#'
#' @details
#' The function reads CSV files from the `source.folder`, processes each file by
#' generating rankings based on the specified columns (`type`), and saves the 
#' rankings to the `destination.folder`. The measure values from `names.list` determine
#' which ranking is saved. If the measure value is 1, it saves the rankings in 
#' reverse order; otherwise, it saves them in the original order.
#'
#' @examples
#' # Example usage
#' type <- c("Column1", "Column2")
#' source.folder <- "path/to/source"
#' destination.folder <- "path/to/destination"
#' file.names <- c("file1.csv", "file2.csv")
#' names.list <- c("measure1", "measure2")
#' 
#' # Generate and save rankings
#' results <- generate.rank.again(type, source.folder, destination.folder, file.names, names.list)
#'
#' @export
generate.rank.again <- function(type, 
                                source.folder, 
                                destination.folder, 
                                file.names, 
                                names.list) {
  
  # Initialize list to store ranking results
  results <- list()
  
  # Loop through each file name
  for (index in seq_along(file.names)) {
    
    # Construct the file path and read the CSV file
    file.path <- file.path(source.folder, file.names[index])
    data <- read.csv2(file.path, stringsAsFactors = FALSE)
    
    # Process the data: replace NA values with 0 and select specific columns
    data[is.na(data)] <- 0
    data <- data[, type, drop = FALSE]
    
    # Generate rankings for the selected data
    rankings <- generate.ranking(data)
    
    # Filter the measures data frame for the current 'names.list' value
    measure <- subset(measures, names == names.list[index])
    
    # Prepare the destination path and create it if it does not exist
    root.folder <- file.path(FolderRoot, "Rankings")
    if (!dir.exists(root.folder)) dir.create(root.folder)
    
    # Create Destination Path
    destination.path <- file.path(root.folder, destination.folder)
    if (!dir.exists(destination.path)) dir.create(destination.path)
    
    # Determine file name and save the appropriate ranking based on the measure value
    file.name <- paste0(names.list[index], "-ranking.csv")
    setwd(destination.path)
    
    if (measure$values == 1) {
      write.csv(rankings$rank.average.1, file.name, row.names = FALSE)
      results[[index]] <- rankings$rank.average.1
    } else {
      write.csv(rankings$rank.average.0, file.name, row.names = FALSE)
      results[[index]] <- rankings$rank.average.0
    }
    
    # Garbage collection to free up memory
    gc()
  }
  
  # Return the list containing all ranking results
  return(results)
}





################################################################################
#' Generate Rankings for All Measures in a Set of CSV Files
#'
#' This function processes a list of CSV files, computes rankings based on
#' various measures, and saves the results in specified directories. It handles
#' different tie-breaking methods and saves both individual and aggregated
#' ranking results. The function also generates mean rankings for both "0"
#' and "1" categories, and stores these in separate files.
#'
#' @param folder.names.csv A vector of file names (without path) for the CSV files
#'   to be processed.
#' @param measure.names A vector of names corresponding to the measures to be 
#'   used for filtering rankings.
#' @param my.methods A vector of method names that will be used as column names 
#'   for the DataFrame created from the CSV files.
#' @param folders A list containing folder paths for different stages of saving 
#'   results. This should include:
#'   \itemize{
#'     \item \code{folder.csvs}: Path where the input CSV files are located.
#'     \item \code{folder.rankings}: Path where individual ranking results should be saved.
#'     \item \code{folder.all.rankings}: Path where all ranking averages should be saved.
#'     \item \code{folder.media.rankings}: Path where mean ranking results should be saved.
#'   }
#' @param results.mm A list containing results with measures data, which includes
#'   information needed to determine which ranking method to apply.
#'
#' @return A list containing the rankings calculated for each CSV file, organized
#'   by the original filenames.
#'
#' @details
#' The function processes each CSV file listed in \code{folder.names.csv}, 
#' applies the specified measures to generate rankings, and saves the results
#' to the directories specified in \code{folders}. Rankings are saved with 
#' prefixes "0-" and "1-" to indicate the categories. Mean rankings for both 
#' categories are also computed and saved.
#'
#' @examples
#' # Example usage
#' folder.names.csv <- c("file1.csv", "file2.csv")
#' measure.names <- c("measure1", "measure2")
#' my.methods <- c("method1", "method2")
#' folders <- list(
#'   folder.csvs = "path/to/csvs",
#'   folder.rankings = "path/to/rankings",
#'   folder.all.rankings = "path/to/all_rankings",
#'   folder.media.rankings = "path/to/media_rankings"
#' )
#' results.mm <- list(measures = data.frame(names = c("measure1", "measure2"), values = c(0, 1)))
#' 
#' # Generate and save rankings
#' results <- ranking.for.all.measures(folder.names.csv, measure.names, my.methods, folders, results.mm)
#'
#' @export
ranking.for.all.measures <- function(folder.names.csv, 
                                     measure.names,
                                     my.methods, 
                                     folders, 
                                     results.mm) {
  
  # Initialize list to store results for each file
  results <- list()
  
  # Extract measures data from the results
  measures <- results.mm$measures
  
  # Process each CSV file
  for (i in seq_along(folder.names.csv)) {
    
    # Construct the file path and read the CSV file
    file.path <- file.path(folders$folder.csvs, folder.names.csv[i])
    data <- read.csv(file.path, stringsAsFactors = FALSE)
    
    # Process the data: remove the first column, replace NA with 0, and set column names
    data <- data[ , -1, drop = FALSE]
    data[is.na(data)] <- 0
    colnames(data) <- my.methods
    
    # Generate rankings for the data
    rankings <- generate.ranking(data)
    
    # Filter the measures data frame for the current measure name
    measure.value <- measures[measures$names == measure.names[i], "values"]
    
    # Define file name for saving rankings
    file.name <- paste0(measure.names[i], "-ranking.csv")
    
    # Set working directory and save the ranking data based on the measure value
    setwd(folders$folder.rankings)
    if (measure.value == 1) {
      write.csv(rankings$rank.average.0, file.name, row.names = FALSE)
      results[[folder.names.csv[i]]]$rank.average.0 <- rankings$rank.average.0
    } else {
      write.csv(rankings$rank.average.1, file.name, row.names = FALSE)
      results[[folder.names.csv[i]]]$rank.average.1 <- rankings$rank.average.1
    }
    
    # Save all ranking averages with prefixes "0-" and "1-"
    setwd(folders$folder.all.rankings)
    write.csv(rankings$rank.average.0, paste0("0-", file.name), row.names = FALSE)
    write.csv(rankings$rank.average.1, paste0("1-", file.name), row.names = FALSE)
    
    # Compute and save the mean rankings for "0" and "1" categories
    setwd(folders$folder.media.rankings)
    mean.rankings.0 <- data.frame(mean = rowMeans(rankings$rank.average.0, na.rm = TRUE))
    write.csv(mean.rankings.0, paste0("mean-0-", file.name), row.names = FALSE)
    results[[folder.names.csv[i]]]$mean.rankings.0 <- mean.rankings.0
    
    mean.rankings.1 <- data.frame(mean = rowMeans(rankings$rank.average.1, na.rm = TRUE))
    write.csv(mean.rankings.1, paste0("mean-1-", file.name), row.names = FALSE)
    results[[folder.names.csv[i]]]$mean.rankings.1 <- mean.rankings.1
    
    # Garbage collection to free up memory
    gc()
  }
  
  # Return the list containing all ranking data
  return(results)
}



################################################################################
#' Save Multiple Dataframes to an Excel Workbook
#'
#' This function creates an Excel workbook and adds each dataframe from a given 
#' list as a separate sheet.
#' The name of each sheet corresponds to the name of the dataframe in the list.
#'
#' @param data.list A named list of dataframes to be written to the Excel workbook.
#' @param file.name A string specifying the file name for the Excel workbook, 
#' including the `.xlsx` extension.
#' @return None. The function saves the Excel workbook to the specified file.
#' @examples
#' # Example usage:
#' # data.list <- list("Sheet1" = df1, "Sheet2" = df2)
#' # save.dataframes.to.excel(data.list, "output.xlsx")
#' @export
save.dataframes.to.excel <- function(data.list, file.name) {
  # Create a new Excel workbook
  excel.file <- createWorkbook()
  
  # Loop over each dataframe in the list and add it as a sheet
  for (sheet.name in names(data.list)) {
    # Add a sheet with the corresponding name
    addWorksheet(excel.file, sheet.name)
    
    # Write the dataframe to the corresponding sheet
    writeData(excel.file, sheet.name, data.list[[sheet.name]])
  }
  
  # Save the Excel workbook
  saveWorkbook(excel.file, file.name, overwrite = TRUE)
  
  # Print a message indicating the file was saved successfully
  message("The Excel file '", file.name, "' has been saved successfully.")
  cat("\n")
}




##############################################################################
#
##############################################################################

