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


# Clear workspace
# rm(list=ls())


##############################################################################
# WORSKSPACE
##############################################################################

FolderRoot = "~/MultiLabelFriedmanNemenyi"
FolderScripts = "~/MultiLabelFriedmanNemenyi/R"
FolderData = "~/MultiLabelFriedmanNemenyi/Data"
FolderResults = "~/MultiLabelFriedmanNemenyi/Results"


##############################################################################
# Set working directories and source necessary scripts
##############################################################################

#setwd(FolderScripts)
#source("libraries.R")
#source("utils.R")
#source("ranking.R")
#source("friedman-nemenyi.R")

library(MultiLabelFriedmanNemenyi)


##############################################################################
# ONE RESULT
##############################################################################

setwd(FolderRoot)
clp = data.frame(read.csv("~/MultiLabelFriedmanNemenyi/Data/clp.csv"))
clp = clp[,-1]

df_res.mes <- fn.measures()
filtered_res.mes <- filter(df_res.mes, names == "clp")
save = paste(FolderResults, "/clp", sep="")

if(filtered_res.mes$type==1){
  # if the measure is type 1, isto é, o melhore valor é um
    res = friedman.nemenyi(data = clp, save = save)
  
} else {
  # if the measure is type 0, isto é, o melhor valor é zero
  
  ranking = generate.ranking(data = clp)
  res.data = data.frame(ranking$rank.average.1) 
  res.fn = friedman.nemenyi(data = res.data, save = save)
}





##############################################################################
# Read and process all CSV files in the data folder
##############################################################################

setwd(FolderData)
current_dir <- getwd()
files <- list.files(pattern = "\\.csv$", full.names = TRUE)  # List all CSV files
full_paths <- sapply(files, function(file) normalizePath(file))



##############################################################################
# 
##############################################################################

# Process each CSV file
for (file_path in full_paths) {
  # Read the CSV file
  data_name <- basename(file_path)  # Extract file name
  data <- data.frame(read.csv(file_path))
  
  # Remove the first column
  data <- data[, -1]
  
  # Generate rankings
  ranking <- generate.ranking(data = data)
  
  # Get measure name from the file name (assuming the file name indicates the measure)
  measure_name <- tools::file_path_sans_ext(data_name)
  
  # Load measures data
  df_res.mes <- measures()
  filtered_res.mes <- filter(df_res.mes, names == measure_name)
  
  # Define the path to save results
  save_path <- paste(FolderResults, "/", measure_name, sep = "")
  
  if (filtered_res.mes$type == 1) {
    # If the measure is type 1, i.e., the best value is one
    res <- friedman.nemenyi(data = data, save = save_path)
    
  } else {
    # If the measure is type 0, i.e., the best value is zero
    res_data <- data.frame(ranking$rank.average.1) 
    res_fn <- friedman.nemenyi(data = res_data, save = save_path)
  }
  
  cat("\nProcessed file:", data_name)
}





##############################################################################
# END OF SCRIPT
##############################################################################
