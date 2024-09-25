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

setwd(FolderScripts)
source("libraries.R")
source("utils.R")
source("ranking.R")
source("friedman-nemenyi.R")

library(MultiLabelFriedmanNemenyi)


##############################################################################
# FOR MEASURES WITH BEST VALUE EQUAL TO 0
##############################################################################

setwd(FolderRoot)
clp = data.frame(read.csv("~/MultiLabelFriedmanNemenyi/Data/clp.csv"))
clp = clp[,-1]

df_res.mes <- fn.measures()
filtered_res.mes <- filter(df_res.mes, names == "clp")

save = paste(FolderResults, "/clp", sep="")
if(dir.exists(save)==FALSE){dir.create(save)}

ranking = generate.ranking(data = clp)
res.data = data.frame(ranking$rank.average.1) 
res.fn = friedman.nemenyi(data = res.data , 
                          save = save,
                          measure.name = "clp",
                          width = 60, 
                          height = 30,
                          cex=5.5)


##############################################################################
# FOR MEASURES WITH BEST VALUE EQUAL TO 1
##############################################################################

setwd(FolderRoot)
accuracy = data.frame(read.csv("~/MultiLabelFriedmanNemenyi/Data/accuracy.csv"))
accuracy = accuracy [,-1]

df_res.mes <- fn.measures()
filtered_res.mes <- filter(df_res.mes, names == "accuracy")

save = paste(FolderResults, "/accuracy", sep="")
if(dir.exists(save)==FALSE){dir.create(save)}

res = friedman.nemenyi(data = accuracy, 
                       save = save,
                       measure.name = "accuracy",
                       width = 60, 
                       height = 30,
                       cex=5.5)



##############################################################################
# Read and process all CSV files in the data folder
##############################################################################

# Set the working directory to the data folder
setwd(FolderData)
current.dir <- getwd()

# List all CSV files with full paths
files <- list.files(pattern = "\\.csv$", full.names = TRUE)
full.paths <- sapply(files, function(file) normalizePath(file))

# Initialize a data frame to store the concatenated results
all.results <- data.frame()

# Process each CSV file
for (file.path in full.paths) {
  
  
  # file.path = "C:/Users/Cissa/Documents/MultiLabelFriedmanNemenyi/Data/accuracy.csv"
  # Extract the file name
  data.name <- basename(file.path)
  
  # Read the CSV file into a data frame
  data <- data.frame(read.csv(file.path))
  
  # Remove the first column
  data <- data[, -1]
  
  # Generate rankings
  ranking <- generate.ranking(data = data)
  
  # Extract the measure name from the file name
  measure.name <- tools::file_path_sans_ext(data.name)
  
  # Load and filter measures data
  df.res.mes <- fn.measures()
  filtered.res.mes <- filter(df.res.mes, names == measure.name)
  
  # Define the path to save the results
  save.path <- file.path(FolderResults, measure.name)
  if (!dir.exists(save.path)) {
    dir.create(save.path)
  }
  
  # Save the rankings to an Excel file
  file.name <- file.path(save.path, paste0(measure.name, "-ranking.xlsx"))
  save.dataframes.to.excel(data.list = ranking, file.name = file.name)
  
  # Run Friedman-Nemenyi test and store the results
  if (filtered.res.mes$type == 1) {
    # If the measure is type 1, the best value is one
    res <- friedman.nemenyi(data = data, 
                            save = save.path,
                            measure.name = measure.name,
                            width = 60, 
                            height = 30,
                            cex = 5.5)
    
    # data, save.dir, measure.name, width = 7, height = 5, cex = 1.2
    
  } else {
    # If the measure is type 0, the best value is zero
    res.data <- data.frame(ranking$rank.average.1)
    res <- friedman.nemenyi(data = res.data, 
                            save = save.path,
                            measure.name = measure.name,
                            width = 60, 
                            height = 30,
                            cex = 5.5)
  }
  
  # Concatenate the result to the all.results data frame
  all.results <- rbind(all.results, res)
  
  methods.names <- colnames(data)  
  generate.boxplots(data = data,
                    methods = methods.names, 
                    save.dir = save.path,
                    measure.name = measure.name,
                    width = 20,
                    height = 10)
  
  # Log the processed file
  cat("\nProcessed file:", data.name, "\n")
}


measures <- tools::file_path_sans_ext(basename(files))
nome.arquivo = paste(FolderResults, "/FN-results.xlsx", sep="")
res = data.frame(measures, all.results)
write_xlsx(res, nome.arquivo)




##############################################################################
# END OF SCRIPT
##############################################################################
