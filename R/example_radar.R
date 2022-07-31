###############################################################################
# MultiLabel Friedman Nemenyi                                                 #
# Copyright (C) 2022                                                          #
#                                                                             #
# This code is free software: you can redistribute it and/or modify it under  #
# the terms of the GNU General Public License as published by the Free        #
# Software Foundation, either version 3 of the License, or (at your option)   #
# any later version. This code is distributed in the hope that it will be     #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of      #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General    #
# Public License for more details.                                            #
#                                                                             #
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin  #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) Campus   #
# Sao Carlos | Computer Department (DC: https://site.dc.ufscar.br/)           #
# Program of Post Graduation in Computer Science                              #
# (PPG-CC: http://ppgcc.dc.ufscar.br/) Bioinformatics and Machine Learning    #
# Group (BIOMAL: http://www.biomal.ufscar.br/)                                #                                                                                                #
###############################################################################


##############################################################################
#
##############################################################################
FolderRoot = "~/Multi-Label-Friedman-Nemenyi"
FolderScripts = "~/Multi-Label-Friedman-Nemenyi/R"


##############################################################################
#
##############################################################################
setwd(FolderScripts)
source("libraries.R")

setwd(FolderScripts)
source("utils.R")

setwd(FolderScripts)
source("radar_plots.R")


##############################################################################
# CALLING MULTILABEL MEASURES FUNCTION - UTILS.R
##############################################################################
measures = multilabel_measures()


##############################################################################
# DIRECTORIES
##############################################################################
FolderPlots = "~/Multi-Label-Friedman-Nemenyi/RadarPlots"
if(dir.exists(FolderPlots)==FALSE){dir.create(FolderPlots)}

Folder_I = paste(FolderPlots, "/Individual", sep="")
if(dir.exists(Folder_I)==FALSE){dir.create(Folder_I)}

Folder_A = paste(FolderPlots, "/All", sep="")
if(dir.exists(Folder_A)==FALSE){dir.create(Folder_A)}

FolderData = paste(FolderRoot, "/Data", sep="")
folder_names = dir(FolderData)

Folder = paste(FolderData, "/", folder_names[2], sep="")
files_names = c(dir(Folder))
files_measures_names = RemoveCSV(files_names)

# MY METHODS TO COMPARE
my_methods = c("Datasets", "G.C", "L.C", 
                    "H.JmaC", "H.JmiC", "H.JSC", 
                    "H.KmaC", "H.KmiC", "H.KSC",
                    "E.MaC", "E.MiC", 
                    "O.MaC", "O.MiC", 
                    "R1.MaC", "R1.MiC", "R1.SC", 
                    "R2.MaC", "R2.MiC", "R2.SC", 
                    "R3.C")

# FOR THE FIRST MEASURE TO THE LAST
a = 1
while(a<=length(files_names)){
  
  setwd(FolderData)
  cat("\nMeasure: \t", files_names[a])

  # open file
  str = paste(Folder, "/", files_names[a], sep="")
  data = data.frame(read.csv2(str))
  
  # changing coluns names
  colnames(data) = my_methods
  
  # I dont need the 3 e 4 rows 
  data = data[c(-3,-4),]
  
  # getting the datasets names
  datasets_names = data$Datasets
  
  # I dont need the first coluns
  data = data[,-1]
  
  # total coluns
  b = ncol(data)
  
  # build the data frame with max and min
  d <- rbind(rep(1.0,b), rep(0.0,b), data)
  rownames(d) = c("max", "min", datasets_names)
  
  Folder_1 = paste(Folder_I, "/", measures$names[a], sep="")
  if(dir.exists(Folder_1)==FALSE){dir.create(Folder_1)}
 
  cat("\n PRINT INDIVIDUALLY - ONE RADAR FOR EACH DATASET")
  radar_plot_1(length(datasets_names), d, datasets_names, Folder_1)
  
  Folder_2 = paste(Folder_A, "/",  measures$names[a], sep="")
  if(dir.exists(Folder_2)==FALSE){dir.create(Folder_2)}
  
  cat("\n PRINT ALL DATASET IN ONE PLOT")
  radar_plot_2(4,2, df1, datasets_names, Folder_2)
  
  a = a + 1
  gc()
}
