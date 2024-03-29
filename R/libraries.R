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


# INSTALL THIS TO USE SCMAMP
#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("graph")
#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("Rgraphviz")
# 
# if (!require("devtools")) {
#   install.packages("devtools")
# }
# devtools::install_github("b0rxa/scmamp")
# 
# devtools::install_github("ricardo-bion/ggradar", 
#                          dependencies = TRUE)


library(gplots)
library(pheatmap)
library(ggstatsplot)
library(dplyr)
library(stringr)
library(tsutils)
library(scmamp)
library(rstatix)
library(ggradar)
library(RColorBrewer)
library(wesanderson)
library(tidyverse)
#library(Rgraphviz)
library(fmsb)
library(unikn)
library(ggplot2)
library(ggpubr)
library(viridis)
library(purrr)
library(scales)
library(tibble)



##############################################################################
#
##############################################################################
