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
source("ranking.R")

setwd(FolderScripts)
source("win-loss-tie.R")

setwd(FolderScripts)
source("Friedman-Nemenyi-v1.R")

setwd(FolderScripts)
source("Friedman-Nemenyi-v2.R")

setwd(FolderScripts)
source("pair-methods-comparison.R")

##############################################################################
# DATASETS DO EXPERIMENTO                                                    #
##############################################################################
datasets.names = c("birds",
                   "cal500",
                   "cellcycle",
                   "derisi",
                   "eisen",
                   "emotions",
                   "EukaryotePseAAC",
                   "flags",
                   "gasch1",
                   "GnegativeGO",
                   "GpositiveGO",
                   "langlog",
                   "medical",
                   "pheno",
                   "PlantGO",
                   "scene",
                   "seq",
                   "VirusPseAAC",
                   "yeast",
                   "Yelp")
length(datasets.names)


##############################################################################
#
##############################################################################

##############################################################################
# MY METHODS TO COMPARE                                                      #
##############################################################################
my.methods = c("L", "G", 
               "H.Ra", "NH.Ra", 
               "H.J.K1", "H.Ro.K1", 
               "H.J.K2", "H.Ro.K2", 
               "H.J.K3", "H.Ro.K3", 
               "H.J.T0", "H.Ro.T0",
               "H.J.T1", "H.Ro.T1", 
               "NH.J.K1", "NH.Ro.K1",
               "NH.J.K2", "NH.Ro.K2", 
               "NH.J.K3", "NH.Ro.K3", 
               "NH.J.T0", "NH.Ro.T0", 
               "NH.J.T1", "NH.Ro.T1")
length(my.methods)




##############################################################################
#
##############################################################################
pastas = list()

FolderData = paste(FolderRoot, "/Data", sep="")
pastas$FolderData = FolderData
folder.names = dir(FolderData)

FolderSetUp = paste(FolderData, "/Set-Up-3", sep="")
pastas$FolderSetUp = FolderSetUp
folder.names.setup = c(dir(FolderSetUp))

Folder.CSVs = paste(FolderSetUp, "/CSVs", sep="")
pastas$FolderCSVs = Folder.CSVs
folder.names.csv = c(dir(Folder.CSVs))
nomes.measures = RemoveCSV(folder.names.csv)

Folder.CSVs.2 = paste(FolderSetUp, "/CSVs-2", sep="")
pastas$FolderCSVs.2 = Folder.CSVs.2
folder.names.csv.2 = c(dir(Folder.CSVs.2))
nomes.measures.2 = RemoveCSV(folder.names.csv)

FolderAnalysed = paste(FolderRoot, "/Analysed", sep="")
if(dir.exists(FolderAnalysed)==FALSE){dir.create(FolderAnalysed)}
pastas$FolderAnalysed = FolderAnalysed

FolderAbril2023 = paste(FolderAnalysed, "/Set-Up-3-Junho", sep="")
pastas$FolderAbril2023 = FolderAbril2023
if(dir.exists(FolderAbril2023)==FALSE){dir.create(FolderAbril2023)}

FolderStatistical = paste(FolderAbril2023,"/Statistical", sep="")
pastas$FolderStatistical = FolderStatistical
if(dir.exists(FolderStatistical)==FALSE){dir.create(FolderStatistical)}

FolderRankings = paste(FolderAbril2023,"/Rankings", sep="")
pastas$FolderRankings = FolderRankings
if(dir.exists(FolderRankings)==FALSE){dir.create(FolderRankings)}

FolderAllRankings = paste(FolderAbril2023,"/All-Rankings", sep="")
pastas$FolderAllRankings= FolderAllRankings
if(dir.exists(FolderAllRankings)==FALSE){dir.create(FolderAllRankings)}

FolderMediaRankings = paste(FolderAbril2023,"/Media-Rankings", sep="")
pastas$FolderMediaRankings = FolderMediaRankings 
if(dir.exists(FolderMediaRankings )==FALSE){dir.create(FolderMediaRankings)}

FolderMethods = paste(FolderAbril2023,"/Methods-Analysis", sep="")
pastas$FolderMethods = FolderMethods
if(dir.exists(FolderMethods)==FALSE){dir.create(FolderMethods)}

FolderMethods2 = paste(FolderAbril2023,"/Methods-Analysis-2", sep="")
pastas$FolderMethods2 = FolderMethods2
if(dir.exists(FolderMethods2)==FALSE){dir.create(FolderMethods2)}

FolderWLT = paste(FolderAbril2023,"/Win-Tie-Loss-Plots", sep="")
pastas$FolderWLT = FolderWLT
if(dir.exists(FolderWLT )==FALSE){dir.create(FolderWLT)}



##############################################################################
# CALLING MULTILABEL MEASURES FUNCTION - UTILS.R
##############################################################################
res.mm = multilabel.measures.1()
measures = res.mm$measures


#############################################################################
# CRIANDO OS RANKINGS
##############################################################################
resultado = ranking.for.all.measures(folder.names.csv = folder.names.csv, 
                                     nomes.measures = nomes.measures,
                                     my.methods = my.methods, 
                                     pastas = pastas, 
                                     res.mm = res.mm)

#############################################################################
# TEST FOR ALL METHODS
##############################################################################

# tsutils
res1 = Friedman.Nemenyi.v1(path = FolderRankings,
                           name.dir = "All.v1",
                           root.dir =  FolderStatistical,
                           title = "")

# scmamp
res2 = Friedman.Nemenyi.v2(path = FolderRankings,
                           name.dir = "All.v2",
                           root.dir = FolderStatistical,
                           title = "")

# tsutils
res1 = Friedman.Nemenyi.v1(path = FolderAllRankings, 
                           name.dir = "All.v3", 
                           root.dir =  FolderStatistical,
                           title = "")

# scmamp
res2 = Friedman.Nemenyi.v2(path = FolderAllRankings, 
                           name.dir = "All.v4", 
                           root.dir = FolderStatistical,
                           title = "")



##############################################################################
# FAZENDO COMPARAÇÃO ENTRE OS PARES DE MÉTODOS
##############################################################################
results = compare.for.all.measures(folder.names.csv = folder.names.csv,
                                   Folder.CSVs = Folder.CSVs, 
                                   FolderMethods = FolderMethods,
                                   my.methods = my.methods, 
                                   nomes.measures = nomes.measures, 
                                   res.mm = res.mm)

ra = total.performance.method.2(folder.names.csv = folder.names.csv,
                                Folder.CSVs = Folder.CSVs, 
                                nomes = nomes.measures,
                                FolderMethods = FolderMethods2,
                                my.methods = my.methods, 
                                res.mm = res.mm)


##############################################################################
# WIN TIE LOSS
##############################################################################
compute.all.win.loss.tie(folder.names.csv = folder.names.csv,
                         Folder.CSVs = Folder.CSVs, 
                         FolderWLT = FolderWLT,
                         my.methods = my.methods, 
                         res.mm = res.mm)


medidas = multilabel.measures.1()



WinLossTiePlotTodos(FolderWLT = FolderWLT, 
                    max.value = 520, 
                    half.value = 230, 
                    measures = medidas, 
                    title = "",
                    idioma = "pt")






