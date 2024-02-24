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
#
##############################################################################
datasets.names = c("emotions", "flags","GpositiveGO", "GpositivePseAAC",
                   "scene", "VirusGO", "VirusPseAAC", "Yelp")


##############################################################################
#
##############################################################################
my.methods.ma = c("G", "Lo",
                  "H.jmi", "H.js",
                  "H.kmi", "H.ks",
                  "E.mi", "O.mi",
                  "R1.mi", "R1.s",
                  "R2.mi", "R2.s", "R3")


##############################################################################
#
##############################################################################
res.mm = multilabel.measures.1()
measures = data.frame(res.mm$measures)


##############################################################################
#
##############################################################################
pastas = list()

FolderData = paste(FolderRoot, "/Data", sep="")
pastas$FolderData = FolderData
folder.names = dir(FolderData)

FolderSetUp = paste(FolderData, "/Set-Up-2", sep="")
pastas$FolderSetUp = FolderSetUp
folder.names.setup = c(dir(FolderSetUp))

Folder.CSVs = paste(FolderSetUp, "/Micro-F1-CSVs", sep="")
pastas$FolderCSVs = Folder.CSVs
folder.names.csv = c(dir(Folder.CSVs))
nomes.measures = RemoveCSV(folder.names.csv)

FolderAnalysed = paste(FolderRoot, "/Analysed", sep="")
if(dir.exists(FolderAnalysed)==FALSE){dir.create(FolderAnalysed)}
pastas$FolderAnalysed = FolderAnalysed

FolderAbril2023 = paste(FolderAnalysed, "/Set-Up-2-Micro-F1", sep="")
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


#############################################################################
#
##############################################################################
resultado.mi = ranking.for.all.measures(folder.names.csv = folder.names.csv, 
                                       nomes.measures = nomes.measures,
                                       my.methods = my.methods.mi, 
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
                                   my.methods = my.methods.mi, 
                                   nomes.measures = nomes.measures, 
                                   res.mm = res.mm)

ra = total.performance.method.2(folder.names.csv = folder.names.csv,
                                Folder.CSVs = Folder.CSVs, 
                                nomes = nomes.measures,
                                FolderMethods = FolderMethods2,
                                my.methods = my.methods.mi, 
                                res.mm = res.mm)


##############################################################################
# WIN TIE LOSS
##############################################################################
compute.all.win.loss.tie(folder.names.csv = folder.names.csv,
                         Folder.CSVs = Folder.CSVs, 
                         FolderWLT = FolderWLT,
                         my.methods = my.methods.mi, 
                         res.mm = res.mm)


medidas = multilabel.measures.1()

WinLossTiePlotMi(FolderWLT = FolderWLT, 
                 max.value = 120, 
                 half.value = 48, 
                 measures = medidas, 
                 title = "",
                 idioma = "pt")






