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
# Generates rankings to be used in tests
##############################################################################
pastas = list()

FolderData = paste(FolderRoot, "/Data", sep="")
pastas$FolderData = FolderData
folder.names = dir(FolderData)

FolderSetUp = paste(FolderData, "/Set-Up-1", sep="")
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

FolderAbril2023 = paste(FolderAnalysed, "/Set-Up-1-Abril-2023", sep="")
pastas$FolderAbril2023 = FolderAbril2023
if(dir.exists(FolderAbril2023)==FALSE){dir.create(FolderAbril2023)}

FolderRankings = paste(FolderAbril2023,"/Rankings", sep="")
pastas$FolderRankings = FolderRankings
if(dir.exists(FolderRankings)==FALSE){dir.create(FolderRankings)}

FolderAllRankings = paste(FolderAbril2023,"/All-Rankings", sep="")
pastas$FolderAllRankings= FolderAllRankings
if(dir.exists(FolderAllRankings)==FALSE){dir.create(FolderAllRankings)}

FolderMediaRankings = paste(FolderAbril2023,"/Media-Rankings", sep="")
pastas$FolderMediaRankings = FolderMediaRankings 
if(dir.exists(FolderMediaRankings )==FALSE){dir.create(FolderMediaRankings)}

FolderWTP = paste(FolderAbril2023,"/Win-Tie-Loss-Plots", sep="")
pastas$FolderWTP = FolderWTP 
if(dir.exists(FolderWTP )==FALSE){dir.create(FolderWTP)}


######################################################################
datasets.names = c("birds", "emotions", "EukaryotePseAAC",
                   "flags", "GpositiveGO", "PlantGO",
                   "scene", "VirusGO", "yeast", "Yelp")


######################################################################
res.mm = multilabel.measures.1()
measures = data.frame(res.mm$measures)


######################################################################
my.methods = c("HPML.A.c", "R1", "R2", "Lo", "G")


######################################################################
resultado = ranking.for.all.measures(folder.names.csv, nomes.measures, 
                                     my.methods, pastas, res.mm)



#############################################################################
# TEST FOR ALL METHODS
##############################################################################

# tsutils
name.dir = "All.v1"
root.dir = FolderAbril2023
ranking.path = FolderRankings
res1 = Friedman.Nemenyi.v1(ranking.path, name.dir, root.dir)

# scmamp
rm(name.dir, root.dir, ranking.path)
name.dir = "All.v2"
root.dir = FolderAbril2023
ranking.path = FolderRankings
res2 = Friedman.Nemenyi.v2(ranking.path, name.dir, root.dir)


# tsutils
rm(name.dir, root.dir, ranking.path)
name.dir = "All.v3"
root.dir = FolderAbril2023
ranking.path = FolderAllRankings
res3 = Friedman.Nemenyi.v1(ranking.path, name.dir, root.dir)


# scmamp
rm(name.dir, root.dir, ranking.path)
name.dir = "All.v4"
root.dir = FolderAbril2023
ranking.path = FolderAllRankings
res4 = Friedman.Nemenyi.v2(ranking.path, name.dir, root.dir)



##############################################################################
# FAZENDO COMPARAÇÃO ENTRE OS PARES DE MÉTODOS
##############################################################################

FolderMethods = paste(FolderAbril2023,"/Methods-Analysis", sep="")
if(dir.exists(FolderMethods)==FALSE){dir.create(FolderMethods)}
pastas$FolderMethods = FolderMethods

results = compare.for.all.measures(folder.names.csv, Folder.CSVs, 
                                   FolderMethods, my.methods, 
                                   nomes.measures, res.mm)


ra = total.performance.method(folder.names.csv, Folder.CSVs, 
                              FolderMethods, my.methods, 
                              nomes.measures, res.mm)


##############################################################################
# WIN TIE LOSS
##############################################################################
compute.all.win.loss.tie(folder.names.csv = folder.names.csv,
                         Folder.CSVs = Folder.CSVs, 
                         FolderWLT = FolderWTP,
                         my.methods = my.methods, 
                         res.mm = res.mm)

medidas = multilabel.measures.1()

WinLossTiePlotTodos(FolderWLT = FolderWTP, 
                    max.value = 50, 
                    half.value = 20,
                    measures = medidas, 
                    title = "", 
                    idioma = "pt")





