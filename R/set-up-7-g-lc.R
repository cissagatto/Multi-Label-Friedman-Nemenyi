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
# CALLING MULTILABEL MEASURES FUNCTION - UTILS.R
##############################################################################
res.mm = multilabel.measures.3()
measures = res.mm$measures


##############################################################################
# MY  METHODS
##############################################################################
my.methods = c("Global", "Local", "ECC", "Standard", "Label", 
               "Cluster", "Complete")


##############################################################################
#
##############################################################################
pastas = list()

FolderData = paste(FolderRoot, "/Data", sep="")
pastas$FolderData = FolderData
folder.names = dir(FolderData)

FolderCC = paste(FolderData, "/Set-Up-7-g", sep="")
pastas$FolderCC = FolderCC
folder.names.cc = c(dir(FolderCC))

Folder.CSVs = paste(FolderCC, "/CSVs-Thr-LC", sep="")
pastas$FolderCSVs = Folder.CSVs
folder.names.csv = c(dir(Folder.CSVs))
nomes = RemoveCSV(folder.names.csv)

FolderCCA = paste(FolderRoot, "/Analysed", sep="")
pastas$FolderCCA = FolderCCA
if(dir.exists(FolderCCA)==FALSE){dir.create(FolderCCA)}

Folder7G = paste(FolderCCA, "/Set-Up-7-g", sep="")
pastas$Folder7G = Folder7G
if(dir.exists(Folder7G)==FALSE){dir.create(Folder7G)}

FolderThrLC = paste(Folder7G, "/ThrLC", sep="")
pastas$FolderThrLC  = FolderThrLC 
if(dir.exists(FolderThrLC )==FALSE){dir.create(FolderThrLC )}

FolderRankings = paste(FolderThrLC,"/Rankings", sep="")
pastas$FolderRankings = FolderRankings
if(dir.exists(FolderRankings)==FALSE){dir.create(FolderRankings)}

FolderAllRankings = paste(FolderThrLC,"/All-Rankings", sep="")
pastas$FolderAllRankings = FolderAllRankings
if(dir.exists(FolderAllRankings)==FALSE){dir.create(FolderAllRankings)}

FolderMediaRankings = paste(FolderThrLC,"/Media-Rankings", sep="")
pastas$FolderMediaRankings = FolderMediaRankings
if(dir.exists(FolderMediaRankings)==FALSE){dir.create(FolderMediaRankings)}

FolderMethods = paste(FolderThrLC,"/Methods-Analysis", sep="")
pastas$FolderMethods = FolderMethods
if(dir.exists(FolderMethods)==FALSE){dir.create(FolderMethods)}

FolderMethods2 = paste(FolderThrLC,"/Methods-Analysis-2", sep="")
pastas$FolderMethods2 = FolderMethods2
if(dir.exists(FolderMethods2)==FALSE){dir.create(FolderMethods2)}

FolderWLT = paste(FolderThrLC,"/Win-Loss-Tie", sep="")
pastas$FolderWLT = FolderWLT
if(dir.exists(FolderWLT)==FALSE){dir.create(FolderWLT)}


##############################################################################
#
##############################################################################
##############################################################################
#
##############################################################################
datasets.names = c("bibtex","cal500","corel16k001", "corel16k003",
                   "corel16k004", "corel16k005", "corel16k006",
                   "corel16k007", "corel16k008", "corel16k009",
                   "corel16k010", "rcv1sub1", "rcv1sub2", "stackex_chemistry")
length(datasets.names)


##############################################################################
#
##############################################################################
resultado = ranking.for.all.measures(folder.names.csv = folder.names.csv, 
                                     nomes.measures = nomes,
                                     my.methods = my.methods, 
                                     pastas = pastas, 
                                     res.mm = res.mm)


#############################################################################
# TEST FOR ALL METHODS
##############################################################################

# tsutils
# res1 = Friedman.Nemenyi.v1(path = FolderRankings, 
#                            name.dir = "All.v1", 
#                            root.dir =  FolderThrLC,
#                            title = "thLC")

# # scmamp
# res2 = Friedman.Nemenyi.v2(path = FolderRankings, 
#                            name.dir = "All.v2", 
#                            root.dir = FolderThrLC,
#                            title = "thLC")

# tsutils
res1 = Friedman.Nemenyi.v1(path = FolderAllRankings, 
                           name.dir = "All.v3", 
                           root.dir =  FolderThrLC,
                           title = "thLC")

# scmamp
res2 = Friedman.Nemenyi.v2(path = FolderAllRankings, 
                           name.dir = "All.v4", 
                           root.dir = FolderThrLC,
                           title = "thLC")


##############################################################################
# FAZENDO COMPARAÇÃO ENTRE OS PARES DE MÉTODOS
##############################################################################
results = compare.for.all.measures(folder.names.csv = folder.names.csv,
                                   Folder.CSVs = Folder.CSVs, 
                                   FolderMethods = FolderMethods,
                                   my.methods = my.methods, 
                                   nomes.measures = nomes, 
                                   res.mm = res.mm)

ra = total.performance.method.2(folder.names.csv = folder.names.csv,
                                Folder.CSVs = Folder.CSVs, 
                                FolderMethods = FolderMethods2,
                                my.methods = my.methods, 
                                res.mm = res.mm)


##############################################################################
# WIN TIE LOSS
##############################################################################

compute.all.win.loss.tie(folder.names.csv = folder.names.csv,
                         Folder.CSVs = Folder.CSVs, 
                         FolderWLT  = FolderWLT,
                         my.methods = my.methods, 
                         res.mm = res.mm)

medidas = multilabel.measures.3()
WinLossTiePlot(FolderWLT = FolderWLT, max.value = 100, half.value = 42, 
               measures = medidas, title = "thLC")




##############################################################################
#
##############################################################################
# final = data.frame(apagar=c(0))
# m = 1
# while(m <= nrow(measures)){
#   mea = measures[m,]
#   Folder = paste(FolderMethods, "/", mea$names, sep="")
#   nome = paste(Folder, "/", mea$names, 
#                "-maior-total-datasets-per-method.csv", sep="")
#   arquivo = data.frame(read.csv(nome))
#   arquivo = data.frame(mea$names, arquivo)
#   final = cbind(final, arquivo)
#   m = m  + 1
#   gc()
# }
# 
# setwd(FolderThrLC)
# write.csv(final, "total-datasets-methods.csv")

