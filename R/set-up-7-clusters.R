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
source("wilcoxon.R")

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
my.methods = c("Global", "Local", "ECC", "Cluster")


##############################################################################
#
##############################################################################
pastas = list()

FolderData = paste(FolderRoot, "/Data", sep="")
pastas$FolderData = FolderData
folder.names = dir(FolderData)

FolderST7= paste(FolderData, "/Set-Up-7", sep="")
pastas$FolderST7 = FolderST7
folder.names.st7 = c(dir(FolderST7))

FolderST7G = paste(FolderST7, "/Set-Up-7-g", sep="")
pastas$FolderST7G = FolderST7G
folder.names.st7g = c(dir(FolderST7G))

Folder.CSVs = paste(FolderST7G, "/CSVs-Thr-05-03", sep="")
pastas$FolderCSVs = Folder.CSVs
folder.names.csv = c(dir(Folder.CSVs))
nomes = RemoveCSV(folder.names.csv)

FolderAnalyzed = paste(FolderRoot, "/Analysed", sep="")
pastas$FolderAnalyzed = FolderAnalyzed
if(dir.exists(FolderAnalyzed)==FALSE){dir.create(FolderAnalyzed)}

Folder7G = paste(FolderAnalyzed, "/Set-Up-7-g", sep="")
pastas$Folder7G = Folder7G
if(dir.exists(Folder7G)==FALSE){dir.create(Folder7G)}

FolderThr05 = paste(Folder7G, "/Thr-05-03", sep="")
pastas$FolderThr05  = FolderThr05 
if(dir.exists(FolderThr05 )==FALSE){dir.create(FolderThr05 )}

FolderRankings = paste(FolderThr05,"/Rankings", sep="")
pastas$FolderRankings = FolderRankings
if(dir.exists(FolderRankings)==FALSE){dir.create(FolderRankings)}

FolderAllRankings = paste(FolderThr05,"/All-Rankings", sep="")
pastas$FolderAllRankings = FolderAllRankings
if(dir.exists(FolderAllRankings)==FALSE){dir.create(FolderAllRankings)}

FolderMediaRankings = paste(FolderThr05,"/Media-Rankings", sep="")
pastas$FolderMediaRankings = FolderMediaRankings
if(dir.exists(FolderMediaRankings)==FALSE){dir.create(FolderMediaRankings)}

FolderMethods = paste(FolderThr05,"/Methods-Analysis", sep="")
pastas$FolderMethods = FolderMethods
if(dir.exists(FolderMethods)==FALSE){dir.create(FolderMethods)}

FolderMethods2 = paste(FolderThr05,"/Methods-Analysis-2", sep="")
pastas$FolderMethods2 = FolderMethods2
if(dir.exists(FolderMethods2)==FALSE){dir.create(FolderMethods2)}

FolderWLT = paste(FolderThr05,"/Win-Loss-Tie", sep="")
pastas$FolderWLT = FolderWLT
if(dir.exists(FolderWLT)==FALSE){dir.create(FolderWLT)}

FolderWilcoxon = paste(FolderThr05,"/Wilcoxon", sep="")
pastas$FolderWilcoxon = FolderWilcoxon
if(dir.exists(FolderWilcoxon)==FALSE){dir.create(FolderWilcoxon)}


##############################################################################
#
##############################################################################
datasets.names = c("bibtex", "corel16k001", "corel16k003",
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
 res1 = Friedman.Nemenyi.v1(path = FolderRankings, 
                            name.dir = "All.v1", 
                            root.dir =  FolderThr05,
                            title = "")

# # scmamp
 res2 = Friedman.Nemenyi.v2(path = FolderRankings, 
                            name.dir = "All.v2", 
                            root.dir = FolderThr05,
                            title = "")

# tsutils
res1 = Friedman.Nemenyi.v1(path = FolderAllRankings, 
                           name.dir = "All.v3", 
                           root.dir =  FolderThr05,
                           title = "")

# scmamp
res2 = Friedman.Nemenyi.v2(path = FolderAllRankings, 
                           name.dir = "All.v4", 
                           root.dir = FolderThr05,
                           title = "")


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
                                nomes = nomes,
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
WinLossTiePlot(FolderWLT = FolderWLT, max.value = 50, half.value = 21, 
               measures = medidas, title = "")



##############################################################################
# WILCOXON TESTE
##############################################################################
wilcoxon.teste()




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
# setwd(FolderThr05)
# write.csv(final, "total-datasets-methods.csv")




