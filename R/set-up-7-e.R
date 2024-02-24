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
source("Friedman-Nemenyi-v1.R")

setwd(FolderScripts)
source("Friedman-Nemenyi-v2.R")

setwd(FolderScripts)
source("pair-methods-comparison.R")


##############################################################################
# CALLING MULTILABEL MEASURES FUNCTION - UTILS.R
##############################################################################
res.mm = multilabel.measures()
measures = res.mm$measures


# MY METHODS TO COMPARE
my.methods = c("Global", "Local", "ECC", "Standard", "Label", 
               "Cluster", "Complete")


##############################################################################
# Generates rankings to be used in tests
##############################################################################
pastas = list()

FolderData = paste(FolderRoot, "/Data", sep="")
pastas$FolderData = FolderData
folder.names = dir(FolderData)

FolderCC = paste(FolderData, "/Set-Up-7-e", sep="")
pastas$FolderCC = FolderCC
folder.names.cc = c(dir(FolderCC))

Folder.CSVs = paste(FolderCC, "/CSVs", sep="")
pastas$FolderCSVs = Folder.CSVs
folder.names.csv = c(dir(Folder.CSVs))
nomes = RemoveCSV(folder.names.csv)

FolderCCA = paste(FolderRoot, "/Analysed", sep="")
if(dir.exists(FolderCCA)==FALSE){dir.create(FolderCCA)}
pastas$FolderCCA = FolderCCA

Folder.2020 = paste(FolderCCA, "/Set-Up-7-e", sep="")
pastas$Folder.2020 = Folder.2020
if(dir.exists(Folder.2020)==FALSE){dir.create(Folder.2020)}

pasta.2 = paste(Folder.2020,"/Rankings", sep="")
pastas$pasta.2 = pasta.2
if(dir.exists(pasta.2)==FALSE){dir.create(pasta.2)}

pasta.3 = paste(Folder.2020,"/Rankings-2", sep="")
pastas$pasta.3 = pasta.3
if(dir.exists(pasta.3)==FALSE){dir.create(pasta.3)}

pasta.5 = paste(Folder.2020,"/All-Rankings", sep="")
pastas$pasta.5 = pasta.5
if(dir.exists(pasta.5)==FALSE){dir.create(pasta.5)}


datasets.names = c("bibtex", "birds", "emotions", 
                   "EukaryotePseAAC", "flags",          
                   "GnegativeGO", "GnegativePseAAC", 
                   "GpositiveGO", "GpositivePseAAC", 
                   "HumanGO", "HumanPseAAC", 
                   "langlog", "ng20", "ohsumed", 
                   "PlantGO", "PlantPseAAC", 
                   "scene", "slashdot", "tmc-2007-500",
                   "VirusGO", "VirusPseAAC", 
                   "yeast", "Yelp")
length(datasets.names)


resultado = ranking.for.all.measures(folder.names.csv, my.methods, pastas)


#############################################################################
# TEST FOR ALL METHODS
##############################################################################

# tsutils
name.dir = "All.v1"
root.dir = Folder.2020 
ranking.path = pasta.2
res1 = Friedman.Nemenyi.v1(path = ranking.path,
                           name.dir = name.dir, 
                           root.dir = root.dir)

# scmamp
rm(name.dir, root.dir, ranking.path)
name.dir = "All.v2"
root.dir = Folder.2020 
ranking.path = pasta.2
res2 = Friedman.Nemenyi.v2(ranking.path, name.dir, root.dir)


# tsutils
name.dir = "All.v3"
root.dir = Folder.2020 
ranking.path = "~/Multi-Label-Friedman-Nemenyi/Analysed/Set-Up-7-d/Rankings-2"
res1 = Friedman.Nemenyi.v1(path = ranking.path,
                           name.dir = name.dir, 
                           root.dir = root.dir)

# scmamp
rm(name.dir, root.dir, ranking.path)
name.dir = "All.v4"
root.dir = Folder.2020 
ranking.path = "~/Multi-Label-Friedman-Nemenyi/Analysed/Set-Up-7-d/Rankings-2"
res2 = Friedman.Nemenyi.v2(ranking.path, name.dir, root.dir)



##############################################################################
# FAZENDO COMPARAÇÃO ENTRE OS PARES DE MÉTODOS
##############################################################################

pasta.6 = paste(Folder.2020,"/Methods-Analysis", sep="")
if(dir.exists(pasta.6)==FALSE){dir.create(pasta.6)}

results = compare.for.all.measures(folder.names.csv,
                                   Folder.CSVs, 
                                   pasta.6, 
                                   my.methods)

pasta.7 = paste(Folder.2020,"/Methods-Analysis-2", sep="")
if(dir.exists(pasta.7)==FALSE){dir.create(pasta.7)}

ra = total.performance.method.2(folder.names.csv,
                                Folder.CSVs,
                                pasta.7, 
                                my.methods)

final = data.frame(apagar=c(0))
m = 1
while(m <= nrow(measures)){
  mea = measures[m,]
  Folder = paste("/home/biomal/Multi-Label-Friedman-Nemenyi/Analysed/Set-Up-7-d/Methods-Analysis/", 
                 mea$names, sep="")
  nome = paste(Folder, "/", mea$names, 
               "-maior-total-datasets-per-method.csv", sep="")
  arquivo = data.frame(read.csv(nome))
  arquivo = data.frame(mea$names, arquivo)
  final = cbind(final, arquivo)
  m = m  + 1
  gc()
}

setwd(FolderCCA)
write.csv(final, "total.csv")



