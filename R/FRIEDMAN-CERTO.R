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
FolderCD = "~/Multi-Label-Friedman-Nemenyi/Analysed/Set-Up-7-e/CriticalDistance"


# MY METHODS TO COMPARE
my.methods = c("Global", "Local", "ECC", "Standard", "Label", 
               "Cluster", "Complete")

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

Folder.CSVs.2 = paste(FolderCC, "/CSVs2", sep="")
pastas$FolderCSVs2 = Folder.CSVs.2
folder.names.csv.2 = c(dir(Folder.CSVs.2))
nomes.2 = RemoveCSV(folder.names.csv.2)


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

chisquare = c(0)
ChiSquare = data.frame(chisquare)

fpvalues = c(0)
FPValue = data.frame(fpvalues )

degre = c(0)
Degreess = data.frame(degre )

critical.difference = c(0)
CriticalDifference = data.frame(critical.difference)

Measure = c("")
NPValue = c(0)
npValue = data.frame(Measure, NPValue)

FPValues = c(0)
fpValues = data.frame(Measure, FPValues)

Hypothesis = c("")
Hypho = data.frame(Measure, Hypothesis)

CriticalDistance = c(0)
CD = data.frame(Measure, CriticalDistance)

ConfidenceLevel = c(0)
CL = data.frame(Measure, ConfidenceLevel)

Treat = c(0)
treatments = data.frame(Measure, Treat)

Obs = c(0)
Observations = data.frame(Measure, Obs)

ChiSquare = c(0)
CSq = data.frame(Measure, ChiSquare)

Degrees = c(0)
Dg = data.frame(Measure, Degrees)

i=1
while(i<length(datasets.names)){
  
  cat("\n", i, " - ", nomes[i])
  
  nome.file = paste(Folder.CSVs, "/", folder.names.csv[i], sep="")
  data.info = data.frame(read.csv(nome.file))
  data.info.2 = data.info[,-1]
  names(data.info.2) = my.methods
  
  fr = friedmanTest(data.info.2)
  name = paste(FolderCD, "/s-", nomes[i], "-friedman.txt", sep="")
  sink(name)
  print(fr)
  sink()
  
  ne = nemenyiTest(data.info.2, alpha=0.05)
  name = paste(FolderCD, "/s-", nomes[i], "-nemenyi.txt", sep="")
  sink(name)
  print(fr)
  sink()
  
  pdf(paste(FolderCD, "/s-", nomes[i], ".pdf", sep=""), 
      width = 7, height = 3)
  print(plotCD(data.info.2, alpha=0.05, cex=1))
  dev.off()
  
  png(paste(FolderCD, "/s-", nomes[i], ".png", sep=""),  
      width = 1280, height = 720, res = 100)
  print(plotCD(data.info.2, alpha=0.05, cex=1, main=names2[i]))
  dev.off()
  
  pdf(paste(FolderCD, "/s-", nomes[i], "-pValues.pdf", sep=""), 
      width = 10, height = 6)
  print(plotPvalues(ne$diff.matrix, show.pvalue = TRUE, font.size = 2))
  dev.off()
  
  CS = as.numeric(fr$statistic)
  ChiSquare = rbind(ChiSquare, CS)
  
  dg = as.numeric(fr$parameter)
  Degreess = rbind(Degreess, dg)
  
  fpv = as.numeric(fr$p.value)
  FPValue = rbind(FPValue, fpv)

  CrDi = ne$statistic
  CriticalDifference = rbind(CriticalDifference, CrDi)
  
  i = i + 1
  gc()
}

setwd(FolderCD)
values = cbind(ChiSquare, Degreess, FPValue, CriticalDifference)
write.csv(values[-1,], "s-results.csv", row.names = FALSE)




