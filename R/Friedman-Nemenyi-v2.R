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

#if (!require("devtools")) {
#  install.packages("devtools")
#}
#devtools::install_github("b0rxa/scmamp")

#install.packages("BiocManager")
#BiocManager::install("Rgraphviz")

##############################################################################
#
##############################################################################
diretorios_2 <- function(name_dir){
  
  retorno = list()
  
  pasta = paste(FolderRoot,"/Analysis", sep="")
  if(dir.exists(pasta)==FALSE){dir.create(pasta)}
  
  FolderSC = paste(pasta, "/", name_dir, sep="")
  if(dir.exists(FolderSC)==FALSE){  dir.create(FolderSC)}
  
  FolderPlots = paste(FolderSC, "/Plots", sep="")
  if(dir.exists(FolderPlots)==FALSE){dir.create(FolderPlots)}
  
  FolderCD = paste(FolderPlots, "/CriticalDistances", sep="")
  if(dir.exists(FolderCD)==FALSE){dir.create(FolderCD)}
  
  FolderDensity = paste(FolderPlots, "/Density", sep="")
  if(dir.exists(FolderDensity)==FALSE){dir.create(FolderDensity)}
  
  FolderNPV = paste(FolderPlots, "/NPV", sep="")
  if(dir.exists(FolderNPV)==FALSE){dir.create(FolderNPV)}
  
  FolderDiff = paste(FolderSC, "/Diff", sep="")
  if(dir.exists(FolderDiff)==FALSE){dir.create(FolderDiff)}
  
  FolderParam2 = paste(FolderSC, "/Parametros-N", sep="")
  if(dir.exists(FolderParam2)==FALSE){dir.create(FolderParam2)}
  
  FolderTXTs = paste(FolderSC, "/TXTs", sep="")
  if(dir.exists(FolderTXTs)==FALSE){dir.create(FolderTXTs)}
 
  retorno$FolderTXTs = FolderTXTs
  retorno$FolderSC = FolderSC
  retorno$FolderPlots = FolderPlots
  retorno$FolderDensity = FolderDensity
  retorno$FolderNPV = FolderNPV
  retorno$FolderDiff = FolderDiff
  retorno$FolderCD = FolderCD
  retorno$FolderParam2 = FolderParam2
  return(retorno) 
}


Friedman_Nemenyi_v2 <- function(path, name_dir){
  
  retorno = list()
  
  pastas2 = diretorios_2(name_dir)
  
  chisquare = c(0)
  ChiSquare = data.frame(chisquare)
  
  fpvalues = c(0)
  FPValue = data.frame(fpvalues )
  
  degre = c(0)
  Degreess = data.frame(degre )
  
  critical_difference = c(0)
  CriticalDifference = data.frame(critical_difference)
  
  rank = c(dir(path))
  names = c(RemoveCSV(rank))
  names2 = c(RemoveR(rank))
  
  i = 1
  while(i<=length(names)){
    
    cat("\n", i, " - ", names2[i])
    
    arquivo = paste(path, "/", rank[i], sep="")
    Ranking = data.frame(read.csv(arquivo))
    
    # friedman
    fr = friedmanTest(Ranking)
    nome = paste(pastas2$FolderTXTs, "/", names2[i], "-friedman.txt", sep="")
    sink(nome)
    print(fr)
    sink()
    
    # nemenyi
    ne = nemenyiTest(Ranking, alpha=0.05)
    nome = paste(pastas2$FolderTXTs, "/", names2[i], "-nemenyi.txt", sep="")
    sink(nome)
    print(ne)
    sink()
    
    setwd(pastas2$FolderCD)
    pdf(paste(names2[i], "-CD.pdf", sep=""), width = 14, height = 6)
    print(plotCD(Ranking, alpha=0.05, cex=1))
    dev.off()
    
    #setwd(pastas$FolderDensity)
    #pdf(paste(names[i], "-Density.pdf", sep=""), width = 10, height = 6)
    #print(plotDensities(Ranking2))
    #dev.off()
    
    setwd(pastas2$FolderNPV)
    pdf(paste(names2[i], "-pValues.pdf", sep=""), width = 10, height = 6)
    print(plotPvalues(ne$diff.matrix, show.pvalue = TRUE, font.size = 2))
    dev.off()
    
    # friedman
    CS = as.numeric(fr$statistic)
    ChiSquare = rbind(ChiSquare, CS)
    
    dg = as.numeric(fr$parameter)
    Degreess = rbind(Degreess, dg)
    
    fpv = as.numeric(fr$p.value)
    FPValue = rbind(FPValue, fpv)
    
    # nemenyi
    CrDi = ne$statistic
    CriticalDifference = rbind(CriticalDifference, CrDi)
    
    setwd(pastas2$FolderParam2)
    write.csv2(ne$parameter, paste(names2[i], "-parameter.csv", sep=""))
    
    setwd(pastas2$FolderDiff)
    write.csv2(ne$diff.matrix, paste(names2[i], "-diff.csv", sep=""))
    
    i = i + 1
    gc()
    
  }
  
  setwd(pastas2$FolderSC)
  values = cbind(ChiSquare, Degreess, FPValue, CriticalDifference)
  write.csv(values[-1,], "results.csv", row.names = FALSE)

  values = values[-1,]
  retorno$values = values
  return(retorno)  
}


