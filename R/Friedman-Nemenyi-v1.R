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
directories.1 <- function(name.dir, root.dir){
  
  retorno = list()
  
  folder = paste(root.dir,"/F-N", sep="")
  if(dir.exists(folder)==FALSE){dir.create(folder)}
  
  FolderFN = paste(folder, "/", name.dir, sep="")
  if(dir.exists(FolderFN)==FALSE){dir.create(FolderFN)}
  
  FolderFNP = paste(FolderFN, "/Plots", sep="")
  if(dir.exists(FolderFNP)==FALSE){dir.create(FolderFNP)}
  
  FolderCD = paste(FolderFNP, "/CriticalDistances", sep="")
  if(dir.exists(FolderCD)==FALSE){dir.create(FolderCD)}
  
  FolderIntervals = paste(FolderFN, "/Intervals", sep="")
  if(dir.exists(FolderIntervals)==FALSE){dir.create(FolderIntervals)}
  
  FolderNPValue = paste(FolderFN, "/NPValues", sep="")
  if(dir.exists(FolderNPValue)==FALSE){dir.create(FolderNPValue)}
  
  FolderFPValues = paste(FolderFN, "/FPValues", sep="")
  if(dir.exists(FolderFPValues)==FALSE){dir.create(FolderFPValues)}
  
  FolderHypo = paste(FolderFN, "/Hypothesis", sep="")
  if(dir.exists(FolderHypo)==FALSE){dir.create(FolderHypo)}
  
  FolderCL = paste(FolderFN, "/ConfidenceLevel", sep="")
  if(dir.exists(FolderCL)==FALSE){dir.create(FolderCL)}
  
  FolderT = paste(FolderFN, "/Treat", sep="")
  if(dir.exists(FolderT)==FALSE){dir.create(FolderT)}
  
  FolderO = paste(FolderFN, "/Observations", sep="")
  if(dir.exists(FolderO)==FALSE){dir.create(FolderO)}
  
  FolderChiS = paste(FolderFN, "/ChiSquare", sep="")
  if(dir.exists(FolderChiS)==FALSE){dir.create(FolderChiS)}
  
  FolderDegrees = paste(FolderFN, "/Degrees", sep="")
  if(dir.exists(FolderDegrees)==FALSE){dir.create(FolderDegrees)}
  
  FolderTXTs = paste(FolderFN, "/TXTs", sep="")
  if(dir.exists(FolderTXTs)==FALSE){dir.create(FolderTXTs)}
  
  
  retorno$FolderFN = FolderFN
  retorno$FolderFNP = FolderFNP
  retorno$FolderCD = FolderCD
  retorno$FolderIntervals = FolderIntervals
  retorno$FolderNPValue = FolderNPValue
  retorno$FolderFPValues = FolderFPValues
  retorno$FolderHypo = FolderHypo
  retorno$FolderCL = FolderCL
  retorno$FolderT = FolderT
  retorno$FolderO = FolderO
  retorno$FolderChiS = FolderChiS
  retorno$FolderDegrees = FolderDegrees
  retorno$FolderTXTs = FolderTXTs
  return(retorno)
}


##############################################################################
# RANK =  caminho absoluto do file
# paste(FolderRanks, "/", files[i], sep="")
# NAMES = apenas o name do file
##############################################################################
Friedman.Nemenyi.v1 <- function(path, name.dir, root.dir, title){
  
  retorno = list()
  
  folders = directories.1(name.dir,  root.dir)
  
  means = data.frame()
  
  Measure = c("")
  NPValue = c(0)
  npValue = data.frame(Measure, NPValue)
  
  Measure = c("")
  FPValues = c(0)
  fpValues = data.frame(Measure, FPValues)
  
  Measure = c("")
  Hypothesis = c("")
  Hypho = data.frame(Measure, Hypothesis)
  
  Measure = c("")
  CriticalDistance = c(0)
  CD = data.frame(Measure, CriticalDistance)
  
  Measure = c("")
  ConfidenceLevel = c(0)
  CL = data.frame(Measure, ConfidenceLevel)
  
  Measure = c("")
  Treat = c(0)
  treatments = data.frame(Measure, Treat)
  
  Measure = c("")
  Obs = c(0)
  Observations = data.frame(Measure, Obs)
  
  Measure = c("")
  ChiSquare = c(0)
  CSq = data.frame(Measure, ChiSquare)
  
  Measure = c("")
  Degrees = c(0)
  Dg = data.frame(Measure, Degrees)
  
  rank = c(dir(path))
  names = c(RemoveCSV(rank))
  names2 = c(RemoveR(rank))
  
  names = names[c(-4,-32)]
  names2 = names2[c(-4,-32)]
  
  i = 1
  while(i<=length(names)){
    
    cat("\n", i, " - ", names[i])
    
    file = paste(path, "/", rank[i], sep="")
    Ranking = data.frame(read.csv(file))
    Ranking[is.na(Ranking)] <- 0
    Ranking = as.matrix(Ranking)
    
    # FRIEDMAN
    f = friedman.test(Ranking)
    name.1 = paste(folders$FolderTXTs,
                   "/", names2[i], "-friedman.txt", sep="")
    sink(name.1)
    print(f)
    sink()
    
    # NEMENYI
    setwd(folders$FolderCD)
    pdf(paste(names2[i], ".pdf", sep=""), width = 10, height = 5)
    n = nemenyi(Ranking, conf.level = 0.95, 
                plottype='vline', sort = TRUE, main = names2[i])
    dev.off()
    # main = paste(title, " ", names2[i], sep="")
    
    setwd(folders$FolderCD)
    png(paste(names2[i], ".png", sep=""), 
        width = 1280, height = 720, res = 100)
    n = nemenyi(Ranking, conf.level = 0.95, plottype='vline', 
                main = names2[i], sort = TRUE)
    dev.off()
    # main=paste(title, " ", names2[i], sep="")
    
    name2 = paste(folders$FolderTXTs,
                  "/", names2[i], "-nemenyi.txt", sep="")
    sink(name2)
    print(n)
    sink()
    
    setwd(folders$FolderIntervals)
    write.csv2(n$intervals, paste(names2[i],
                                  "-Intervals.csv", sep=""),
               row.names = FALSE)
    
    setwd(folders$FolderNPValue)
    Measure = names2[i]
    NPValue = n$fpval
    npValue = rbind(npValue, data.frame(Measure, NPValue))
    write.csv2(npValue[-1,], "NPValues.csv", row.names = FALSE)
    
    setwd(folders$FolderHypo)
    Hypothesis = n$fH
    Hypho = rbind(Hypho, data.frame(Measure, Hypothesis))
    write.csv2(Hypho[-1,], "hypothesis.csv", row.names = FALSE)
    
    setwd(folders$FolderCD)
    CriticalDistance = n$cd
    CD = rbind(CD, data.frame(Measure, CriticalDistance))
    write.csv2(CD[-1,], "CriticalDistance.csv", row.names = FALSE)
    
    setwd(folders$FolderCL)
    ConfidenceLevel = n$conf.level
    CL = rbind(CL, data.frame(Measure, ConfidenceLevel))
    write.csv2(CL[-1,], "ConfidenceLevel.csv", row.names = FALSE)
    
    setwd(folders$FolderT)
    Treat = n$k
    treatments = rbind(treatments, data.frame(Measure, Treat))
    write.csv2(treatments[-1,], "Treatments.csv", row.names = FALSE)
    
    setwd(folders$FolderO)
    Obs = n$n
    Observations = rbind(Observations, data.frame(Measure, Obs))
    write.csv2(Observations[-1,], "Observations.csv", row.names = FALSE)
    
    setwd(folders$FolderChiS)
    ChiSquare = as.numeric(f$statistic)
    CSq = rbind(CSq, data.frame(Measure, ChiSquare))
    write.csv2(CSq[-1,], "ChiSquare.csv", row.names = FALSE)
    
    setwd(folders$FolderDegrees)
    Degrees = as.numeric(f$parameter)
    Dg = rbind(Dg, data.frame(Measure, Degrees))
    write.csv2(Dg[-1,], "Degrees.csv", row.names = FALSE)
    
    setwd(folders$FolderFPValues)
    FPValues = as.numeric(f$p.value)
    fpValues = rbind(fpValues, data.frame(Measure, FPValues))
    write.csv2(fpValues[-1,], "FPValues.csv", row.names = FALSE)
    
    i = i + 1
    gc()
  }
  
  ChiSquare = CSq[-1,]
  Nemenyi = npValue[-1,]
  Friedman = fpValues[-1,]
  Hyphotesis = Hypho[-1,]
  Critical.Distances = CD[-1,]
  Treatments = treatments[-1,]
  Observations = Observations[-1,]
  Confidence.Levels = CL[-1,]
  Degrees = Dg[-1,]
  
  values = cbind(ChiSquare,
                 Nemenyi = Nemenyi$NPValue,
                 Friedman = Friedman$FPValues,
                 Hyphotesis = Hyphotesis$Hypothesis,
                 Critical.Distances = Critical.Distances$CriticalDistance,
                 Treatments = Treatments$Treat,
                 Observations = Observations$Obs,
                 Confidence.Levels = Confidence.Levels$ConfidenceLevel,
                 Degrees = Degrees$Degrees)
  
  setwd(folders$FolderFN)
  write.csv(values, "results.csv", row.names = FALSE)
  
  retorno$values = values
  return(retorno)
  
}


##############################################################################
#
##############################################################################