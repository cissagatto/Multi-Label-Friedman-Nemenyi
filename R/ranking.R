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
generate.ranking <- function(data){
  
  retorno = list()
  coluna = ncol(data)
  linha = nrow(data)
  
  rank.first.0 = data.frame()
  rank.last.0 = data.frame()
  rank.average.0 = data.frame()
  rank.random.0 = data.frame()
  rank.min.0 = data.frame()
  rank.max.0 = data.frame()
  
  i = 1
  for(i in 1:linha){
    rf = rank(data[i,], ties.method = "first")     # first occurrence wins
    rl = rank(data[i,], ties.method = "last")      # last occurrence wins
    rav = rank(data[i,], ties.method = "average")   # média
    ran = rank(data[i,], ties.method = "random")    # ordem aleatória
    rma = rank(data[i,], ties.method = "max")       # máximo
    rmi = rank(data[i,], ties.method = "min")       # mínimo
    
    rank.first.0 = rbind(rank.first.0, rf)
    rank.last.0 = rbind(rank.last.0, rl)
    rank.average.0 = rbind(rank.average.0, rav)
    rank.random.0 = rbind(rank.random.0, ran)
    rank.max.0 = rbind(rank.max.0, rma)
    rank.min.0 = rbind(rank.min.0, rmi)
    
  }
  
  colnames(rank.first.0) = colnames(data)
  colnames(rank.last.0) = colnames(data)
  colnames(rank.average.0) = colnames(data)
  colnames(rank.random.0) = colnames(data)
  colnames(rank.max.0) = colnames(data)
  colnames(rank.min.0) = colnames(data)
  
  
  rank.first.1 = data.frame()
  rank.last.1 = data.frame()
  rank.average.1 = data.frame()
  rank.random.1 = data.frame()
  rank.min.1 = data.frame()
  rank.max.1 = data.frame()
  
  i = 1
  for(i in 1:linha){
    rf = (coluna -  rank.first.0[i,]) +1
    rl = (coluna - rank.last.0[i,]) +  1
    rav = (coluna - rank.average.0[i,]) + 1
    ran = (coluna - rank.random.0[i,]) + 1
    rma = (coluna - rank.max.0[i,]) + 1
    rmi = (coluna - rank.min.0[i,]) + 1
    
    rank.first.1 = rbind(rank.first.1, rf)
    rank.last.1 = rbind(rank.last.1, rl)
    rank.average.1 = rbind(rank.average.1, rav)
    rank.random.1 = rbind(rank.random.1, ran)
    rank.max.1 = rbind(rank.max.1, rma)
    rank.min.1 = rbind(rank.min.1, rmi)
  }
  
  colnames(rank.first.1) = colnames(data)
  colnames(rank.last.1) = colnames(data)
  colnames(rank.average.1) = colnames(data)
  colnames(rank.random.1) = colnames(data)
  colnames(rank.max.1) = colnames(data)
  colnames(rank.min.1) = colnames(data)
  
  rank.average.0 = trunc(rank.average.0,0)
  rank.average.1 = trunc(rank.average.1,0)
  
  retorno$rank.first.0 = rank.first.0
  retorno$rank.last.0 = rank.last.0
  retorno$rank.average.0 = rank.average.0
  retorno$rank.random.0 = rank.random.0
  retorno$rank.max.0 = rank.max.0
  retorno$rank.min.0 = rank.min.0
  
  retorno$rank.first.1 = rank.first.1
  retorno$rank.last.1 = rank.last.1
  retorno$rank.average.1 = rank.average.1
  retorno$rank.random.1 = rank.random.1
  retorno$rank.max.1 = rank.max.1
  retorno$rank.min.1 = rank.min.1
  
  return(retorno)
}

##############################################################################
#
##############################################################################
generates.rank.again <- function(tipo, 
                                 Folder.Origem, 
                                 Folder.Destino=Folders, 
                                 nomes.arquivos, 
                                 str){
  
  retorno = list()
  
  a = 1
  while(a<=length(nomes.arquivos)){
    
    nome = paste(Folder.Origem, "/", nomes.arquivos[a], sep="")
    
    # read the files and format the dataframe
    data = data.frame(read.csv2(nome))
    
    # replacing NA with zero
    data <- data %>% replace(is.na(.), 0)
    
    # data = data[c(-3,-4),]
    # names(data)[1] = "dataset"
    
    # retirando a primeira coluna
    data = data[,-1]
    
    # selecionando as colunas específicas
    data = data[,tipo]
    
    # generate the rankings for all csv files
    res = generate.ranking(data)
    
    # filtando por tipo d emedia
    val = filter(measures, measures$names == str[a])
    
    # criando string para salvar
    str.0 = paste(str[a], "-ranking.csv", sep="")  
    
    # create directory to store results
    pasta = paste(FolderRoot,"/Rankings", sep="")
    if(dir.exists(pasta)==FALSE){dir.create(pasta)}
    
    pasta1 = paste(pasta, "/", Folder.Destino, sep="")
    if(dir.exists(pasta1)==FALSE){dir.create(pasta1)}
    
    if(val$values==1){
      cat("\n1 \t", str[a])
      setwd(pasta1)
      write.csv(res$rank.average.1, str.0, row.names = FALSE)
      retorno$rank[[a]] = res$rank.average.1
      
    } else{
      cat("\n0 \t", str[a])
      setwd(pasta1)
      write.csv(res$rank.average.0, str.0, row.names = FALSE)  
      retorno$rank[[a]] = res$rank.average.0
    }
    
    a = a + 1
    gc()
    
  }
  
  return(retorno)
  
}

##############################################################################
#
##############################################################################
ranking.for.all.measures <- function(folder.names.csv, nomes.measures,
                                     my.methods, pastas, res.mm){
  
  retorno = list()
  
  apagar=c(0)
  todos.0 =  data.frame(apagar)
  todos.1 =  data.frame(apagar)
  
  measures = res.mm$measures

  a = 1
  while(a<=length(folder.names.csv)){
    
    # /home/cissa/Multi-Label-Friedman-Nemenyi/Data/Set-Up-2/
    # csvs_datasets_methods
    
    nome = paste(pastas$FolderCSVs, "/", folder.names.csv[a], sep="")
    #cat("\n", nome)
    
    #cat("\nRead the files and format the dataframe")
    # use csv if the file uses comma
    # use csv2 if the file uses dot comma
    data = data.frame(read.csv(nome))
    #print(data)
    #cat("\n")
    
    #cat("\nI dont need the 1 colun")
    data = data[,-1]
    
    #cat("\nsubstituindo na por zero")
    data[is.na(data)] <- 0
    
    #cat("\nMy methods")
    colnames(data) = my.methods
    
    #cat("\nColuns total")
    total.col = ncol(data)
    #cat("\n", total.col)
    
    #cat("\nGenerate the rankings for all csv files")
    res = generate.ranking(data)
    #cat("\n")
    #print(res)
    
    #cat("\nWhat type of measure is??")
    val = data.frame(filter(measures, measures$names == nomes.measures[a]))
    # cat("\n")
    # print(val)
    
    #cat("\ncreate str")
    str = paste(nomes.measures[a], "-ranking.csv", sep="")  
    
    if(val$values==1){
      cat("\n1 \t", nomes.measures[a])
      # for this measures you must use rank.average.1
      # its equivalent to =ORDER.EQ(X;start:end;1) in excel
      setwd(pastas$FolderRankings)
      write.csv(res$rank.average.0, str, row.names = FALSE)
      
    } else{
      cat("\n0 \t",  nomes.measures[a])
      # for this measures you must use rank.min.0
      # its equivalent to =ORDER.EQ(X;start:end;0) in excel
      setwd(pastas$FolderRankings)
      write.csv(res$rank.average.1, str, row.names = FALSE)
    }
    
    
    setwd(pastas$FolderAllRankings)
    
    #cat("\nsalva1")
    str.3 = paste("0-", str, sep="")
    write.csv(res$rank.average.0, str.3, row.names = FALSE)
    
    #cat("\nsalva2")
    str.4 = paste("1-", str, sep="")
    write.csv(res$rank.average.1, str.4, row.names = FALSE)
    
    #cat("\nsalva3")
    setwd(pastas$FolderMediaRankings)
    #print(res$rank.average.0)
    res.0 = data.frame(apply(res$rank.average.0, 2, mean))
    #print(res.0)
    colnames(res.0) = "mean"
    str.2 = paste("0-media-", str, sep="")
    write.csv(res.0, str.2)
    todos.0 = cbind(todos.0, res.0)
    
    #cat("\nsalva4")
    res.1 = data.frame(apply(res$rank.average.1, 2, mean))
    #print(res.1)
    colnames(res.1) = "mean"
    str.2 = paste("1-media-", str, sep="")
    write.csv(res.1, str.2)
    # print(res.1)
    
    teste = data.frame(apply(res$rank.min.1 , 2, mean))
    
    #cat("\ntodos")
    todos.1 = cbind(todos.1, res.1)
    # print(todos.1)
    # cat("\nconta")
    a = a + 1
    gc()
  }
  
  setwd(pastas$FolderMediaRankings)
  
  #cat("\nTODOS 0")
  todos.0 = todos.0[,-1]
  names(todos.0) =  nomes.measures
  write.csv(todos.0, "todos-0.csv")
  
  #cat("\nTODOS 1")
  todos.1 = todos.1[,-1]
  names(todos.1) =  nomes.measures[a]
  write.csv(todos.1, "todos-1.csv")
  
  retorno$all.0 = todos.0
  retorno$all.1 = todos.1
  
  return(retorno)
}


##############################################################################
#
##############################################################################