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
generate_ranking <- function(data){
  
  retorno = list()
  coluna = ncol(data)
  linha = nrow(data)
  
  rank_first_0 = data.frame()
  rank_last_0 = data.frame()
  rank_average_0 = data.frame()
  rank_random_0 = data.frame()
  rank_min_0 = data.frame()
  rank_max_0 = data.frame()
  
  i = 1
  for(i in 1:linha){
    rf = rank(data[i,], ties.method = "first")     # first occurrence wins
    rl = rank(data[i,], ties.method = "last")      # last occurrence wins
    rav = rank(data[i,], ties.method = "average")   # média
    ran = rank(data[i,], ties.method = "random")    # ordem aleatória
    rma = rank(data[i,], ties.method = "max")       # máximo
    rmi = rank(data[i,], ties.method = "min")       # mínimo
    
    rank_first_0 = rbind(rank_first_0, rf)
    rank_last_0 = rbind(rank_last_0, rl)
    rank_average_0 = rbind(rank_average_0, rav)
    rank_random_0 = rbind(rank_random_0, ran)
    rank_max_0 = rbind(rank_max_0, rma)
    rank_min_0 = rbind(rank_min_0, rmi)
    
  }
  
  colnames(rank_first_0) = colnames(data)
  colnames(rank_last_0) = colnames(data)
  colnames(rank_average_0) = colnames(data)
  colnames(rank_random_0) = colnames(data)
  colnames(rank_max_0) = colnames(data)
  colnames(rank_min_0) = colnames(data)
  
  
  rank_first_1 = data.frame()
  rank_last_1 = data.frame()
  rank_average_1 = data.frame()
  rank_random_1 = data.frame()
  rank_min_1 = data.frame()
  rank_max_1 = data.frame()
  
  i = 1
  for(i in 1:linha){
    rf = (coluna -  rank_first_0[i,]) +1
    rl = (coluna - rank_last_0[i,]) +  1
    rav = (coluna - rank_average_0[i,]) + 1
    ran = (coluna - rank_random_0[i,]) + 1
    rma = (coluna - rank_max_0[i,]) + 1
    rmi = (coluna - rank_min_0[i,]) + 1
    
    rank_first_1 = rbind(rank_first_1, rf)
    rank_last_1 = rbind(rank_last_1, rl)
    rank_average_1 = rbind(rank_average_1, rav)
    rank_random_1 = rbind(rank_random_1, ran)
    rank_max_1 = rbind(rank_max_1, rma)
    rank_min_1 = rbind(rank_min_1, rmi)
  }
  
  colnames(rank_first_1) = colnames(data)
  colnames(rank_last_1) = colnames(data)
  colnames(rank_average_1) = colnames(data)
  colnames(rank_random_1) = colnames(data)
  colnames(rank_max_1) = colnames(data)
  colnames(rank_min_1) = colnames(data)
  
  rank_average_0 = trunc(rank_average_0,0)
  rank_average_1 = trunc(rank_average_1,0)
  
  retorno$rank_first_0 = rank_first_0
  retorno$rank_last_0 = rank_last_0
  retorno$rank_average_0 = rank_average_0
  retorno$rank_random_0 = rank_random_0
  retorno$rank_max_0 = rank_max_0
  retorno$rank_min_0 = rank_min_0
  
  retorno$rank_first_1 = rank_first_1
  retorno$rank_last_1 = rank_last_1
  retorno$rank_average_1 = rank_average_1
  retorno$rank_random_1 = rank_random_1
  retorno$rank_max_1 = rank_max_1
  retorno$rank_min_1 = rank_min_1
  
  return(retorno)
}

##############################################################################
#
##############################################################################
gera_rank_again <- function(tipo, Folder_Origem, 
                            Folder_Destino, nomes_arquivos, 
                            str){
  
  a = 1
  while(a<=length(arquivos)){
    
    nome = paste(Folder_Origem, "/", nomes_arquivos[a], sep="")
    
    # read the files and format the dataframe
    data = data.frame(read.csv(nome))
    data = data[c(-3,-4),]
    names(data)[1] = "dataset"
    data = data[,-1]
    
    # selecionando as colunas específicas
    data = data[,tipo]
    
    # generate the rankings for all csv files
    res = generate_ranking(data)
    
    # filtando por tipo d emedia
    val = filter(measures, measures$names == str[a])
    
    # criando string para salvar
    str_ = paste(str[a], "-ranking.csv", sep="")  
    
    # create directory to store results
    pasta = paste(FolderRoot,"/Rankings", sep="")
    if(dir.exists(pasta)==FALSE){dir.create(pasta)}
    
    pasta1 = paste(pasta, "/", Folder_Destino, sep="")
    if(dir.exists(pasta1)==FALSE){dir.create(pasta1)}
    
    if(val$values==1){
      cat("\n1 \t", str[a])
      setwd(pasta1)
      write.csv(res$rank_average_1, str_, row.names = FALSE)
      
    } else{
      cat("\n0 \t", str[a])
      setwd(pasta1)
      write.csv(res$rank_average_0, str_, row.names = FALSE)  
    }
    
    a = a + 1
    gc()
    
  }
  
}


##############################################################################
#
##############################################################################