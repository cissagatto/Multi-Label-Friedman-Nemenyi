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
res_mm = multilabel_measures()
measures = res_mm$measures


# MY METHODS TO COMPARE
my_methods = c("L", "G", "H.Ra", "NH.Ra", 
               "H.J.K1", "H.Ro.K1", "H.J.K2", "H.Ro.K2", 
               "H.J.K3", "H.Ro.K3", "H.J.T0", "H.Ro.T0",
               "H.J.T1", "H.Ro.T1", "NH.J.K1", "NH.Ro.K1",
               "NH.J.K2", "NH.Ro.K2", "NH.J.K3", "NH.Ro.K3", 
               "NH.J.T0", "NH.Ro.T0", "NH.J.T1", "NH.Ro.T1")

length(my_methods)

##############################################################################
# Generates rankings to be used in tests
##############################################################################
FolderData = paste(FolderRoot, "/Data", sep="")
folder_names = dir(FolderData)

FolderCC = paste(FolderData, "/clus-communities", sep="")
FolderDCC = paste(FolderCC, "/versao-1", sep="")
files_names = c(dir(FolderDCC))
files_measures_names = RemoveCSV(files_names)

# create directory to store results
pasta.1 = paste(FolderRoot,"/Clus-Communities-Analysis", sep="")
if(dir.exists(pasta.1)==FALSE){dir.create(pasta.1)}

pasta.2 = paste(pasta.1,"/Rankings", sep="")
if(dir.exists(pasta.2)==FALSE){dir.create(pasta.2)}

pasta.3 = paste(pasta.2,"/Critical-Distances", sep="")
if(dir.exists(pasta.3)==FALSE){dir.create(pasta.3)}

pasta.4 = paste(pasta.2,"/Mean", sep="")
if(dir.exists(pasta.4)==FALSE){dir.create(pasta.4)}

pasta.5 = paste(pasta.2,"/All", sep="")
if(dir.exists(pasta.5)==FALSE){dir.create(pasta.5)}

apagar = c(0)
todos.0 = data.frame(apagar)
todos.1 = data.frame(apagar)

a = 1
while(a<=length(files_names)){
  
  nome = paste(FolderDCC, "/", files_names[a], sep="")
  
  #read the files and format the dataframe
  # use csv if the file uses comma
  # use csv2 if the file uses dot comma
  data = data.frame(read.csv2(nome))
  
  # I dont need the 1 colun
  data = data[,-1]
  
  # MY METHODS
  colnames(data) = my_methods
  
  # coluns total
  total_col = ncol(data)
  
  # generate the rankings for all csv files
  res = generate_ranking(data)
  
  # what type of measure is??
  val = filter(measures, measures$names == files_measures_names[a])
  
  str = paste(files_measures_names[a], "-ranking.csv", sep="")  
  
  if(val$values==1){
    cat("\n1 \t", files_measures_names[a])
    # for this measures you must use rank_average_1
    # its equivalent to =ORDER.EQ(X;start:end;1) in excel
    setwd(pasta.3)
    write.csv(res$rank_average_0, str, row.names = FALSE)
    
  } else{
    cat("\n0 \t", files_measures_names[a])
    # for this measures you must use rank_min_0
    # its equivalent to =ORDER.EQ(X;start:end;0) in excel
    setwd(pasta.3)
    write.csv(res$rank_average_1, str, row.names = FALSE) 
  }
  
  setwd(pasta.5)
  str.3 = paste("0-", str, sep="")
  str.4 = paste("1-", str, sep="")
  write.csv(res$rank_average_0, str.3, row.names = FALSE)
  write.csv(res$rank_average_1, str.4, row.names = FALSE)
  
  res.0 = data.frame(apply(res$rank_average_0, 2, mean))
  colnames(res.0) = "mean"
  setwd(pasta.4)
  str.2 = paste("0-media-", str, sep="")
  write.csv(res.0, str.2)
  todos.0 = cbind(todos.0, res.0)
  
  res.1 = data.frame(apply(res$rank_average_1, 2, mean))
  colnames(res.1) = "mean"
  setwd(pasta.4)
  str.2 = paste("1-media-", str, sep="")
  write.csv(res.1, str.2)
  todos.1 = cbind(todos.1, res.1)
  
  a = a + 1
  gc()
}

todos.0 = todos.0[,-1]
names(todos.0) = files_measures_names

todos.1 = todos.1[,-1]
names(todos.1) = files_measures_names

setwd(pasta.4)
write.csv(todos.0, "todos-0.csv")
write.csv(todos.1, "todos-1.csv")


#############################################################################
# TEST FOR ALL METHODS
##############################################################################

# tsutils
name_dir = "All_v1"
path = pasta.3
res1 = Friedman_Nemenyi_v1(path, name_dir)

# scmamp
rm(name_dir, path)
name_dir = "All_v2"
path = pasta.3
res2 = Friedman_Nemenyi_v2(path, name_dir)

##############################################################################
# FAZENDO COMPARAÇÃO ENTRE OS PARES DE MÉTODOS
##############################################################################

pasta.6 = paste(pasta.1,"/Methods-Analysis", sep="")
if(dir.exists(pasta.6)==FALSE){dir.create(pasta.6)}

FolderCC = paste(FolderData, "/clus-communities", sep="")
FolderDCC = paste(FolderCC, "/versao-1", sep="")
files_names = c(dir(FolderDCC))
files_measures_names = RemoveCSV(files_names)

e = 1
while(e<=length(files_names)){
  
  nome = paste(FolderDCC, "/", files_names[e], sep="")
  data = data.frame(read.csv2(nome))
  nomes.linhas = data$Datasets
  
  data = data[,-1]
  
  colnames(data) = my_methods
  num.linhas = nrow(data)
  num.colunas = ncol(data)
  nomes.colunas = my_methods
  
  # o método X foi melhor que o método Y em W datasets
  resultado.maior.colunas = compara.colunas(num.linhas, num.colunas, 
                                            nomes.colunas, nomes.linhas, 
                                            data, soma.maior)
  
  resultado.menor.colunas = compara.colunas(num.linhas, num.colunas, 
                                            nomes.colunas, nomes.linhas, 
                                            data, soma.menor)
  
  # no dataset W o método X foi melhor que Y métodos
  # exemplo: no dataset FLAGS, o método GLOBAL foi melhor que 14 métodos
  # e pior que os outros 10
  resultado.maior.linhas = compara.linhas(num.linhas, num.colunas, 
                                          nomes.colunas, nomes.linhas, 
                                          data, maior)
  
  resultado.menor.linhas = compara.linhas(num.linhas, num.colunas, 
                                          nomes.colunas, nomes.linhas, 
                                          data, menor)
  
  setwd(pasta.6)
  write.csv(resultado.maior.colunas, 
            paste(files_measures_names[e], "-maior-colunas.csv", sep=""))
  write.csv(resultado.maior.linhas, 
            paste(files_measures_names[e], "-maior-linhas.csv", sep=""))
  write.csv(resultado.menor.colunas, 
            paste(files_measures_names[e], "-menor-colunas.csv", sep=""))
  write.csv(resultado.menor.linhas, 
            paste(files_measures_names[e], "-menor-linhas.csv", sep=""))
  
  
  e = e + 1
  gc()
}


##############################################################################
# 
##############################################################################
FolderData = paste(FolderRoot, "/Data", sep="")
folder_names = dir(FolderData)

Folder_Origem = paste(FolderData, "/clus-communities", sep="")
nomes_arquivos = c(dir(Folder_Origem))
str = RemoveCSV(nomes_arquivos)

only_jaccard = c(5,7,9,11,13,15,17,19,21,23) # 1
only_rogers = c(6,8,10,12,14,16,18,20,22,24) # 2
only_h = c(3,5:14) # 3
only_nh = c(4,15:24) # 4
only_treshold = c(11:14,21:24) # 5
only_knn = c(5:10,15:20) # 6

global_local_random = c(1:4) # 7
global_local_hybrid_h = c(1,2,5:14) # 8
global_local_hybrid_nh = c(1,2,15:24) # 9

hybrid_random = c(3:24) # 10
hybrid_local = c(5:24,1) # 11
hybrid_global = c(5:24,2) # 12

h_hybrid_random = c(5:14,3) # 13
h_hybrid_local = c(5:14,1) # 14
h_hybrid_global = c(5:14,2) # 15

nh_hybrid_random = c(15:24,4) # 16
nh_hybrid_local = c(15:24,1) # 17
nh_hybrid_global = c(15:24,2) # 18

h_jaccard_hybrid = c(5,7,9,11,13) # 19
h_rogers_hybrid = c(6,8,10,12,14) # 20

nh_jaccard_hybrid = c(15, 17, 19, 21, 23) # 21
nh_rogers_hybrid = c(16, 18, 20, 22, 24) # 22

Folders = c("only_jaccard", "only_rogers", "only_h", "only_nh",
            "only_treshold", "only_knn", "global_local_random",
            "global_local_hybrid_h", "global_local_hybrid_nh",
            "hybrid_random", "hybrid_local", "hybrid_global",
            "h_hybrid_random", "h_hybrid_local", "h_hybrid_global",  
            "nh_hybrid_random", "nh_hybrid_local", "nh_hybrid_global",  
            "h_jaccard_hybrid", "h_rogers_hybrid",
            "nh_jaccard_hybrid", "nh_rogers_hybrid")

length(Folders)

##############################################################################
# 
##############################################################################
gera_rank_again(only_jaccard, Folder_Origem,
                Folders[1], nomes_arquivos, str)

gera_rank_again(only_rogers, Folder_Origem,
                Folders[2], nomes_arquivos, str)

gera_rank_again(only_h, Folder_Origem,
                Folders[3], nomes_arquivos, str)

gera_rank_again(only_nh, Folder_Origem,
                Folders[4], nomes_arquivos, str)

gera_rank_again(only_treshold, Folder_Origem,
                Folders[5], nomes_arquivos, str)

gera_rank_again(only_knn, Folder_Origem,
                Folders[6], nomes_arquivos, str)

gera_rank_again(global_local_random, Folder_Origem,
                Folders[7], nomes_arquivos, str)

gera_rank_again(global_local_hybrid_h, Folder_Origem,
                Folders[8], nomes_arquivos, str)

gera_rank_again(global_local_hybrid_nh, Folder_Origem,
                Folders[9], nomes_arquivos, str)

gera_rank_again(hybrid_random, Folder_Origem,
                Folders[10], nomes_arquivos, str)

gera_rank_again(hybrid_local, Folder_Origem,
                Folders[11], nomes_arquivos, str)

gera_rank_again(hybrid_global, Folder_Origem,
                Folders[12], nomes_arquivos, str)

gera_rank_again(h_hybrid_random, Folder_Origem,
                Folders[13], nomes_arquivos, str)

gera_rank_again(h_hybrid_local, Folder_Origem,
                Folders[14], nomes_arquivos, str)

gera_rank_again(h_hybrid_global, Folder_Origem,
                Folders[15], nomes_arquivos, str)

gera_rank_again(nh_hybrid_random, Folder_Origem,
                Folders[16], nomes_arquivos, str)

gera_rank_again(nh_hybrid_local, Folder_Origem,
                Folders[17], nomes_arquivos, str)

gera_rank_again(nh_hybrid_global, Folder_Origem,
                Folders[18], nomes_arquivos, str)

gera_rank_again(h_jaccard_hybrid, Folder_Origem,
                Folders[19], nomes_arquivos, str)

gera_rank_again(h_rogers_hybrid, Folder_Origem,
                Folders[20], nomes_arquivos, str)

gera_rank_again(nh_jaccard_hybrid, Folder_Origem,
                Folders[21], nomes_arquivos, str)

gera_rank_again(nh_rogers_hybrid, Folder_Origem,
                Folders[22], nomes_arquivos, str)


##############################################################################
# 
##############################################################################
j = 1
while(j<=length(Folders)){
  cat("\n\n================================================")
  cat("\n", j, " - ", Folders[j])
  cat("\n================================================\n\n")
  
  path_1 = paste(FolderRoot, "/Rankings/", Folders[j], sep="")
  name_dir_1 = paste(Folders[j], "_v1", sep="")
  Friedman_Nemenyi_v1(path_1, name_dir_1)
  
  path_2 = paste(FolderRoot, "/Rankings/", Folders[j], sep="")
  name_dir_2 = paste(Folders[j], "_v2", sep="")
  Friedman_Nemenyi_v2(path_2, name_dir_2)
  
  rm(name_dir_1, path_1, name_dir_2, path_2)
  
  j = j + 1
  gc()
}







