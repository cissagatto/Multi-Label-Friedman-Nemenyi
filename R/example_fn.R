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


##############################################################################
# CALLING MULTILABEL MEASURES FUNCTION - UTILS.R
##############################################################################
res_mm = multilabel_measures()
measures = res_mm$measures


# MY METHODS TO COMPARE
my_methods = c("G.C", "L.C", 
               "H.JmaC", "H.JmiC", "H.JSC", 
               "H.KmaC", "H.KmiC", "H.KSC",
               "E.MaC", "E.MiC", 
               "O.MaC", "O.MiC", 
               "R1.MaC", "R1.MiC", "R1.SC", 
               "R2.MaC", "R2.MiC", "R2.SC", 
               "R3.C")


##############################################################################
# Generates rankings to be used in tests
##############################################################################
FolderData = paste(FolderRoot, "/Data", sep="")
folder_names = dir(FolderData)

Folder = paste(FolderData, "/", folder_names[2], sep="")
files_names = c(dir(Folder))
files_measures_names = RemoveCSV(files_names)

a = 1
while(a<=length(files_names)){
  
  nome = paste(Folder, "/", files_names[a], sep="")
  
  # read the files and format the dataframe
  # use csv if the file uses comma
  # use csv2 if the file uses dot comma
  data = data.frame(read.csv2(nome))
  
  # I dont need the 3 and 4 rows
  data = data[c(-3,-4),]
  
  # I dont need the 1 colun
  data = data[,-1]
  
  # MY METHODS
  colnames(data) = my_methods
  
  # coluns total
  total_col = ncol(data)
  
  # generate the rankings for all csv files
  res = generate_ranking(data)
  
  # what type of measure is??
  val = filter(measures, measures$names== files_measures_names[a])
  
  #
  str = paste(files_measures_names[a], "-ranking.csv", sep="")  
  
  # create directory to store results
  pasta = paste(FolderRoot,"/Rankings", sep="")
  if(dir.exists(pasta)==FALSE){dir.create(pasta)}
  
  pasta1 = paste(pasta,"/All", sep="")
  if(dir.exists(pasta1)==FALSE){dir.create(pasta1)}
  
  if(val$values==1){
    cat("\n1 \t", files_measures_names[a])
    # for this measures you must use rank_average_1
    # its equivalent to =ORDER.EQ(X;start:end;1) in excel
    setwd(pasta1)
    write.csv(res$rank_average_0, str, row.names = FALSE)
    
  } else{
    cat("\n0 \t", files_measures_names[a])
    # for this measures you must use rank_min_0
    # its equivalent to =ORDER.EQ(X;start:end;0) in excel
    setwd(pasta1)
    write.csv(res$rank_average_1, str, row.names = FALSE)  
  }
  
  a = a + 1
  gc()
  
}



##############################################################################
# TEST FOR ALL METHODS
##############################################################################

# tsutils
name_dir = "All_v1"
path = paste(FolderRoot, "/Rankings/All", sep="")
res1 = Friedman_Nemenyi_v1(path, name_dir)

# scmamp
rm(name_dir, path)
name_dir = "All_v2"
path = paste(FolderRoot, "/Rankings/All", sep="")
res2 = Friedman_Nemenyi_v2(path, name_dir)


##############################################################################
# 
##############################################################################
Folder_Origem = paste(FolderRoot, "/csvs", sep="")
nomes_arquivos = c(dir(Folder_Origem))
str = RemoveCSV(nomes_arquivos)

colunas_metodos = c("G.C", "L.C", 
                  "H.JmaC", "H.JmiC", "H.JSC", 
                  "H.KmaC", "H.KmiC", "H.KSC",
                  "E.MaC", "E.MiC", 
                  "O.MaC", "O.MiC", 
                  "R1.MaC", "R1.MiC", "R1.SC", 
                  "R2.MaC", "R2.MiC", "R2.SC", 
                  "R3.C")

sem_oracle = colunas_metodos[c(-11,-12)]

global_local_hybrid = colunas_metodos[c(1:8)]
global_local_exhaustive = colunas_metodos[c(1,2,9,10)]
global_local_oracle = colunas_metodos[c(1,2,11,12)]
global_local_random = colunas_metodos[c(1,2,13:19)]

hybrid_exhaustive_oracle = colunas_metodos[c(3:8,9:12)]
random_exhaustive_oracle = colunas_metodos[c(13:19,9:12)]

hybrid_global = colunas_metodos[c(3:8,1)]
hybrid_local = colunas_metodos[c(3:8,2)]
hybrid_exhaustive = colunas_metodos[c(3:8,9,10)]
hybrid_oracle = colunas_metodos[c(3:8,11,12)]
hybrid_random = colunas_metodos[c(3:8,13:19)]

random_global = colunas_metodos[c(13:19,1)]
random_local = colunas_metodos[c(13:19,2)]
random_exhaustive = colunas_metodos[c(13:19,9,10)]
random_oracle = colunas_metodos[c(13:19,11,12)]

Folders = c("sem_oracle", "global_local_hybrid", "global_local_exhaustive",
            "global_local_oracle", "global_local_random",
            "hybrid_exhaustive_oracle", "random_exhaustive_oracle",
            "hybrid_global", "hybrid_local", "hybrid_exhaustive",
            "hybrid_oracle", "hybrid_random", "random_global",
            "random_local", "random_exhaustive", "random_oracle")

#length(Folders)


##############################################################################
# 
##############################################################################
gera_rank_again(sem_oracle, Folder_Origem,
                Folders[1], nomes_arquivos, str)

gera_rank_again(global_local_hybrid, Folder_Origem,
                Folders[2], nomes_arquivos, str)

gera_rank_again(global_local_exhaustive, Folder_Origem,
                Folders[3], nomes_arquivos, str)

gera_rank_again(global_local_oracle, Folder_Origem,
                Folders[4], nomes_arquivos, str)

gera_rank_again(global_local_random, Folder_Origem,
                Folders[5], nomes_arquivos, str)

gera_rank_again(hybrid_exhaustive_oracle, Folder_Origem,
                Folders[6], nomes_arquivos, str)

gera_rank_again(random_exhaustive_oracle, Folder_Origem,
                Folders[7], nomes_arquivos, str)

gera_rank_again(hybrid_global, Folder_Origem,
                Folders[8], nomes_arquivos, str)

gera_rank_again(hybrid_local, Folder_Origem,
                Folders[9], nomes_arquivos, str)

gera_rank_again(hybrid_exhaustive, Folder_Origem,
                Folders[10], nomes_arquivos, str)

gera_rank_again(hybrid_oracle, Folder_Origem,
                Folders[11], nomes_arquivos, str)

gera_rank_again(hybrid_random, Folder_Origem,
                Folders[12], nomes_arquivos, str)

gera_rank_again(random_global, Folder_Origem,
                Folders[13], nomes_arquivos, str)

gera_rank_again(random_local, Folder_Origem,
                Folders[14], nomes_arquivos, str)

gera_rank_again(random_exhaustive, Folder_Origem,
                Folders[15], nomes_arquivos, str)

gera_rank_again(random_oracle, Folder_Origem,
                Folders[16], nomes_arquivos, str)

##############################################################################
# 
##############################################################################
j = 1
while(j<=length(Folders)){
  cat("\n\n", Folders[j])
  path = paste(FolderRoot, "/Rankings/", Folders[j], sep="")
  name_dir_1 = paste(Folders[j], "_v1", sep="")
  Friedman_Nemenyi_v1(path, name_dir_1)
  name_dir_2 = paste(Folders[j], "_v2", sep="")
  Friedman_Nemenyi_v2(path, name_dir_2)
  j = j + 1
  gc()
}




