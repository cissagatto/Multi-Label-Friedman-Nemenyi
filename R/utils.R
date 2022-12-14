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
# IF YOU NEED, HERE THE VECTORS WITH THE MEASURES
##############################################################################

multilabel_measures <- function(){
  retorno = list()
  
  multilabel_measures_names_0 =  c("clp", "coverage", "hamming-loss",
                                   "margin-loss", "mlp", "one-error",
                                   "ranking-loss", "wlp")
  
  multilabel_measures_names_1 =  c("accuracy", "average-precision", "f1", 
                                   "macro-auc", "macro-f1", "macro-precision",
                                   "macro-recall", "micro-auc", "micro-f1",
                                   "micro-precision", "micro-recall", "precision",        
                                   "recall", "subset-accuracy")
  
  names = c("accuracy", "average-precision", "clp", "coverage", 
            "f1", "hamming-loss", "macro-auc", "macro-f1", 
            "macro-precision", "macro-recall", "margin-loss", "micro-auc", 
            "micro-f1", "micro-precision", "micro-recall", "mlp", 
            "one-error", "precision", "ranking-loss", "recall", 
            "subset-accuracy", "wlp")
  
  values = c(1,1,0,0,
             1,0,1,1,
             1,1,0,1,
             1,1,1,0,
             0,1,0,1,
             1,0)
  
  measures = data.frame(names, values)
  
  retorno$measures = measures
  retorno$multilabel_measures_names_0 = multilabel_measures_names_0
  retorno$multilabel_measures_names_1 = multilabel_measures_names_1
  retorno$names = names
  retorno$values = values
  
  return(retorno)
}


##############################################################################
#
##############################################################################
RemoveCSV <- function(files){
  files2 = files
  j = 0
  for(j in 1:length(files2)){
    a = str_length(files2[j])
    a = a - 4
    files2[j] = str_sub(files2[j], end = a)
    j = j + 1
    gc()
  }
  return(files2)
}



##############################################################################
#
##############################################################################
RemoveR <- function(files){
  files2 = files
  j = 0
  for(j in 1:length(files2)){
    a = str_length(files2[j])
    a = a - 12
    files2[j] = str_sub(files2[j], end = a)
    j = j + 1
    gc()
  }
  return(files2)
}



##############################################################################
#
##############################################################################