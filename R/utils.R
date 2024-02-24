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
multilabel.measures.3 <- function(){
  retorno = list()
  
  multilabel.measures.names.0 =  c("clp", "coverage", "hamming-loss", 
                                   "margin-loss", "mlp", "one-error",
                                   "ranking-loss", "runtime", "wlp")
  
  
  multilabel.measures.names.1 =  c("accuracy", "average-precision", "f1",
                                   "macro-auc", "macro-auprc","macro-f1", 
                                   "macro-precision", "macro-recall",
                                   "micro-auc", "micro-auprc", "micro-f1", 
                                   "micro-precision", "micro-recall", 
                                   "precision", "recall",
                                   "roc-auc", "roc-auc-macro", 
                                   "roc-auc-micro", "subset-accuracy")
  
  names = c("accuracy", 
            "average-precision", 
            "clp", 
            "coverage", 
            "f1", 
            "hamming-loss", 
            "macro-auc", 
            "macro-auprc", 
            "macro-f1", 
            "macro-precision", 
            "macro-recall", 
            "margin-loss", 
            "micro-auc", 
            "micro-auprc", 
            "micro-f1", 
            "micro-precision", 
            "micro-recall", 
            "mlp", 
            "one-error", 
            "precision", 
            "ranking-loss", 
            "recall", 
            "roc-auc", 
            "roc-auc-macro",  
            "roc-auc-micro", 
            "runtime", 
            "subset-accuracy", 
            "wlp")
  
  values = c(1,1,0,0,1,1,1,1,1,1,1,0,1,1,1,1,1,0,0,1,0,1,1,1,1,0,1,0)
  
  names.2 = c("0-accuracy", "0-average-precision", "0-clp", 
              "0-coverage", "0-f1", "0-hamming-loss", 
              "0-macro-auc", "0-macro-auprc", "0-macro-f1", 
              "0-macro-precision", "0-macro-recall", "0-margin-loss", 
              "0-micro-auc", "0-micro-auprc", "0-micro-f1", 
              "0-micro-precision", "0-micro-recall", "0-mlp", 
              "0-one-error", "0-precision", "0-ranking-loss", 
              "0-recall", "0-roc-auc", "0-roc-auc-macro",  
              "0-roc-auc-micro", "0-runtime", "0-subset-accuracy", "0-wlp",
              "1-accuracy", "1-average-precision", "1-clp", 
              "1-coverage", "1-f1", "1-hamming-loss", 
              "1-macro-auc", "1-macro-auprc", "1-macro-f1", 
              "1-macro-precision", "1-macro-recall", "1-margin-loss", 
              "1-micro-auc", "1-micro-auprc", "1-micro-f1", 
              "1-micro-precision", "1-micro-recall", "1-mlp", 
              "1-one-error", "1-precision", "1-ranking-loss", 
              "1-recall", "1-roc-auc", "1-roc-auc-macro",  
              "1-roc-auc-micro", "1-runtime", "1-subset-accuracy", "1-wlp")
  
  
  measures = data.frame(names, values)
  
  retorno$measures = measures
  retorno$length.measures = length(measures)
  
  retorno$multilabel.measures.names.0 = multilabel.measures.names.0
  retorno$length.multilabel.measures.names.0 = length(multilabel.measures.names.0)
  
  retorno$multilabel.measures.names.1 = multilabel.measures.names.1
  retorno$length.multilabel.measures.names.1 = length(multilabel.measures.names.1)
  
  retorno$names = names
  retorno$length.names = length(names)
  
  retorno$names.2 = names.2
  retorno$length.names.2 = length(names.2)
  
  retorno$values = values
  retorno$length.values = length(values)
  
  return(retorno)
}

 


##############################################################################
#
##############################################################################
multilabel.measures.2 <- function(){
  retorno = list()
  
  multilabel.measures.names.0 =  c("clp", "coverage", "hamming-loss", 
                                   "margin-loss", 
                                   "mlp", "one-error",
                                   "ranking-loss",
                                   "Runtimes-Hours", "Runtimes-Minutes", 
                                   "Runtimes-Seconds", 
                                   "wlp")
  
  
  multilabel.measures.names.1 =  c("accuracy", 
                                   "auprc-macro", "auprc-micro", 
                                   "average-precision", 
                                   "bin-auc", "f1",
                                   "macro-auc", "macro-f1", 
                                   "macro-precision", "macro-recall",
                                   "micro-auc", "micro-f1", 
                                   "micro-precision", "micro-recall", 
                                   "precision", "proba-auc","recall", 
                                   "roc-bin-macro-auc", "roc-bin-micro-auc",  
                                   "roc-proba-macro-auc", "roc-proba-micro-auc",
                                   "subset-accuracy")
  
  names = c("accuracy", "auprc-macro", "auprc-micro", 
            "average-precision", "bin-auc", "clp", 
            "coverage", "f1", "hamming-loss", 
            "macro-auc", "macro-f1", "macro-precision", 
            "macro-recall", "margin-loss", "micro-auc", 
            "micro-f1", "micro-precision", "micro-recall", 
            "mlp", "one-error", "precision", 
            "proba-auc", "ranking-loss", "recall", 
            "roc-bin-macro-auc", "roc-bin-micro-auc", "roc-proba-macro-auc", 
            "roc-proba-micro-auc", "Runtimes-Hours", "Runtimes-Minutes", 
            "Runtimes-Seconds", "subset-accuracy", "wlp")
  
  values = c(1, 1, 1,
             1, 1, 0,
             0, 1, 0,
             1, 1, 1,
             1, 0, 1,
             1, 1, 1, 
             0, 0, 1, 
             1, 0, 1,
             1, 1, 1, 
             1, 0, 0, 
             0, 1, 0)
             
  names.2 = c("0-accuracy", 
              "0-auprc-macro", "0-auprc-micro", 
              "0-average-precision", 
              "0-bin-auc", 
              "0-clp", "0-coverage", "0-f1", "0-hamming-loss", 
              "0-macro-auc", "0-macro-f1", 
              "0-macro-precision", "0-macro-recall",
              "0-margin-loss", 
              "0-micro-auc", "0-micro-f1", 
              "0-micro-precision", "0-micro-recall", 
              "0-mlp", "0-one-error", "0-precision", 
              "0-proba-auc",
              "0-ranking-loss", "0-recall", 
              "0-roc-bin-macro-auc", "0-roc-bin-micro-auc",  
              "0-roc-proba-macro-auc", "0-roc-proba-micro-auc",
              "0-Runtimes-Hours", "0-Runtimes-Minutes", "0-Runtimes-Seconds", 
              "0-subset-accuracy", "0-wlp",
              "1-accuracy", 
              "1-auprc-macro", "1-auprc-micro", 
              "1-average-precision", 
              "1-bin-auc", 
              "1-clp", "1-coverage", "1-f1", "1-hamming-loss", 
              "1-macro-auc", "1-macro-f1", 
              "1-macro-precision", "1-macro-recall",
              "1-margin-loss", 
              "1-micro-auc", "1-micro-f1", 
              "1-micro-precision", "1-micro-recall", 
              "1-mlp", "1-one-error", "1-precision", 
              "1-proba-auc",
              "1-ranking-loss", "1-recall", 
              "1-roc-bin-macro-auc", "1-roc-bin-micro-auc",  
              "1-roc-proba-macro-auc", "1-roc-proba-micro-auc",
              "1-Runtimes-Hours", "1-Runtimes-Minutes", "1-Runtimes-Seconds", 
              "1-subset-accuracy", "1-wlp")
  
  
  measures = data.frame(names, values)
  
  retorno$measures = measures
  retorno$multilabel.measures.names.0 = multilabel.measures.names.0
  retorno$multilabel.measures.names.1 = multilabel.measures.names.1
  retorno$names = names
  retorno$values = values
  retorno$names.2 = names.2
  
  return(retorno)
}


##########################################################################
multilabel.measures.1 <- function(){
  retorno = list()
  
  multilabel.measures.names.0 =  c("clp", "coverage", "hamming-loss", 
                                   "margin-loss", "mlp", "one-error", 
                                   "ranking-loss", "wlp")
  length(multilabel.measures.names.0)
  
  multilabel.measures.names.1 =  c("accuracy", "average-precision", "f1",
                                   "macro-auc", "macro-f1", 
                                   "macro-precision", "macro-recall", 
                                   "micro-auc", "micro-f1", 
                                   "micro-precision", "micro-recall", 
                                   "precision", "recall", "subset-accuracy")
  length(multilabel.measures.names.1)
  
  names = c("accuracy", "average-precision", 
            "clp", "coverage", 
            "f1", "hamming-loss", 
            "macro-auc", "macro-f1", 
            "macro-precision", "macro-recall", 
            "margin-loss", 
            "micro-auc", "micro-f1", 
            "micro-precision", "micro-recall", 
            "mlp", 
            "one-error", "precision", 
            "ranking-loss", "recall", 
            "subset-accuracy", "wlp")
  
  values = c(1,1,
             0,0,
             1,0,
             1,1,
             1,1,
             0,1,
             1,1,
             1,0,
             0,1,
             0,1,
             1,0)
  
  names.2 = c("0-accuracy", "0-average-precision", "0-clp", "0-coverage", 
              "0-f1", "0-hamming-loss", "0-macro-auc", "0-macro-f1", 
              "0-macro-precision", "0-macro-recall", "0-margin-loss", 
              "0-micro-auc", "0-micro-f1", "0-micro-precision", 
              "0-micro-recall", "0-mlp", "0-one-error", "0-precision", 
              "0-ranking-loss", "0-recall", "0-subset-accuracy", "0-wlp",
              "1-accuracy", "1-average-precision", "1-clp", "1-coverage", 
              "1-f1", "1-hamming-loss", "1-macro-auc", "1-macro-f1", 
              "1-macro-precision", "1-macro-recall", "1-margin-loss", 
              "1-micro-auc", "1-micro-f1", "1-micro-precision", 
              "1-micro-recall", "1-mlp", "1-one-error", "1-precision", 
              "1-ranking-loss", "1-recall", "1-subset-accuracy", "1-wlp")
  length(names.2)
  
  measures = data.frame(names, values)
  
  retorno$measures = measures
  retorno$multilabel.measures.names.0 = multilabel.measures.names.0
  retorno$multilabel.measures.names.1 = multilabel.measures.names.1
  retorno$names = names
  retorno$values = values
  retorno$names.2 = names.2
  
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
Remove.2 <- function(files){
  files2 = files
  j = 1
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