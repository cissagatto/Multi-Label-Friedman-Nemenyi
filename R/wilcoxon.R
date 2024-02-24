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


# compare two groups when the normality assumption is violated using the 
# Wilcoxon test. The Wilcoxon test is a non-parametric test, meaning that it 
# does not rely on data belonging to any particular parametric family of 
# probability distributions. Non-parametric tests have the same objective as 
# their parametric counterparts. However, they have two advantages over 
# parametric tests: they do not require the assumption of normality of 
# distributions and they can deal with outliers. The reason is that 
# non-parametric tests are usually less powerful than corresponding 
# parametric tests when the normality assumption holds. Therefore, all else 
# being equal, with a non-parametric test you are less likely to reject the 
# null hypothesis when it is false if the data follows a normal distribution

# compare two groups and see whether they are significantly different from each
# other in terms of the variable of interest.


# Independent samples
# 1. The Mann-Withney-Wilcoxon test (also referred as Wilcoxon rank sum test or 
# Mann-Whitney U test) is performed when the samples are independent (so this 
# test is the non-parametric equivalent to the Student’s t-test for independent 
# samples).


# 2. The Wilcoxon signed-rank test (also sometimes referred as Wilcoxon test 
# for paired samples) is performed when the samples are paired/dependent (so 
# this test is the non-parametric equivalent to the Student’s t-test for paired 
# samples).

# Remember that the null and alternative hypothesis of the Wilcoxon test are as 
# follows:

# H0: the 2 groups are equal in terms of the variable of interest
# H1: the 2 groups are different in terms of the variable of interest

# Applied to our research question, we have:
# H0: clusters and XXXX are equal
# H1: clusters and XXXX are different

# The p-value (displayed after p = in the subtitle of the plot) indicates that 
# we do not reject the null hypothesis, so we do not reject the hypothesis that
# grades are equal before and after the semester (p-value = 0.17).

# The point of this section was to illustrate how to easily draw plots together 
# with statistical results, which is exactly the aim of the {ggstatsplot} 
# package. See more details and examples in this article.


test <- function(data, nome.medida){
  
  df = data.frame(pivot_longer(data, cols = 1:2))
  df = df[order(df$value, decreasing=TRUE), ]
  colnames(df) = c("Methods", "Value")
  
  result <- wilcox.test(df$Value ~ df$Methods, paired = TRUE)
  
  # FRIEDMAN
  name.3 = paste(plots.folder, "/", nome.medida, ".txt", sep="")
  sink(name.3)
  print(result)
  sink()
  
  statistic = as.numeric(result$statistic)
  parameter = toString(result$parameter)
  p.value = as.numeric(result$p.value)
  location.shift = as.numeric(result$null.value)
  alternative = toString(result$alternative)
  metodo = toString(result$method)
  
  resultado = data.frame(statistic, parameter, p.value, location.shift,
                         alternative, metodo)
  
  plot.wilcoxon(df, nome.medida)
  
  return(resultado)
}


plot.wilcoxon <- function(df, nome.medida){
  
  name.1 = paste(plots.folder, "/", nome.medida, "-plot.1.png", sep="")
  png(name.1, width = 5000, height = 3000, res = 500)
  print(ggplot(df) +
          aes(x = Methods, y = Value) +
          geom_boxplot(fill = "#0c4c8a") +
          theme_minimal())
  dev.off()
  
  name.2 = paste(plots.folder, "/", nome.medida, "-plot.2.png", sep="")
  png(name.2, width = 5000, height = 3000, res = 500)
  print(ggwithinstats( # paired samples
    data = df, x = Methods, y = Value,
    type = "nonparametric", # for wilcoxon
    centrality.plotting = FALSE # remove median
  ))
  dev.off()
  
  # name.2 = paste(plots.folder, "/", nome.medida, "-plot.2.png", sep="")
  # png(name.2, width = 5000, height = 3000, res = 500)
  # print(ggbetweenstats(
  #   data = df,
  #   x = Methods,
  #   y = Value,
  #   plot.type = "box",
  #   type = "nonparametric",
  #   centrality.plotting = FALSE
  # ))
  # dev.off()
  
}

wilcoxon.teste <- function(){
  
  plots.folder = "/home/biomal/Multi-Label-Friedman-Nemenyi/Analysed/Set-Up-7-g/Thr05-04/Wilcoxon/Plots"
  setwd(Folder.CSVs)
  nomes.arquivos = dir(Folder.CSVs)
  nomes = RemoveCSV(nomes.arquivos)
  measures = res.mm$measure
  todos = data.frame()
  # da medida 1 até a 22
  i = 1
  while(i<=length(nomes.arquivos)){
    
    cat("\n%----------------------------------------------------------%")
    cat("\n", nomes.arquivos[i])
    arquivo = data.frame(read.csv(nomes.arquivos[i]))
    nomes.datasets = arquivo$Datasets
    arquivo[is.na(arquivo)] <- 0
    arquivo2 = arquivo[,-1]
    names.methods = colnames(arquivo2)
    
    # cluster x ecc
    nome.medida = paste(nomes[i], "-lccml-ecc", sep="")
    data = data.frame(lccml = arquivo2$LCC.ML, ecc= arquivo2$ECC)
    res = test(data, nome.medida)
    res = cbind(nome.medida, res)
    todos = rbind(todos, res)
    
    # cluster x global
    nome.medida = paste(nomes[i], "-lccml-global", sep="")
    data = data.frame(lccml = arquivo2$LCC.ML, global = arquivo2$Global)
    res = test(data, nome.medida)
    res = cbind(nome.medida, res)
    todos = rbind(todos, res)
    
    # cluster x local
    nome.medida = paste(nomes[i], "-lccml-local", sep="")
    data = data.frame(lccml = arquivo2$LCC.ML, local = arquivo2$Local)
    res = test(data, nome.medida)
    res = cbind(nome.medida, res)
    todos = rbind(todos, res)
    
    nome.medida = paste(nomes[i], "-lccml-hpml", sep="")
    data = data.frame(lccml = arquivo2$LCC.ML, local = arquivo2$Local)
    res = test(data, nome.medida)
    res = cbind(nome.medida, res)
    todos = rbind(todos, res)
    
    i = i + 1
    gc()
    
  }
  
  cat("\n")
  setwd(plots.folder)
  write.csv(todos, "resultados.csv")
  
}

