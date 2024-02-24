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
biggerEqual <- function(x, y){return(x >= y)}
bigger <- function(x, y){return(x > y)}

lessEqual <- function(x, y){return(x <= y)}
less <- function(x, y){return(x < y)}

equal <- function(x, y){return(x == y)}
subtraction <- function(x, y){return(x - y)}

add.biggerEqual <- function(x, y){return(sum(x >= y))}
add.lessEqual <- function(x, y){return(sum(x <= y))}

add.bigger <- function(x, y){return(sum(x > y))}
add.less <- function(x, y){return(sum(x < y))}
add.equal <- function(x, y){return(sum(x == y))}


#############################################################################
# constructs an array with the exact size of rows and columns 
# of the dataframe - not square
matrix.columns.rows <- function(names.columns, names.rows,
                                num.rows, num.columns){
  matrix <- matrix(nrow=num.rows, ncol=num.columns, data=0)
  colnames(matrix) <- names.columns
  rownames(matrix) <- names.rows
  return(matrix)
  gc()
}


#############################################################################
# builds a square matrix with the size of the columns
matrix.columns <- function(names.columns, num.columns){
  matrix <- matrix(nrow=num.columns, ncol=num.columns, data=0)
  colnames(matrix) <- names.columns
  rownames(matrix) <- names.columns
  return(matrix)
  gc()
}


#############################################################################
# builds a square matrix with the size of the rows
matrix.rows <- function(names.rows, num.rows){
  matrix <- matrix(nrow=num.rows, ncol=num.rows, data=0)
  colnames(matrix) <- names.rows
  rownames(matrix) <- names.rows
  return(matrix)
  gc()
}


#############################################################################
# builds a matrix equal to the original dataframe
matrix.rows.columns <- function(names.rows, num.columns){
  matrix <- matrix(nrow=num.rows, ncol=num.columns, data=0)
  colnames(matrix) <- names.columns
  rownames(matrix) <- names.rows
  return(matrix)
  gc()
}


#############################################################################
# compare dataframe row values
compare.rows <- function(num.rows, num.columns, names.columns, 
                         names.rows, data, FUN){
  
  delete = c(0)
  all = data.frame(delete)
  
  i = 1
  j = 1
  
  for(i in 1:num.columns){
    matrix = matrix.columns.rows(names.columns, names.rows, 
                                 num.rows, num.columns)
    for (j in 1:num.columns){
      matrix[,j] = FUN(data[,i], data[,j])
      j = j + 1
    }
    
    matrix = data.frame(matrix)
    colnames(matrix) = names.columns
    
    res = apply(matrix, 1, sum)
    all = cbind(all, res)
    
    i = i + 1
  }
  
  all = all[,-1]
  colnames(all) = names.columns
  
  return(all)
}


#############################################################################
# número total de datasets onde um método foi melhor que o outro
# o método X foi melhor que o método Y em W datasets
total.datasets.per.method <- function(num.rows, num.columns, 
                                      names.columns, names.rows, 
                                      data, FUN){
  retorno = list()
  
  
  # num.rows = nrow(data)
  # num.columns = ncol(data)
  # names.columns = my.methods
  # names.rows = my.methods
  # FUN = add.bigger
  
  matrix = matrix.columns(names.columns, num.columns)
  for(i in 1:num.columns){
    for(j in 1:num.columns){
      matrix[i,j] = FUN(data[,i], data[,j])
    }  
  }
  
  # MÉDIA DAS COLUNAS É A MÉDIA POR DATASET
  mean.columns = data.frame(apply(matrix, 2, mean)-1)
  names(mean.columns) = "mean.columns"
  
  # MÉDIA DAS LINHAS É A MÉDIA POR MÉTODO
  mean.rows = data.frame(apply(matrix, 1, mean)-1)
  names(mean.rows) = "mean.rows"
  
  retorno$matrix = matrix
  retorno$mean.per.columns = mean.columns
  retorno$mean.per.rows = mean.rows
  return(retorno)
}


#############################################################################
# checks if one method is bigger/less than the other according to the columns
compare <- function(measures, num.rows, num.columns,
                    names.columns, names.rows, 
                    names.all, data, FUN){
  
  delete = c(0)
  all.all = data.frame(delete)
  
  all = 1
  while(all<=length(names.all)){
    
    cat("\n\nMethod ", names.all[all])
    
    matrix = matrix.rows.columns(names.rows, num.columns)
    
    final = data.frame(delete)
    
    rows = 1
    
    for(rows in rows:num.rows){
      
      cat("\n\tRow ", rows)
      columns = 1
      index = 1
      rm(a)
      a = c()
      names = c()
      
      for(columns in columns:num.columns){
        cat("\n\t\tColumns ", columns)
        a[index] = FUN(data[rows,1], data[rows,columns])
        names[index] = paste(names.columns[rows], " X ", 
                             names.columns[columns], sep="")
        columns = columns + 1
        index = index + 1
        gc()
      }
      
      #cat("\nVETOR")
      #print(a)
      #cat("\n")
      
      #cat("\nmatrix")
      matrix[rows,] = a
      #print(matrix)
      #cat("\n")
      
      rows = rows + 1
      gc()
    }
    
    namae = c("")
    q = 1
    while(q<=length(names.all)){
      namae[q] = paste(names.all[all], " X ",
                       names.all[q])  
      q = q + 1
      gc()
    }
    colnames(matrix) = namae
    
    all.all = cbind(all.all, matrix)
    
    all = all + 1
    gc()
  }
  
  all.all = all.all[,-1]
  return(all.all)
  
}


##############################################################################
#
##############################################################################
compare.for.all.measures <- function(folder.names.csv,
                                     Folder.CSVs, 
                                     FolderMethods,
                                     my.methods, 
                                     nomes.measures, 
                                     res.mm){
  
  apagar = c(0)
  
  # MÉDIA DAS COLUNAS É A MÉDIA POR DATASET
  # MÉDIA DAS LINHAS É A MÉDIA POR MÉTODO
  
  bigE.per.col = data.frame(apagar) # linha
  bigE.per.row = data.frame(apagar) # coluna
  
  big.per.col = data.frame(apagar) # linha
  big.per.row = data.frame(apagar) # coluna
  
  lesE.per.col = data.frame(apagar) # linha
  lesE.per.row = data.frame(apagar) # coluna
  
  les.per.col = data.frame(apagar) # linha
  les.per.row = data.frame(apagar) # coluna
  
  eq.per.col = data.frame(apagar) # linha
  eq.per.row = data.frame(apagar) # coluna
  
  measures = data.frame(res.mm$measures)
  
  e = 1
  while(e<=length(nomes.measures)){
    
    nomes.res = RemoveCSV(folder.names.csv[e])
    
    cat("\n", nomes.res)
    
    FolderMedida = paste(FolderMethods, "/", nomes.res, sep="")
    if(dir.exists(FolderMedida)==FALSE){dir.create(FolderMedida)}
    
    nome = paste(Folder.CSVs, "/", folder.names.csv[e], sep="")
    data = data.frame(read.csv(nome), stringsAsFactors = FALSE)
    nomes.linhas = data$dataset
    
    data = data[,-1]
    data[is.na(data)] <- 0
    # data = data[c(-3,-4),]
    
    colnames(data) = my.methods
    num.linhas = nrow(data)
    num.colunas = ncol(data)
    nomes.colunas = my.methods
    
    # sapply(data, class)
    
    ###########################################################
    # o método X foi melhor que o método Y em W datasets
    res.bigger = total.datasets.per.method(num.linhas, num.colunas, 
                                           nomes.colunas, nomes.linhas, 
                                           data, add.bigger)
    
    res.biggerE = total.datasets.per.method(num.linhas, num.colunas, 
                                            nomes.colunas, nomes.linhas, 
                                            data, add.biggerEqual)
    
    res.less = total.datasets.per.method(num.linhas, num.colunas, 
                                         nomes.colunas, nomes.linhas, 
                                         data, add.less)
    
    res.lessE = total.datasets.per.method(num.linhas, num.colunas, 
                                          nomes.colunas, nomes.linhas, 
                                          data, add.lessEqual)
    
    res.equal = total.datasets.per.method(num.linhas, num.colunas, 
                                          nomes.colunas, nomes.linhas, 
                                          data, add.equal)
    
    ###########################################################
    # um método X teve melhor desempenho que o método Y?
    
    # PARA MEDIDAS COM 1 SENDO O MELHOR RESULTADO
    # se (metodo.1 >= metodo.2) então 1 senão 0
    biggerE = data.frame(res.biggerE$matrix)
    
    # PARA MEDIDAS COM 1 SENDO O MELHOR RESULTADO
    # se (metodo.1 > metodo.2) então 1 senão 0
    bigger = data.frame(res.bigger$matrix)
    
    # PARA MEDIDAS COM 1 SENDO O MELHOR RESULTADO
    # se (metodo.1 >= metodo.2) então 1 senão 0
    lessE = data.frame(res.lessE$matrix)
    
    # PARA MEDIDAS COM 0 SENDO O MELHOR RESULTADO
    # se (metodo.1 < metodo.2) então 1 senão 0
    less = data.frame(res.less$matrix)
    
    # APENAS VERIFICA SE OS VALORES SÃO IDÊNTICOS
    equal = data.frame(res.equal$matrix)
    
    ###########################################################
    setwd(FolderMedida)
    write.csv(biggerE,  
              paste(nomes.measures[e], "-maior-igual-total-datasets-per-method.csv", 
                    sep=""))
    write.csv(bigger,  
              paste(nomes.measures[e], "-maior-total-datasets-per-method.csv", 
                    sep=""))
    write.csv(lessE, 
              paste(nomes.measures[e], "-menor-igual-total-datasets-per-method.csv", 
                    sep=""))
    write.csv(less, 
              paste(nomes.measures[e], "-menor-total-datasets-per-method.csv", 
                    sep=""))
    write.csv(equal, 
              paste(nomes.measures[e], "-igual-total-datasets-per-method.csv", 
                    sep=""))
    
    ########################################
    
    bigE.per.col = cbind(bigE.per.col, res.biggerE$mean.per.columns)
    names(bigE.per.col)[e+1] = nomes.res 
    
    big.per.col = cbind(big.per.col, res.bigger$mean.per.columns)
    names(big.per.col)[e+1] = nomes.res 
    
    bigE.per.row = cbind(bigE.per.row, res.biggerE$mean.per.rows)
    names(bigE.per.row)[e+1] = nomes.res 
    
    big.per.row = cbind(big.per.row, res.bigger$mean.per.rows)
    names(big.per.row)[e+1] = nomes.res 
    
    lesE.per.col = cbind(lesE.per.col, res.lessE$mean.per.columns)
    names(lesE.per.col)[e+1] = nomes.res 
    
    les.per.col = cbind(les.per.col, res.less$mean.per.columns)
    names(les.per.col)[e+1] = nomes.res 
    
    lesE.per.row = cbind(lesE.per.row, res.lessE$mean.per.rows)
    names(lesE.per.row)[e+1] = nomes.res 
    
    les.per.row = cbind(les.per.row, res.less$mean.per.rows)
    names(les.per.row)[e+1] = nomes.res 
    
    eq.per.col = cbind(eq.per.col, res.equal$mean.per.columns)
    names(eq.per.col)[e+1] = nomes.res 
    
    eq.per.row = cbind(eq.per.row, res.equal$mean.per.rows)
    names(eq.per.row)[e+1] = nomes.res 
    
    ##################################################
    #   selecionado = filter(measures, names==nomes.res)
    #   
    #   if(selecionado$values==1){
    #     cat("\n1")
    #     # bigger
    #     per.method = cbind(per.dataset, res.bigger$mean.per.rows)
    #     names(per.method)[e+1] = nomes.res 
    #     
    #     per.dataset = cbind(per.method, res.bigger$mean.per.columns)
    #     names(per.dataset)[e+1] = nomes.res 
    #     
    #   } else {
    #     cat("\n0")
    #     # less
    #     per.method = cbind(per.dataset, res.less$mean.per.rows)
    #     names(per.method)[e+1] = nomes.res 
    #     
    #     per.dataset = cbind(per.method, res.less$mean.per.columns)
    #     names(per.dataset)[e+1] = nomes.res  
    #   }
    #   
    #   cat("\n")
    #   print(per.dataset)
    #   cat("\n")
    #   print(per.method)
    #   cat("\n")
    #   
    #   e = e + 1
    #   gc()
    # }
    # 
    # per.dataset = per.dataset[,-1]
    # per.method = per.method[,-1]
    
    e = e + 1
    gc()
  }
  
  bigE.per.col = big.per.col[,-1]
  big.per.col = big.per.col[,-1]
  
  bigE.per.row = bigE.per.row[,-1]
  big.per.row = big.per.row[,-1]
  
  lesE.per.col = lesE.per.col[,-1]
  les.per.col = les.per.col[,-1]
  
  lesE.per.row = lesE.per.row[,-1]
  les.per.row = les.per.row[,-1]
  
  eq.per.col = eq.per.col[,-1]
  eq.per.row = eq.per.row[,-1]
  
  setwd(FolderMethods)
  write.csv(big.per.col, "average-big-per-col.csv")
  write.csv(bigE.per.col, "average-big-equal-per-col.csv")
  
  write.csv(big.per.row, "average-big-per-row.csv")
  write.csv(bigE.per.row, "average-big-equal-per-row.csv")
  
  write.csv(les.per.col, "average-les-per-col.csv")
  write.csv(lesE.per.col, "average-les-equal-per-col.csv")
  
  write.csv(les.per.row, "average-les-per-row.csv")
  write.csv(lesE.per.row, "average-les-equal-per-row.csv")
  
  write.csv(eq.per.col, "average-eq-per-col.csv")
  write.csv(eq.per.row, "average-eq-per-row.csv")
  
}




##############################################################################
# O método X foi melhor que o método Y? Se sim = 1 senão = 0. Isso resulta em 
# uma matriz de zeros e uns indicando onde um método foi que o outro
# Podemos somar os valores de cada linha para saber:
# Na medida de avaliação Z, e no dataset D, o método X foi melhor que quantos 
# outros métodos? # Exemplo:
# Na medida de avaliação MACRO-F1 e no dataset EMOTIONS, o método Global foi
# melhor que W outros métodos.
# Se temos no total 19 métodos, então o método Global pode ter sido melhor
# em 9 dos 19 métodos e, portanto, teve pior desempenho com relação aos
# outros 10 métodos. 
# Resumindo, o método Global teve bom desempenho em 9 dos 19 particionamentos
# Resumindo, o método Global teve o pior desempenho em 10 dos 19 particionamento
##############################################################################
total.performance.method <- function(folder.names.csv,
                                     Folder.CSVs, 
                                     FolderMethods,
                                     my.methods,
                                     nomes.measures,
                                     res.mm){
  
  retorno = list()
  nomes.linhas = c()
  media.todos.r = data.frame(apagar=c(0))
  media.todos.c = data.frame(apagar=c(0))
  final.sub = data.frame(apagar=c(0))
  
  measures = data.frame(res.mm$measures)
  
  e = 1
  while(e<=length(nomes.measures)){
    
    nomes.res = RemoveCSV(folder.names.csv[e])
    
    FolderMedida = paste(FolderMethods, "/", nomes.res, sep="")
    if(dir.exists(FolderMedida)==FALSE){dir.create(FolderMedida)}
    
    nome = paste(Folder.CSVs, "/", folder.names.csv[e], sep="")
    data = data.frame(read.csv(nome), stringsAsFactors = FALSE)
    
    #sapply(data, class)
    data[is.na(data)] <- 0
    nomes.linhas = data$dataset
    data = data[,-1]
    
    colnames(data) = my.methods
    num.linhas = nrow(data)
    num.colunas = ncol(data)
    nomes.colunas = my.methods
    
    # sapply(data, class)
    
    final.row = data.frame(apagar=c(0))
    final.col = data.frame(apagar=c(0))
    subtracao = data.frame(apagar=c(0))
    
    selecionado = filter(measures, names==nomes.res)
    
    if(selecionado$values==1){
      
      j = 1
      for(j in 1:ncol(data)){
        cat("\n|=============================================|")
        cat("\n", nomes.res, " = 1")
        cat("\n\t", nomes.colunas[j])
        
        res.2 = matrix.columns.rows(nomes.colunas, nomes.linhas, 
                                    num.linhas, num.colunas)
        i = 1
        for(i in 1:ncol(data)){
          cat("\n\t\t", nomes.colunas[i])
          res.2[,i] = data[,j] > data[,i]
          i = i + 1
          gc()
        }
        
        setwd(FolderMedida)
        write.csv(res.2, paste(nomes.res, "-colunas-comparadas.csv", sep=""))
        
        # SOMANDO POR LINHA DÁ O TOTAL DE TODOS OS MÉTODOS EM QUE
        # O DATASET FOI MELHOR
        total.row = data.frame(apply(res.2, 1, sum))
        names(total.row) = nomes.colunas[j]
        
        # SOMANDO POR coluna DÁ O TOTAL DE TODOS OS DATASET EM QUE
        # O MÉTODO FOI MELHOR
        # total.col = data.frame(apply(res.2, 2, sum))
        # names(total.col) = nomes.colunas[j]
        
        final.row = cbind(final.row, total.row)
        # final.col = cbind(final.col, total.col)
        
        j = j + 1
        gc()
        
      } # fim de tudo
      
      setwd(FolderMedida)
      write.csv2(final.row, 
                 paste(nomes.res, "-maior-total-method-per-dataset.csv", 
                       sep=""))
      
    } else {
      
      j = 1
      for(j in 1:ncol(data)){
        
        cat("\n|=============================================|")
        cat("\n ", nomes.res, " = 0")
        cat("\n\t", nomes.colunas[j])
        
        res.2 = matrix.columns.rows(nomes.colunas, nomes.linhas, 
                                    num.linhas, num.colunas)
        i = 1
        for(i in 1:ncol(data)){
          cat("\n\t\t", nomes.colunas[i])
          res.2[,i] = data[,j] < data[,i]
          i = i + 1
          gc()
        }
        
        
        setwd(FolderMedida)
        write.csv(res.2, paste(nomes.res, "-colunas-comparadas.csv"))
        
        # SOMANDO POR LINHA DÁ O TOTAL DE TODOS OS MÉTODOS EM QUE
        # O DATASET FOI MELHOR
        total.row = data.frame(apply(res.2, 1, sum))
        names(total.row) = nomes.colunas[j]
        
        final.row = cbind(final.row, total.row)
        
        j = j + 1
        gc()
        
      } # fim de tudo
      
      # cat("\nSalvando")
      setwd(FolderMedida)
      write.csv2(final.row, paste(nomes.res, "-menor-total-method-per-dataset.csv", sep=""))
      
    } 
    
    # cat("\nfora do if")
    final.row = final.row[,-1]
    
    # por linhas é método
    res.fin.row = data.frame(apply(final.row, 1, mean))
    names(res.fin.row) = nomes.res
    media.todos.r = cbind(media.todos.r, res.fin.row)
    
    # por colunas é dataset
    # res.fin.coluna = data.frame(apply(final.row, 2, mean))
    # names(res.fin.coluna) = nomes.res
    # media.todos.c = cbind(media.todos.c, res.fin.coluna)
    
    # cat("\nSUBTRACAO")
    # j = 1
    # for(j in 1:ncol(data)){
    #   cat("\n\t", nomes.colunas[j])
    #   
    #   res.2 = matrix.columns.rows(nomes.colunas, nomes.linhas, 
    #                               num.linhas, num.colunas)
    #   i = 1
    #   for(i in 1:ncol(data)){
    #     cat("\n\t\t", nomes.colunas[i])
    #     res.2[,i] = data[,j] - data[,i]
    #     i = i + 1
    #     gc()
    #   }
    #   
    #   total = data.frame(apply(res.2, 1, sum))
    #   names(total) = nomes.colunas[j]
    #   
    #   subtracao = cbind(subtracao , total)
    #   #cat("\n")
    #   #print(subtracao)
    #   #cat("\n")
    #   
    #   j = j + 1
    #   gc()
    # }
    # 
    # subtracao = subtracao[,-1]
    # setwd(FolderMedida)
    # write.csv2(subtracao, paste(nomes.res, "-sub.csv", sep=""))
    
    e = e + 1
    gc()
  }
  
  
  # cat("\nfora do loop")
  media.todos.r = media.todos.r[,-1]
  media.todos.r = cbind(datasets = nomes.linhas, media.todos.r)
  
  # media.todos.c = media.todos.c[,-1]
  # media.todos.c = data.frame(methods = my.methods, media.todos.c)
  
  # cat("\nsalvando")
  setwd(FolderMethods)
  write.csv2(media.todos.r, paste(nomes.res, "-average-method-per-dataset.csv", sep=""))
  
  
  retorno$all.average.row = media.todos.r
  return(retorno)
}


# freq.pares.rotulos = map_dfr(.x = combn(names(data), 2, simplify = FALSE),
#                              ~ data %>%
#                                select(.x) %>%
#                                summarise(par_a = .x[1],
#                                          par_b = .x[2],
#                                          n = sum(par_a > par_b)))
# names(freq.pares.rotulos)[3] = "coocorrencia"
# retorno$freq.pares.rotulos = arrange(freq.pares.rotulos, desc(coocorrencia))
 

#############################################################################
total.performance.method.2 <- function(folder.names.csv,
                                       Folder.CSVs, 
                                       nomes,
                                       FolderMethods,
                                       my.methods, 
                                       res.mm){
  
  retorno = list()
  nomes.linhas = c()
  media.todos.r = data.frame(apagar=c(0))
  media.todos.c = data.frame(apagar=c(0))
  final.sub = data.frame(apagar=c(0))
  
  measures = data.frame(res.mm$measures)
  
  e = 1
  while(e<=length(nomes)){
    
    nomes.res = RemoveCSV(folder.names.csv[e])
    
    FolderMedida = paste(FolderMethods, "/", nomes.res, sep="")
    if(dir.exists(FolderMedida)==FALSE){dir.create(FolderMedida)}
    
    nome = paste(Folder.CSVs, "/", folder.names.csv[e], sep="")
    data = data.frame(read.csv(nome), stringsAsFactors = FALSE)
    
    #sapply(data, class)
    
    nomes.linhas = data$dataset
    data = data[,-1]
    
    colnames(data) = my.methods
    num.linhas = nrow(data)
    num.colunas = ncol(data)
    nomes.colunas = my.methods
    
    # sapply(data, class)
    
    final.row = data.frame(apagar=c(0))
    final.col = data.frame(apagar=c(0))
    subtracao = data.frame(apagar=c(0))
    j = 1
    for(j in 1:ncol(data)){
      cat("\n|=============================================|")
      cat("\nMAIOR")
      cat("\n\t", nomes.colunas[j])
      
      res.2 = matrix.columns.rows(nomes.colunas, nomes.linhas, 
                                  num.linhas, num.colunas)
      i = 1
      for(i in 1:ncol(data)){
        cat("\n\t\t", nomes.colunas[i])
        res.2[,i] = data[,j] > data[,i]
        i = i + 1
        gc()
      }
      
      setwd(FolderMedida)
      write.csv(res.2, paste(nomes.res, "-maior-colunas-comparadas.csv", 
                             sep=""))
      
      # SOMANDO POR LINHA DÁ O TOTAL DE TODOS OS MÉTODOS EM QUE
      # O DATASET FOI MELHOR
      total.row = data.frame(apply(res.2, 1, sum))
      names(total.row) = nomes.colunas[j]
      
      # SOMANDO POR coluna DÁ O TOTAL DE TODOS OS DATASET EM QUE
      # O MÉTODO FOI MELHOR
      # total.col = data.frame(apply(res.2, 2, sum))
      # names(total.col) = nomes.colunas[j]
      
      final.row = cbind(final.row, total.row)
      # final.col = cbind(final.col, total.col)
      
      j = j + 1
      gc()
      
    } # fim de tudo
    
    final.row = final.row[,-1]
    setwd(FolderMedida)
    write.csv2(final.row, 
               paste(nomes.res, "-maior-total-method-per-dataset.csv", 
                     sep=""))
    
    
    final.row = data.frame(apagar=c(0))
    final.col = data.frame(apagar=c(0))
    subtracao = data.frame(apagar=c(0))
    j = 1
    for(j in 1:ncol(data)){
      
      cat("\n|=============================================|")
      cat("\nMENOR")
      cat("\n\t", nomes.colunas[j])
      
      res.2 = matrix.columns.rows(nomes.colunas, nomes.linhas, 
                                  num.linhas, num.colunas)
      i = 1
      for(i in 1:ncol(data)){
        cat("\n\t\t", nomes.colunas[i])
        res.2[,i] = data[,j] < data[,i]
        i = i + 1
        gc()
      }
      
      
      setwd(FolderMedida)
      write.csv(res.2, 
                paste(nomes.res, "-menor-colunas-comparadas.csv", sep=""))
      
      # SOMANDO POR LINHA DÁ O TOTAL DE TODOS OS MÉTODOS EM QUE
      # O DATASET FOI MELHOR
      total.row = data.frame(apply(res.2, 1, sum))
      names(total.row) = nomes.colunas[j]
      
      final.row = cbind(final.row, total.row)
      
      j = j + 1
      gc()
      
    } # fim de tudo
    
    # cat("\nSalvando")
    setwd(FolderMedida)
    write.csv2(final.row, paste(nomes.res, "-menor-total-method-per-dataset.csv", sep=""))
    
    # cat("\nfora do if")
    final.row = final.row[,-1]
    
    # por linhas é método
    res.fin.row = data.frame(apply(final.row, 1, mean))
    names(res.fin.row) = nomes.res
    media.todos.r = cbind(media.todos.r, res.fin.row)
    
    e = e + 1
    gc()
  }
  
  
  # cat("\nfora do loop")
  media.todos.r = media.todos.r[,-1]
  media.todos.r = cbind(datasets = nomes.linhas, media.todos.r)
  
  # media.todos.c = media.todos.c[,-1]
  # media.todos.c = data.frame(methods = my.methods, media.todos.c)
  
  # cat("\nsalvando")
  setwd(FolderMethods)
  write.csv2(media.todos.r, paste(nomes.res, "-average-method-per-dataset.csv", sep=""))
  
  
  retorno$all.average.row = media.todos.r
  return(retorno)
}
