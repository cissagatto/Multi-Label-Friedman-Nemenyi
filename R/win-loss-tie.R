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



###########################################################3
WinLossTiePlotTodos <- function(FolderWLT, 
                                max.value, 
                                half.value, 
                                measures, 
                                title,
                                idioma){
  
  i = 1
  while(i<=length(measures$names)){
    cat("\n", i)
    
    setwd(FolderWLT)
    nome.arquivo = paste("win-loss-tie-", measures$names[i], ".csv", sep="")
    arquivo = data.frame(read.csv(nome.arquivo))
    arquivo.2 = data.frame(t(arquivo))
    names(arquivo.2) = arquivo$method
    arquivo.2 = arquivo.2[-1,]
    
    my.methods = c("HPML.A.c", "R1", "R2", "Lo", "G")
    
    arquivo.3 = data.frame(HPML = arquivo.2$HPML.A.c,
                           R1 = arquivo.2$R1,
                           R2 = arquivo.2$R2,
                           Lo = arquivo.2$Lo,
                           G = arquivo.2$G)
    rownames(arquivo.3) = c("win", "loss", "tie")
    
    
    # arquivo.3 = data.frame(G = arquivo.2$G,
    #                        Lo = arquivo.2$Lo,
    #                        H.Ra = arquivo.2$H.Ra,
    #                        NH.Ra = arquivo.2$NH.Ra,
    #                        H.J.K1 = arquivo.2$H.J.K1,
    #                        H.Ro.K1 = arquivo.2$H.Ro.K1,
    #                        H.J.K2 = arquivo.2$H.J.K2,
    #                        H.Ro.K2 = arquivo.2$H.Ro.K2,
    #                        H.J.K3 = arquivo.2$H.J.K3,
    #                        H.Ro.K3 = arquivo.2$H.Ro.K3,
    #                        H.J.T0 = arquivo.2$H.J.T0,
    #                        H.Ro.T0 = arquivo.2$H.Ro.T0,
    #                        H.J.T0 = arquivo.2$H.J.T0,
    #                        H.Ro.T1 = arquivo.2$H.Ro.T1,
    #                        NH.J.K1 = arquivo.2$NH.J.K1,
    #                        NH.Ro.K1 = arquivo.2$NH.Ro.K1,
    #                        NH.J.K2 = arquivo.2$NH.J.K2,
    #                        NH.Ro.K2 = arquivo.2$NH.Ro.K2,
    #                        NH.J.K3 = arquivo.2$NH.J.K3,
    #                        NH.Ro.K3 = arquivo.2$NH.Ro.K3,
    #                        NH.J.T0 = arquivo.2$NH.J.T0,
    #                        NH.Ro.T0 = arquivo.2$NH.Ro.T0,
    #                        NH.J.T0 = arquivo.2$H.J.T0,
    #                        NH.Ro.T1 = arquivo.2$NH.Ro.T1)
    # rownames(arquivo.3) = c("win", "loss", "tie")
    # 
    # 
    # my.methods = c("G", "Lo",
    #                "H.Ra", "NH.Ra",
    #                "H.J.K1", "H.Ro.K1",
    #                "H.J.K2", "H.Ro.K2",
    #                "H.J.K3", "H.Ro.K3",
    #                "H.J.T0", "H.Ro.T0",
    #                "H.J.T1", "H.Ro.T1",
    #                "NH.J.K1", "NH.Ro.K1",
    #                "NH.J.K2", "NH.Ro.K2",
    #                "NH.J.K3", "NH.Ro.K3",
    #                "NH.J.T0", "NH.Ro.T0",
    #                "NH.J.T1", "NH.Ro.T1")

    #arquivo.4 = data.frame(t(arquivo.3))
    #rownames(arquivo.4) = my.methods
    
    save = paste(FolderWLT, "/", measures$names[i], ".png", sep="")
    png(save, width = 3000, height = 3000, res = 500)
    
    #save = paste(FolderWLT, "/", measures$names[i], ".pdf", sep="")
    #pdf(save, width = 10, height = 10)
    
    #(arquivo.3)
    
    #ggplot(data=arquivo.3, aes(x=arquivo.3, y=values, fill=wtls)) +
    #  geom_bar(stat="identity", width = 0.5) + 
    #  scale_fill_manual(values = c("purple", "green", "blue")) + 
    #  coord_flip()
    
    if(idioma=="pt"){
      wtl = c("vitórias", "derrotas", "empates")
      #colnames(arquivo.4) = wtl
    } else {
      wtl = c("win", "loss", "tie")
      #colnames(arquivo.4) = wtl
    }
    
    
    #main = measures$names[i],
    colors = c("#00CCFF","#990066","#009900")
    barplot(as.matrix(arquivo.3), col=colors, horiz = TRUE, 
            names.arg = my.methods, 
            las = 1, border = "#888888", xlim = c(0,max.value),
            cex.names = 0.8,
            cex.axis = 0.8, axisnames = TRUE)
    abline(v=half.value, col="white")
    abline(v=115, col="white")
    abline(v=345, col="white")
    legend("right", wtl, cex = 0.8, fill = colors)
    
    #  xlab = "values", ylab = "methods",
    # main = paste(title, " ", measures$names[i], sep="")
    
    dev.off()
    
    # font.main = 1, # xpd = FALSE, axes = TRUE, 
    
    i = i + 1
    gc()
  }
}




###########################################################3
WinLossTiePlotMa <- function(FolderWLT, 
                                max.value, 
                                half.value, 
                                measures, 
                                title,
                                idioma){
  
  i = 1
  while(i<=length(measures$names)){
    cat("\n", i)
    
    setwd(FolderWLT)
    nome.arquivo = paste("win-loss-tie-", measures$names[i], ".csv", sep="")
    arquivo = data.frame(read.csv(nome.arquivo))
    arquivo.2 = data.frame(t(arquivo))
    names(arquivo.2) = arquivo$method
    arquivo.2 = arquivo.2[-1,]
    
    
    arquivo.3 = data.frame(G = arquivo.2$G,
                           Lo = arquivo.2$Lo, 
                           H.jma = arquivo.2$`H-JMa`,
                           H.js = arquivo.2$`H-JS`,
                           H.kma = arquivo.2$`H-KMa`,
                           H.ks = arquivo.2$`H-KS`,
                           E.ma = arquivo.2$`E-Ma`,
                           O.ma = arquivo.2$`O-Ma`,
                           R1.ma = arquivo.2$`R1-Ma`,
                           R1.s = arquivo.2$`R1-S`,
                           R2.ma = arquivo.2$`R2-Ma`,
                           R2.s = arquivo.2$`R2-S`,
                           R3 = arquivo.2$R3)
    
    my.methods.ma = c("G", "Lo",
                      "H.jma", "H.js",
                      "H.kma", "H.ks",
                      "E.ma", "O.ma",
                      "R1.ma", "R1.s",
                      "R2.ma", "R2.s", "R3")
    rownames(arquivo.3) = rownames(my.methods)
    
    #save = paste(FolderWLT, "/", measures$names[i], ".png", sep="")
    #png(save, width = 5000, height = 3000, res = 500)
    
    save = paste(FolderWLT, "/", measures$names[i], ".pdf", sep="")
    pdf(save, width = 5, height = 5)
    
    #(arquivo.3)
    
    #ggplot(data=arquivo.3, aes(x=arquivo.3, y=values, fill=wtls)) +
    #  geom_bar(stat="identity", width = 0.5) + 
    #  scale_fill_manual(values = c("purple", "green", "blue")) + 
    #  coord_flip()
    
    if(idioma=="p"){
      wtl = c("vitórias", "perdas", "empates")
    } else {
      wtl = c("win", "loss", "tie")
    }
    
    
    #main = measures$names[i],
    colors = c("#00CCFF","#990066","#009900")
    barplot(as.matrix(arquivo.3), col=colors, horiz = TRUE, 
            names.arg = my.methods.ma, 
            las = 1, border = "#888888", xlim = c(0,max.value),
            cex.names = 0.8,
            cex.axis = 0.8, axisnames = TRUE)
    abline(v=half.value, col="white")
    legend("right", wtl, cex = 0.8, fill = colors)
    
    #  xlab = "values", ylab = "methods",
    # main = paste(title, " ", measures$names[i], sep="")
    
    dev.off()
    
    # font.main = 1, # xpd = FALSE, axes = TRUE, 
    
    i = i + 1
    gc()
  }
}




###############################################################
#
###############################################################
compute.all.win.loss.tie <- function(folder.names.csv,
                                     Folder.CSVs, 
                                     FolderWLT,
                                     my.methods, 
                                     res.mm){
  
  setwd(Folder.CSVs)
  nomes.arquivos = dir(Folder.CSVs)
  nomes = RemoveCSV(nomes.arquivos)
  
  measures = res.mm$measures
  
  todos = data.frame(apagar = c(0))
  
  # da medida 1 até a 22
  i = 1
  while(i<=length(nomes.arquivos)){
    cat("\n%----------------------------------------------------------%")
    cat("\n", nomes.arquivos[i])
    arquivo = data.frame(read.csv(nomes.arquivos[i]))
    nomes.datasets = arquivo$Datasets
    arquivo[is.na(arquivo)] <- 0
    arquivo2 = arquivo[,-1]
    
    # do primeiro método ao último
    final.teste = data.frame()
    j = 1
    for(j in 1:ncol(arquivo2)){
      cat("\n\t", my.methods[j])
      
      k = 1
      for(k in 1:length(my.methods)){
        cat("\n\t\t", my.methods[k])
        score.method.1 = arquivo2[,j]
        score.method.2 = arquivo2[,k]
        teste = data.frame(name.method.1 = my.methods[j], 
                           name.method.2 =  my.methods[k],
                           score.method.1, score.method.2)
        final.teste = rbind(final.teste, teste)
        k = k + 1
        gc()
      } # FIM DO SEGUNDO FOR
      
      j = j + 1
      gc()
    } # FIM DO PRIMEIRO FOR
    
    write.csv(final.teste, 
              paste(FolderWLT, "/", nomes.arquivos[i], sep=""), 
              row.names = FALSE)
    
    final.teste.certo = data.frame(filter(final.teste, 
                                          !(final.teste$name.method.1 == final.teste$name.method.2)))
    
    val = data.frame(filter(measures, measures$names == nomes[i]))
    
    cat("\n=======================================")
    if(val$values==1){
      cat("\n", nomes[i] ," valor = 1")
      resultado <- final.teste.certo %>%
        mutate(win = if_else(score.method.1 > score.method.2, 1, 0),
               loss = if_else(score.method.1 < score.method.2, 1, 0),
               tie = if_else(score.method.1  == score.method.2, 1, 0))
    } else {
      cat("\n", nomes[i] ," valor = 0")
      resultado <- final.teste.certo %>%
        mutate(win = if_else(score.method.1 < score.method.2, 1, 0),
               loss = if_else(score.method.1 > score.method.2, 1, 0),
               tie = if_else(score.method.1  == score.method.2, 1, 0))
    }
    cat("\n=======================================\n")
    
    #group by
    res.final <- resultado %>%
      group_by(name.method.1) %>%
      dplyr::summarise(win = sum(win), loss = sum(loss), tie = sum(tie)) %>%
      arrange(-win)
    names(res.final)[1] = "method"
    
    res.final = arrange(res.final, res.final$method)
    write.csv(res.final, 
              paste(FolderWLT, "/win-loss-tie-", nomes.arquivos[i], sep=""), 
              row.names = FALSE)
    
    todos = cbind(todos, res.final)
    
    i = i + 1
    gc()
    cat("\n%----------------------------------------------------------%\n")
  } # FIM DO WHILE
  
  write.csv(todos, 
            paste(FolderWLT, "/todos-win-loss-tie.csv", sep=""), 
            row.names = FALSE)
} # FIM DA MEDIDA

 
# 
# library("ggplot2")
# 
# df <- data.frame(wtls=rep(c("win", "loss", "tie"), each=3), 
#                  classifiers=c("ECC", "BR", "RFPCT", 
#                                "ECC", "BR", "RFPCT", 
#                                "ECC", "BR", "RFPCT"), 
#                  values=c(1,2,3,4,5,6,5,3,1))
# 
# df
# 
# ggplot(data=df, aes(x=classifiers, y=values, fill=wtls)) +
#   geom_bar(stat="identity", width = 0.5) + 
#   scale_fill_manual(values = c("purple", "green", "blue")) + 
#   coord_flip()
