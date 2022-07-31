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
cores = seecol(pal_unikn_pref, 22)
a <- c(1:22)
cores2 <- colorRampPalette(c("red","yellow")) # Degradê de vermelho para amarelo



##############################################################################
#
##############################################################################
create_beautiful_radarchart <- function(data, color = cores3,
                                        vlabels = colnames(data),
                                        vlcex = 0.7,
                                        caxislabels = NULL,
                                        title = NULL, ...){
  radarchart(
    
    data, axistype = 1,
    
    # Customize the polygon
    pcol = color,
    pfcol = scales::alpha(color, 0.1),
    plwd = 2, 
    plty = 1,
    
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    
    # Customize the axis
    # axislabcol = color,
    axislabcol = "black",
    
    # Variable labels
    vlcex = vlcex, 
    vlabels = vlabels,
    caxislabels = caxislabels, 
    title = title, ...
  )
}


##############################################################################
#
##############################################################################
radar_plot_1 <- function(g, df, name, folder){
  
  i = 1
  for(i in 1:g) {
    cat("\n\n Radar Graph: \t", name[i])
    colors <- cores
    setwd(folder)
    pdf(paste(name[i], ".pdf", sep=""), width = 6, height = 6)
    op <- par(mar = c(1, 2, 2, 1))
    create_beautiful_radarchart(d[c(1, 2, i+2), ],
                                caxislabels = seq(0.0,1.0,0.2),
                                color = colors[i], title = name[i])
    # color = colors[i]
    print(par(op))
    dev.off()
    cat("\n")
    i = i + 1
    gc()
  }
}

##############################################################################
#
##############################################################################
radar_plot_2 <- function(x,y,df,name,folder){
  
  colors <- cores
  setwd(folder)
  pdf("all.pdf", width = 5, height = 10)
  
  # Reduce plot margin using par()
  op <- par(mar = c(1, 1, 1, 1), oma=c(1,1,1,1))
  par(mfrow = c(x,y))
  
  # Create the radar chart
  i = 1
  for(i in 1:(x*y)){
    create_beautiful_radarchart(
      data = d[c(1, 2, i+2), ], caxislabels = seq(0,1.0,0.2),
      color = colors[i], title = name[i]
    )
    i = i + 1
    gc()
  }
  par(op)
  print(par(op))
  dev.off()
  cat("\n")
}


##############################################################################
#
##############################################################################


