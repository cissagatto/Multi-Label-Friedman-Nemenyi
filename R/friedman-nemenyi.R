##############################################################################
# Copyright (C) 2024                                                         #
#                                                                            #
# This code is free software: you can redistribute it and/or modify it under #
# the terms of the GNU General Public License as published by the Free       #
# Software Foundation, either version 3 of the License, or (at your option)  #
# any later version. This code is distributed in the hope that it will be    #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General   #
# Public License for more details.                                           #
#                                                                            #
# Prof. Elaine Cecilia Gatto | Prof. Ricardo Cerri | Prof. Mauri Ferrandin   #
#                                                                            #
# Federal University of São Carlos - UFSCar - https://www2.ufscar.br         #
# Campus São Carlos - Computer Department - DC - https://site.dc.ufscar.br   #
# Post Graduate Program in Computer Science - PPGCC                          #
# http://ppgcc.dc.ufscar.br - Bioinformatics and Machine Learning Group      #
# BIOMAL - http://www.biomal.ufscar.br                                       #
#                                                                            #
##############################################################################



##############################################################################
# WORSKSPACE
##############################################################################
FolderRoot = "~/Multi-Label-Friedman-Nemenyi"
FolderScripts = "~/Multi-Label-Friedman-Nemenyi/R"




##############################################################################
# Function: friedman.nemenyi
# Description: Perform Friedman test followed by Nemenyi post-hoc test.
# Parameters:
#   data - A data frame containing the results of multiple methods.
#   save - A string specifying the base name for output files.
# Returns:
#   A data frame with the test statistics and results.
##############################################################################
friedman.nemenyi <- function(data, save) {
  
  # Perform Friedman test
  fr <- friedmanTest(data)
  
  # Perform Nemenyi post-hoc test
  ne <- nemenyiTest(data, alpha=0.05)
  
  # Extract test statistics
  f.ChiSquare <- as.numeric(fr$statistic)
  f.pValue <- as.numeric(fr$p.value)
  f.Method <- toString(fr$method)
  n.CriticalDifference <- as.numeric(ne$statistic)
  
  # Determine if the methods are significantly different
  result <- ifelse(f.pValue < 0.05, 
                   "Methods are different", 
                   "Methods are not different")
  
  # Combine all results into a data frame
  all <- data.frame(f.ChiSquare, f.pValue, f.Method, 
                    n.CriticalDifference, result)
  rownames(all) <- NULL
  
  # Save the difference matrix to a CSV file
  name <- paste(save, "-diff-matrix.txt", sep="")
  write.csv(data.frame(ne$diff.matrix), name)
  
  # Save the Critical Difference (CD) plot as a PDF
  pdf(paste(save, ".pdf", sep=""), width = 13, height = 7)
  print(plotCD(data, alpha=0.05, cex=3))
  dev.off()
  
  # Save the p-values plot as a PDF
  pdf(paste(save, "-pValues.pdf", sep=""), width = 10, height = 6)
  print(plotPvalues(ne$diff.matrix, show.pvalue = TRUE, font.size = 2))
  dev.off()
  
  # Return the results data frame
  return(all)
}



##############################################################################
# 
##############################################################################
