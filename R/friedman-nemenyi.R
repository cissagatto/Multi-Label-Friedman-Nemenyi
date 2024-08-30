##############################################################################
# Copyright (C) 2024                                                         #
#                                                                            #
# CC BY-NC-SA 4.0                                                            #
#                                                                            #
# Canonical URL https://creativecommons.org/licenses/by-nc-sa/4.0/           #
# Attribution-NonCommercial-ShareAlike 4.0 International CC BY-NC-SA 4.0     #
#                                                                            #
# Prof. Elaine Cecilia Gatto | Prof. Ricardo Cerri | Prof. Mauri Ferrandin   #
#                                                                            #
# Federal University of São Carlos - UFSCar - https://www2.ufscar.br         #
# Campus São Carlos - Computer Department - DC - https://site.dc.ufscar.br   #
# Post Graduate Program in Computer Science - PPGCC                          # 
# http://ppgcc.dc.ufscar.br - Bioinformatics and Machine Learning Group      #
# BIOMAL - http://www.biomal.ufscar.br                                       #
#                                                                            #
# You are free to:                                                           #
#     Share — copy and redistribute the material in any medium or format     #
#     Adapt — remix, transform, and build upon the material                  #
#     The licensor cannot revoke these freedoms as long as you follow the    #
#       license terms.                                                       #
#                                                                            #
# Under the following terms:                                                 #
#   Attribution — You must give appropriate credit , provide a link to the   #
#     license, and indicate if changes were made . You may do so in any      #
#     reasonable manner, but not in any way that suggests the licensor       #
#     endorses you or your use.                                              #
#   NonCommercial — You may not use the material for commercial purposes     #
#   ShareAlike — If you remix, transform, or build upon the material, you    #
#     must distribute your contributions under the same license as the       #
#     original.                                                              #
#   No additional restrictions — You may not apply legal terms or            #
#     technological measures that legally restrict others from doing         #
#     anything the license permits.                                          #
#                                                                            #
##############################################################################



##############################################################################
# WORSKSPACE
##############################################################################
FolderRoot = "~/MultiLabelFriedmanNemenyi"
FolderScripts = "~/MultiLabelFriedmanNemenyi/R"


#' Perform Friedman Test Followed by Nemenyi Post-Hoc Test
#'
#' This function performs the Friedman test on the provided data to assess
#' if there are significant differences among multiple methods. If the
#' Friedman test indicates significant differences, the function then performs
#' the Nemenyi post-hoc test to determine which methods are significantly different.
#' The results are saved as CSV and PDF files, including the difference matrix,
#' Critical Difference (CD) plot, and p-values plot.
#'
#' @param data A data frame containing the results of multiple methods. Each
#'   row should represent a different method, and columns should represent the
#'   observations or measurements.
#' @param save A string specifying the base name for output files. The function
#'   will use this base name to save the difference matrix, Critical Difference
#'   plot, and p-values plot.
#'
#' @return A data frame with the following columns:
#'   \itemize{
#'     \item \code{f.ChiSquare}: The Chi-Square statistic from the Friedman test.
#'     \item \code{f.pValue}: The p-value from the Friedman test.
#'     \item \code{f.Method}: The method used for the Friedman test.
#'     \item \code{n.CriticalDifference}: The Critical Difference from the Nemenyi test.
#'     \item \code{result}: A string indicating whether the methods are significantly different.
#'   }
#'
#' @details
#' The function saves three outputs:
#' \itemize{
#'   \item A CSV file with the name \code{paste(save, "-diff-matrix.txt", sep="")} containing
#'     the difference matrix from the Nemenyi post-hoc test.
#'   \item A PDF file with the name \code{paste(save, ".pdf", sep="")} containing the Critical
#'     Difference (CD) plot.
#'   \item A PDF file with the name \code{paste(save, "-pValues.pdf", sep="")} containing
#'     the p-values plot.
#' }
#'
#' @examples
#' # Example usage
#' # Assume 'my_data' is a data frame with results for different methods
#' # and 'base_name' is the base name for output files
#' result <- friedman.nemenyi(my_data, "base_name")
#'
#' @export
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
