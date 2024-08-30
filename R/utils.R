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


#' Create a List of Evaluation Metrics
#'
#' This function creates a data frame containing names of evaluation metrics,
#' their types, and the total count of metrics. Each metric is categorized by
#' its type, which is represented as an integer.
#'
#' @return A data frame with the following components:
#'   \itemize{
#'     \item \code{names}: A character vector of metric names.
#'     \item \code{type}: An integer vector representing the type of each metric.
#'   }
#'   The total number of metrics is equal to the length of the \code{names} vector.
#'
#' @details
#' The \code{type} vector categorizes metrics as follows:
#' \itemize{
#'   \item \code{1}: Metrics that are generally used for performance evaluation.
#'   \item \code{0}: Metrics that are less commonly used or have different evaluation criteria.
#' }
#' 
#' @examples
#' # Create the data frame of metrics
#' metric_df <- fn.measures()
#' 
#' # Print the data frame
#' print(metric_df)
#'
#' @export
fn.measures <- function() {
  
  # Define the metric names and their types
  names <- c("accuracy", 
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
             "subset-accuracy", 
             "wlp")
  
  type <- c(1, 
            1, 
            0, 
            0, 
            1, 
            0, 
            1, 
            1,
            1, 
            1, 
            1, 
            0, 
            1, 
            1, 
            1, 
            1, 
            1, 
            0, 
            0, 
            1, 
            0, 
            1, 
            1, 
            1, 
            1, 
            1, 
            0)
  
  # Create a data frame with the metric names and their types
  df <- data.frame(
    names = names,
    type = type,
    stringsAsFactors = FALSE
  )
  
  # Return the data frame
  return(df)
}

##############################################################################
# 
##############################################################################
