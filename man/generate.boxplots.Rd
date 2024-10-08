% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/friedman-nemenyi.R
\name{generate.boxplots}
\alias{generate.boxplots}
\title{Generate Boxplots of Method Performance}
\usage{
generate.boxplots(data, methods, save.dir)

generate.boxplots(data, methods, save.dir)
}
\arguments{
\item{data}{A data frame containing performance metrics for different methods.}

\item{methods}{A character vector specifying the names of the methods (columns)
in the data frame.}

\item{save.dir}{A character string specifying the directory where the boxplots 
will be saved.}
}
\value{
A boxplot of the performance of the specified methods.

A boxplot of the performance of the specified methods with mean labels.
}
\description{
This function generates boxplots for the performance of various methods
based on the provided data. It saves the plots as PDF files in the specified
directory.

This function generates boxplots for the performance of various methods
and replaces method names on the x-axis with their mean performance.
It also removes the legend from the plot.
}
\examples{
generate.boxplots(data = results_data, methods = c("Method1", "Method2"), save.dir = "output_directory")
generate.boxplots(data = results_data, methods = c("Method1", "Method2"), save.dir = "output_directory")
}
