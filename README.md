# Multi-Label Friedman-Nemenyi Analysis Toolkit

Welcome to the **Multi-Label Friedman-Nemenyi Analysis Toolkit**! This powerful suite is designed to streamline the process of ranking and analyzing multi-label datasets using the Friedman-Nemenyi test, a key statistical method for comparing multiple algorithms or classifiers.

## 🚀 **Features**

- **Batch Processing:** Seamlessly handle multiple CSV files in a single run.
- **Versatile Ranking Methods:** Compute rankings with various tie-breaking strategies including first, last, average, random, minimum, and maximum.
- **Comprehensive Statistical Analysis:** Utilize the Friedman-Nemenyi test to rigorously evaluate and compare multiple methods or classifiers.
- **Customizable Outputs:** Save results to organized folders with intuitive naming conventions for easy reference and further analysis.

## How to Cite

```plaintext
@misc{MLFN2024,
  author = {Elaine Cecília Gatto},
  title = {MultiLabelFriedmanNemenyi: A package for multi-label Friedman-Nemenyi analysis},  
  year = {2024},
  note = {R package version 0.1.0 Licensed under CC BY-NC-SA 4.0},
  doi = {10.13140/RG.2.2.17865.35687/1},
  url = {https://github.com/cissagatto/MultiLabelFriedmanNemenyi}
}
```

## 📥 **Getting Started**


### Prerequisites

- R (version 4.0 or higher)
- Necessary R packages: `dplyr`, `tools`, `ggplot2`, `stringr`, `scmamp`, `openxlsx`, `writexl`.



### Installation

    
```r
# install.packages("devtools")
library("devtools")
devtools::install_github("https://github.com/cissagatto/MultiLabelFriedmanNemenyi")
library(MultiLabelFriedmanNemenyi)
```

### Examples

Here are some examples of how to use the toolkit:

**For Measures with the best value equal to 0**

```r

setwd(FolderRoot)
clp = data.frame(read.csv("~/MultiLabelFriedmanNemenyi/Data/clp.csv"))
clp = clp[,-1]

df_res.mes <- fn.measures()
filtered_res.mes <- filter(df_res.mes, names == "clp")

save = paste(FolderResults, "/clp", sep="")
if(dir.exists(save)==FALSE){dir.create(save)}

ranking = generate.ranking(data = clp)
res.data = data.frame(ranking$rank.average.1) 
res.fn = friedman.nemenyi(data = res.data , 
                          save = save,
                          measure.name = "clp",
                          width = 60, 
                          height = 30,
                          cex=5.5)
```

**For Measures with the best value equal to 1**

```r

setwd(FolderRoot)
accuracy = data.frame(read.csv("~/MultiLabelFriedmanNemenyi/Data/accuracy.csv"))
accuracy = accuracy [,-1]

df_res.mes <- fn.measures()
filtered_res.mes <- filter(df_res.mes, names == "accuracy")

save = paste(FolderResults, "/accuracy", sep="")
if(dir.exists(save)==FALSE){dir.create(save)}

res = friedman.nemenyi(data = accuracy, 
                       save = save,
                       measure.name = "accuracy",
                       width = 60, 
                       height = 30,
                       cex=5.5)

```

**Processing Multiple CSV Files:**

```r

# Set the working directory to the data folder
setwd(FolderData)
current.dir <- getwd()

# List all CSV files with full paths
files <- list.files(pattern = "\\.csv$", full.names = TRUE)
full.paths <- sapply(files, function(file) normalizePath(file))

# Initialize a data frame to store the concatenated results
all.results <- data.frame()

# Process each CSV file
for (file.path in full.paths) {
  
  
  #file.path = "C:/Users/Cissa/Documents/MultiLabelFriedmanNemenyi/Data/accuracy.csv"
  # Extract the file name
  data.name <- basename(file.path)
  
  # Read the CSV file into a data frame
  data <- data.frame(read.csv(file.path))
  
  # Remove the first column
  data <- data[, -1]
  
  # Generate rankings
  ranking <- generate.ranking(data = data)
  
  # Extract the measure name from the file name
  measure.name <- tools::file_path_sans_ext(data.name)
  
  # Load and filter measures data
  df.res.mes <- fn.measures()
  filtered.res.mes <- filter(df.res.mes, names == measure.name)
  
  # Define the path to save the results
  save.path <- file.path(FolderResults, measure.name)
  if (!dir.exists(save.path)) {
    dir.create(save.path)
  }
  
  # Save the rankings to an Excel file
  file.name <- file.path(save.path, paste0(measure.name, "-ranking.xlsx"))
  save.dataframes.to.excel(data.list = ranking, file.name = file.name)
  
  # Run Friedman-Nemenyi test and store the results
  if (filtered.res.mes$type == 1) {
    # If the measure is type 1, the best value is one
    res <- friedman.nemenyi(data = data, 
                            save = save.path,
                            measure.name = measure.name,
                            width = 60, 
                            height = 30,
                            cex = 5.5)
    
    # data, save.dir, measure.name, width = 7, height = 5, cex = 1.2
    
  } else {
    # If the measure is type 0, the best value is zero
    res.data <- data.frame(ranking$rank.average.1)
    res <- friedman.nemenyi(data = res.data, 
                            save = save.path,
                            measure.name = measure.name,
                            width = 60, 
                            height = 30,
                            cex = 5.5)
  }
  
  # Concatenate the result to the all.results data frame
  all.results <- rbind(all.results, res)
  
  # plotting boxplots
  methods.names <- colnames(data)  
  generate.boxplots(data = data,
                    methods = methods.names, 
                    save.dir = save.path,
                    measure.name = measure.name,
                    width = 20,
                    height = 10)
  
  # Log the processed file
  cat("\nProcessed file:", data.name, "\n")
}


measures <- tools::file_path_sans_ext(basename(files))
nome.arquivo = paste(FolderResults, "/FN-results.xlsx", sep="")
res = data.frame(measures, all.results)
write_xlsx(res, nome.arquivo)
```

### Documentation

For more detailed documentation on each function, check out the `~/MultiLabelFriedmanNemenyi/docs` folder.
A complete example is available in `~/MultiLabelFriedmanNemenyi/example` folder.


### Folder Structure

Ensure the following folder structure is set up:

- `FolderRoot`: Root directory of the project.
- `FolderData`: Directory where CSV data files are stored.
- `FolderResults`: Directory where results and plots are saved.


## 📚 **Contributing**

We welcome contributions from the community! If you have suggestions, improvements, or bug fixes, please submit a pull request or open an issue in the GitHub repository.

## 📧 **Contact**

For any questions or support, please contact:
- **Prof. Elaine Cecilia Gatto** (elainececiliagatto@gmail.com)
  
Thank you for using the Multi-Label Friedman-Nemenyi Analysis Toolkit. We hope this tool helps you in your multi-label classification tasks!



## Acknowledgment
- This study was financed in part by the Coordenação de Aperfeiçoamento de Pessoal de Nível Superior - Brasil (CAPES) - Finance Code 001.
- This study was financed in part by the Conselho Nacional de Desenvolvimento Científico e Tecnológico - Brasil (CNPQ) - Process number 200371/2022-3.
- The authors also thank the Brazilian research agencies FAPESP financial support.


# Links

| [Site](https://sites.google.com/view/professor-cissa-gatto) | [Post-Graduate Program in Computer Science](http://ppgcc.dc.ufscar.br/pt-br) | [Computer Department](https://site.dc.ufscar.br/) |  [Biomal](http://www.biomal.ufscar.br/) | [CNPQ](https://www.gov.br/cnpq/pt-br) | [Ku Leuven](https://kulak.kuleuven.be/) | [Embarcados](https://www.embarcados.com.br/author/cissa/) | [Read Prensa](https://prensa.li/@cissa.gatto/) | [Linkedin Company](https://www.linkedin.com/company/27241216) | [Linkedin Profile](https://www.linkedin.com/in/elainececiliagatto/) | [Instagram](https://www.instagram.com/cissagatto) | [Facebook](https://www.facebook.com/cissagatto) | [Twitter](https://twitter.com/cissagatto) | [Twitch](https://www.twitch.tv/cissagatto) | [Youtube](https://www.youtube.com/CissaGatto) |


---

Start making good decisions with the Multi-label Friedman Nemenyi Tool today! 🚀 🎉
