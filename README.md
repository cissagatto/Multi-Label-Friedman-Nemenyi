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
  doi = {10.13140/RG.2.2.17865.35687},
  url = {https://github.com/cissagatto/MultiLabelFriedmanNemenyi},
}
```

## 📥 **Getting Started**


### Prerequisites

- R (version 4.0 or higher)
- Necessary R packages: `dplyr`, `tools`, `ggplot2`, and any other dependencies listed in `libraries.R`.



### Installation

Install the library:

   ```r
     library(MultiLabelFriedmanNemenyi)
   ```


### Examples

Here are some examples of how to use the toolkit:

**Processing a Single File:**

```r

setwd(FolderRoot)
clp = data.frame(read.csv("~/MultiLabelFriedmanNemenyi/Data/clp.csv"))
clp = clp[,-1]

df_res.mes <- fn.measures()
filtered_res.mes <- filter(df_res.mes, names == "clp")
save = paste(FolderResults, "/clp", sep="")

if(filtered_res.mes$type==1){
  # if the measure is type 1, isto é, o melhore valor é um
    res = friedman.nemenyi(data = clp, save = save)
  
} else {
  # if the measure is type 0, isto é, o melhor valor é zero
  
  ranking = generate.ranking(data = clp)
  res.data = data.frame(ranking$rank.average.1) 
  res.fn = friedman.nemenyi(data = res.data, save = save)
}

```

**Processing Multiple Files:**

```r

setwd(FolderData)
current_dir <- getwd()
files <- list.files(pattern = "\\.csv$", full.names = TRUE)  # List all CSV files
full_paths <- sapply(files, function(file) normalizePath(file))

# Process each CSV file
for (file_path in full_paths) {
  # Read the CSV file
  data_name <- basename(file_path)  # Extract file name
  data <- data.frame(read.csv(file_path))
  
  # Remove the first column
  data <- data[, -1]
  
  # Generate rankings
  ranking <- generate.ranking(data = data)
  
  # Get measure name from the file name (assuming the file name indicates the measure)
  measure_name <- tools::file_path_sans_ext(data_name)
  
  # Load measures data
  df_res.mes <- fn.measures()
  filtered_res.mes <- filter(df_res.mes, names == measure_name)
  
  # Define the path to save results
  save_path <- paste(FolderResults, "/", measure_name, sep = "")
  
  if (filtered_res.mes$type == 1) {
    # If the measure is type 1, i.e., the best value is one
    res <- friedman.nemenyi(data = data, save = save_path)
    
  } else {
    # If the measure is type 0, i.e., the best value is zero
    res_data <- data.frame(ranking$rank.average.1) 
    res_fn <- friedman.nemenyi(data = res_data, save = save_path)
  }
  
  cat("\nProcessed file:", data_name)
}

```

### Documentation

For more detailed documentation on each function, check out the `~/MultiLabelFriedmanNemenyi/docs` folder


### R Example

You can fin a complete script R example in `~/MultiLabelFriedmanNemenyi/example` folder


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
