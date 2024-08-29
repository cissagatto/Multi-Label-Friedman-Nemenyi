# Multi-Label Friedman-Nemenyi Analysis Toolkit

Welcome to the **Multi-Label Friedman-Nemenyi Analysis Toolkit**! This powerful suite is designed to streamline the process of ranking and analyzing multi-label datasets using the Friedman-Nemenyi test, a key statistical method for comparing multiple algorithms or classifiers.

## 🚀 **Features**

- **Batch Processing:** Seamlessly handle multiple CSV files in a single run.
- **Versatile Ranking Methods:** Compute rankings with various tie-breaking strategies including first, last, average, random, minimum, and maximum.
- **Comprehensive Statistical Analysis:** Utilize the Friedman-Nemenyi test to rigorously evaluate and compare multiple methods or classifiers.
- **Customizable Outputs:** Save results to organized folders with intuitive naming conventions for easy reference and further analysis.

## 📥 **Getting Started**

### Prerequisites

- R (version 4.0 or higher)
- Necessary R packages: `dplyr`, `tools`, `ggplot2`, and any other dependencies listed in `libraries.R`.

### Installation

1. **Clone the Repository:**

   ```sh
   git clone https://github.com/yourusername/multi-label-friedman-nemenyi.git
   ```

2. **Set Up Your Environment:**

   Open the R console or RStudio and navigate to the directory containing the scripts:

   ```r
   setwd("~/path-to-cloned-repo/multi-label-friedman-nemenyi/R")
   source("libraries.R")  # Load all required libraries
   source("utils.R")      # Load utility functions
   source("ranking.R")    # Load ranking functions
   source("friedman-nemenyi.R")  # Load Friedman-Nemenyi functions
   ```

3. **Prepare Your Data:**

   Place your CSV files into the `Data` directory. Ensure that each CSV file corresponds to a different measure or dataset.

### Usage

1. **Set Up Your Working Directories:**

   Configure your working directories by setting the `FolderRoot`, `FolderScripts`, `FolderData`, and `FolderResults` paths in the `main.R` script.

2. **Process Your Data:**

   The R MAIN script has a complete example of how to use the functions.


### Examples

Here are some examples of how to use the toolkit:

**Processing a Single File:**

```r
data <- read.csv("path/to/your/file.csv")
ranking <- generate.ranking(data)
```

**Processing Multiple Files:**

```r
setwd(FolderData)
files <- list.files(pattern = "\\.csv$", full.names = TRUE)
for (file in files) {
  data <- read.csv(file)
  ranking <- generate.ranking(data)
  # Additional processing...
}
```

### Documentation

For more detailed documentation on each function, check out the comments in the R scripts. Each function is thoroughly documented to help you understand its purpose and parameters.

## 📚 **Contributing**

We welcome contributions from the community! If you have suggestions, improvements, or bug fixes, please submit a pull request or open an issue in the GitHub repository.

## 📧 **Contact**

For any questions or support, please contact:
- **Prof. Elaine Cecilia Gatto** (email@example.com)
- **Prof. Ricardo Cerri** (email@example.com)
- **Prof. Mauri Ferrandin** (email@example.com)

Thank you for using the Multi-Label Friedman-Nemenyi Analysis Toolkit. We hope this tool helps you in your multi-label classification tasks!

---

**Happy Analyzing!** 🎉



## Acknowledgment
- This study was financed in part by the Coordenação de Aperfeiçoamento de Pessoal de Nível Superior - Brasil (CAPES) - Finance Code 001.
- This study was financed in part by the Conselho Nacional de Desenvolvimento Científico e Tecnológico - Brasil (CNPQ) - Process number 200371/2022-3.
- The authors also thank the Brazilian research agencies FAPESP financial support.


# Contact
elainececiliagatto@gmail.com

# Links

| [Site](https://sites.google.com/view/professor-cissa-gatto) | [Post-Graduate Program in Computer Science](http://ppgcc.dc.ufscar.br/pt-br) | [Computer Department](https://site.dc.ufscar.br/) |  [Biomal](http://www.biomal.ufscar.br/) | [CNPQ](https://www.gov.br/cnpq/pt-br) | [Ku Leuven](https://kulak.kuleuven.be/) | [Embarcados](https://www.embarcados.com.br/author/cissa/) | [Read Prensa](https://prensa.li/@cissa.gatto/) | [Linkedin Company](https://www.linkedin.com/company/27241216) | [Linkedin Profile](https://www.linkedin.com/in/elainececiliagatto/) | [Instagram](https://www.instagram.com/cissagatto) | [Facebook](https://www.facebook.com/cissagatto) | [Twitter](https://twitter.com/cissagatto) | [Twitch](https://www.twitch.tv/cissagatto) | [Youtube](https://www.youtube.com/CissaGatto) |
