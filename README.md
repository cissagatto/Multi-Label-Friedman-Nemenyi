# MULTILABEL FRIEDMAN NEMENYI
Compute Friedman and Nemenyi Statistical Tests for Multilabel Classification

## SCRIPTS
This source code has the following R scripts

1. libraries.r
2. utils.r
3. ranking.R
4. Friedman-Nemenyi-v1.R
5. Friedman-Nemenyi-v2.R
6. radar_plots.R
7. example_fn.R
8. example_radar.R


## BEFORE RUNNING
The *ranking.R* generates the rankings for each of the multilabel measures according to their best value as follow:

| Measure           | Best Value |
|-------------------|------------|
| accuracy          |     1.0    |
| average-precision |     1.0    |
| clp               |     0.0    |
| coverage          |     0.0    |
| f1                |     1.0    |
| hamming-loss      |     0.0    |
| macro-auc         |     1.0    |
| macro-f1          |     1.0    |
| macro-precision   |     1.0    |
| macro-recall      |     1.0    |
| margin-loss       |     0.0    |
| micro-auc         |     1.0    |
| micro-f1          |     1.0    |
| micro-precision   |     1.0    |
| micro-recall      |     1.0    |
| mlp               |     0.0    |
| one-error         |     0.0    |
| precision         |     1.0    |
| ranking-loss      |     0.0    |
| recall            |     1.0    |
| subset-accuracy   |     1.0    |
| wlp               |     0.0    |


The *Friedman-Nemenyi_v1.R* uses the *tsutils* package, while *Friedman-Nemenyi_v2.R* uses the *scmamp* package.

This source code does not provide the installation of any of the packages used, therefore, you must install the libraries used. Check *libraries.R*.

The scmamp package can give some installation headaches. For newer versions of R it is not available, so it is necessary to force the installation manually. Consult [here](https://github.com/b0rxa/scmamp) and [here] (http://cran.nexr.com/web/packages/scmamp/index.html) all necessary dependencies and I suggest you to install via *tar.gz*.

# CSV FILES
For this code to work correctly, the files must be provided in CSV format. This code was developed to work with the 22 multi-label assessment measures. You can provide just one CSV file, or all 22 files (one for each measure). The CSV file format of the assessment measure should be as follows:

| Dataset   | Method_1 | Method_2 | .... | Method_n |
|-----------|----------|----------|------|----------|
| dataset_1 |          |          |      |          |
| dataset_1 |          |          |      |          |
| ......... |          |          |      |          |
| dataset_n |          |          |      |          |

  

Along with this code are provided examples of csv files in the DATA folder

## RADAR PLOTS
You can generate radar plots for your data. An example is provided in *example_radar.R*

## HOW TO USE THIS CODE
You can see an example of how to use this code by referring to the *example_fn.R*.

## ACKNOWLEDGMENT
This study is financed in part by the Coordenação de Aperfeiçoamento de Pessoal de Nível Superior - Brasil (CAPES) - Finance Code 001

## LINKS
| [Post-Graduate Program in Computer Science](http://ppgcc.dc.ufscar.br/pt-br) | [Computer Department](https://site.dc.ufscar.br/) |  [Biomal](http://www.biomal.ufscar.br/) | [CAPES](https://www.gov.br/capes/pt-br) | [Embarcados](https://www.embarcados.com.br/author/cissa/) | [Read Prensa](https://prensa.li/@cissa.gatto/) | [Linkedin Company](https://www.linkedin.com/company/27241216) | [Linkedin Profile](https://www.linkedin.com/in/elainececiliagatto/) | [Instagram](https://www.instagram.com/cissagatto) | [Facebook](https://www.facebook.com/cissagatto) | [Twitter](https://twitter.com/cissagatto) | [Twitch](https://www.twitch.tv/cissagatto) | [Youtube](https://www.youtube.com/CissaGatto) |

## REPORT ERROR
Please contact me: elainececiliagatto@gmail.com

# THANKS
