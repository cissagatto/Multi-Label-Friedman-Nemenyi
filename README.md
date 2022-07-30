# Multi-Label-Friedman-Nemenyi
Compute Friedman and Nemenyi Statistical Tests for Multilabel Classification

## SCRIPTS

This source code has the following R scripts

1. libraries.r
2. utils.r
3. ranking.R
4. Friedman-Nemenyi-v1.R
5. Friedman-Nemenyi-v2.R
6. example.R


## BEFORE RUNNING

The R ranking script generates the rankings for each of the multi-label measures according to their maximum value. If the best result for the measure is ZERO, the ranking will be generated accordingly. If the best result for the measure is ONE, the ranking will be generated accordingly.

The Friedman-Nemenyi V1 script uses the *tsutils* package, while Friedman-Nemenyi_v2 uses the *scmamp* package.

This source code does not provide the installation of any of the packages used, therefore, you must install the libraries used. Check the R libraries.R script.

The scmamp package can give some installation headaches. For newer versions of R it is not available, so it is necessary to force the installation manually. Consult [here](https://github.com/b0rxa/scmamp) and [here] (http://cran.nexr.com/web/packages/scmamp/index.html) all necessary dependencies and I suggest you to install via files.

# CSV FILES

For this code to work correctly, the files must be provided in CSV format. This code was developed to work with the 22 multi-label assessment measures. You can provide just one CSV file, or all 22 files, it will be able to generate the critical distance graphs.

The CSV file format of the assessment measure should be as follows:



| Dataset   | Method_1 | Method_2 | .... | Method_n |
|-----------|----------|----------|------|----------|
| dataset_1 |          |          |      |          |
| dataset_1 |          |          |      |          |
| ......... |          |          |      |          |
| dataset_n |          |          |      |          |
|-----------|----------|----------|------|----------|

  

Along with this code are provided examples of csv files in the CSVs folder

# How to use this code

You can see an example of how to use this code by referring to the R script example.R


## Acknowledgment
This study is financed in part by the Coordenação de Aperfeiçoamento de Pessoal de Nível Superior - Brasil (CAPES) - Finance Code 001

## Links

| [Post-Graduate Program in Computer Science](http://ppgcc.dc.ufscar.br/pt-br) | [Computer Department](https://site.dc.ufscar.br/) |  [Biomal](http://www.biomal.ufscar.br/) | [CAPES](https://www.gov.br/capes/pt-br) | [Embarcados](https://www.embarcados.com.br/author/cissa/) | [Read Prensa](https://prensa.li/@cissa.gatto/) | [Linkedin Company](https://www.linkedin.com/company/27241216) | [Linkedin Profile](https://www.linkedin.com/in/elainececiliagatto/) | [Instagram](https://www.instagram.com/cissagatto) | [Facebook](https://www.facebook.com/cissagatto) | [Twitter](https://twitter.com/cissagatto) | [Twitch](https://www.twitch.tv/cissagatto) | [Youtube](https://www.youtube.com/CissaGatto) |

## Report Error

Please contact me: elainececiliagatto@gmail.com

# Thanks

