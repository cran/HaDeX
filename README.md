[![Build Status](https://api.travis-ci.org/hadexversum/HaDeX.png)](https://travis-ci.org/hadexversum/HaDeX)
[![Coverage status](https://codecov.io/gh/hadexversum/hadex/branch/master/graph/badge.svg)](https://codecov.io/github/hadexversum/hadex?branch=master)

![HaDeX](https://raw.githubusercontent.com/hadexversum/HaDeX/master/inst/HaDeX/www/mock_logo.png)

## HaDeX: Analysis and Visualisation of Hydrogen/Deuterium Exchange Mass Spectrometry Data 

Hydrogen/Deuterium eXchange Mass Spectrometry (HDX-MS) is a staple technology in structural proteomics. HaDeX provides a analytic workflow for HDX-MS data availble as a standalone GUI (https://sourceforge.net/projects/HaDeX/), web server (http://mslab-ibb.pl/shiny/HaDeX/) and the **R** package. 

### Local instance of HaDeX GUI

To run HaDeX GUI locally on Windows, install it using the following binary file: https://sourceforge.net/projects/HaDeX/files/HaDeX_setup.exe/download

### Local instance of HaDeX package

You can install the latest development version of the package:

```R
source("https://install-github.me/hadexversum/HaDeX")
```

After installation GUI can be accessed locally:

```R
library(HaDeX)
HaDeX_gui()
```

### Online manual

The HaDeX documentation is available [online](https://HaDeXversum.github.io/HaDeX/).

### Citation

Puchala W, Burdukiewicz M, Kistowski M, Dabrowska KA, Badaczewska-Dawid AE, Cysewski D and Dadlez M (2019). HaDeX: Analysis and Visualisation of Hydrogen/Deuterium Exchange Mass Spectrometry Data. R package version 0.1.

### Funding  

This work is supported by Foundation of Polish Science (TEAM TECH CORE FACILITY/2016-2/2 *Mass Spectrometry of Biopharmaceuticals - improved methodologies for qualitative, quantitative and structural characterization of drugs, proteinaceous drug targets and diagnostic molecules)*.
