---
layout: frontpage
title: PredictiveEcology - NetLogoR
description: This is the main web page for the NetLogoR package for writing and running individual based models using the NetLogo dictionary, ported to R
---

<head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="chrome=1">
    <link rel="stylesheet" href="stylesheets/styles.css">
    <link rel="stylesheet" href="stylesheets/pygment_trac.css">
    <meta name="viewport" content="width=device-width, initial-scale=1, user-scalable=no">
</head>

# NetLogoR

NetLogoR is an R package which aims to help translating agent-based models built in NetLogo (Wilensky, 1999) into R or help directly with creating new agent-based models in R following the NetLogo framework.

NetLogoR provides the necessary NetLogo's primitives as well as complementary functions to build agent-based models. A programming guide derived from the NetLogo’s Programming Guide is available.

This package is under construction and therefore function errors and mismatches with the documentation may occur.

[**Programming Guide**](https://github.com/PredictiveEcology/NetLogoR/blob/master/vignettes/ProgrammingGuide.Rmd) [https://github.com/PredictiveEcology/NetLogoR/blob/master/vignettes/ProgrammingGuide.Rmd](https://github.com/PredictiveEcology/NetLogoR/blob/master/vignettes/ProgrammingGuide.Rmd) 


**GitHub Repository:** [https://github.com/PredictiveEcology/NetLogoR](https://github.com/PredictiveEcology/NetLogoR)


## Installation

Building packages from source requires the appropriate development libraries for your operating system (*e.g.*, Windows users should install [Rtools](http://cran.r-project.org/bin/windows/Rtools/)).


### Current stable release [![Build Status](https://travis-ci.org/PredictiveEcology/NetLogoR.svg?branch=master)](https://travis-ci.org/PredictiveEcology/NetLogoR) [![Coverage Status](https://coveralls.io/repos/PredictiveEcology/NetLogoR/badge.svg?branch=master)](https://coveralls.io/r/PredictiveEcology/NetLogoR?branch=master) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/NetLogoR)](https://cran.r-project.org/package=NetLogoR) [![Downloads](http://cranlogs.r-pkg.org/badges/NetLogoR)](https://cran.rstudio.com/package=NetLogoR)

**Install from CRAN:**

Not yet available on CRAN

**Install from GitHub:**
    
```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/NetLogoR", build_vignettes = TRUE) # stable
```

```


## Reporting bugs

Contact us via the package GitHub site: [https://github.com/PredictiveEcology/NetLogoR/issues](https://github.com/PredictiveEcology/NetLogoR/issues).

This project is maintained by Eliot McIntire (eliot.mcintire at canada.ca) and Alex Chubaty (alexander.chubaty at canada.ca)

<small>Hosted on GitHub Pages</small>
