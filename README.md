:package: SMRD: Statistical Methods for Reliability Data
================
Jason K. Freels, William Q. Meeker, and Luis A. Escobar
<br/>06 June 2019

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/SMRD2)](https://cran.r-project.org/package=SMRD2)
[![Travis build
status](https://travis-ci.org/Auburngrads/SMRD2.svg?branch=master)](https://travis-ci.org/Auburngrads/SMRD2)

## Background

Since its initial publication,
<a target=' 'href='https://www.amazon.com/Statistical-Methods-Reliability-William-Meeker/dp/0471143286'>**Statistical
Methods for Reliability Data**</a> has remained a foundational text for
analyzing time-to-failure (survival) data. Along with the first edition
of the text, the authors provided an S-Plus software package, called
[**SPLIDA**](http://www.public.iastate.edu/~splida/') (**S**-**P**lus
**LI**fe **D**ata **A**nalysis), to help readers utilize the methods for
industry data.

Today, R has supplanted S and S-Plus as the most popular statistical
computing language in the world. In response to this shift, Meeker began
an effort to translate **SPLIDA** into R under the name **RSplida**.
However, RSplida couldn’t be installed as a traditional R package and
was difficult to use with modern IDE’s. In 2015, Freels partnered with
Meeker to restructure and modernize RSplida into this R package
**SMRD**, with the aim of publishing to the
<a target="" href="https://cran.r-project.org">**CRAN**</a>.

## Installation

As this package has not yet been published, you can install the latest
development version from GitHub:

``` r
if (packageVersion("devtools") < 1.6) {
  
    install.packages("devtools")
  
}

devtools::install_github("Auburngrads/SMRD")
```

Please note that this package is currently experimental and is under
very active development. If you encounter any problems or unexpected
behaviours, please create a
<a target="" href="https://github.com/Auburngrads/SMRD2/issues">**new
issue**</a> and include a reporducible example.

## Getting started

Once installed, the easiest way get started using `SMRD` is by checking
out the echapters. These are documents that present example code and
results that correspond to what is shown in the each chapter of the 1st
edition of the SMRD text.

``` r
SMRD::echapter(chapter = 1)
```

## Getting involved

There are a number of remaining tasks to be completed before publishing
<b>(% complete)</b>

  - <a target="" href="https://github.com/Auburngrads/SMRD2/projects/1">**Update
    older R & S-Plus idioms to modern equivalents**</a>
  - <a target="" href="https://github.com/Auburngrads/SMRD2/projects/1">**Update
    graphics objects**</a>
  - <a target="" href="https://github.com/Auburngrads/SMRD2/projects/1">**Document
    datasets and exported functions**</a>
  - <a target="" href="https://github.com/Auburngrads/SMRD2/projects/2">**Update
    for modern use-cases - literate programming/interactivity**</a>
