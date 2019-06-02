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

## Overview

  - ***Statistical Methods for Reliability Data*** (Meeker & Escobar) is
    a foundational text for analyzing failure-time and survival data

  - Along with the text, the authors developed an S-Plus software
    package to utilize the methods for industry data

  - Today, R is the most popular statistical computing language in the
    world - largely supplanting S

  - This presentation introduces tools under-development for use with
    the current and future versions of the ***SMRD*** to
    
      - Analyzing industry and, laboratory test data
      - Simplify reliability/survivability instruction in the classroom

## The ***SMRD*** Toolkit

1.  R Package `SMRD`
    
      - Implements methods from the text to reproduce results from the
        text
      - Implements methods from the text for use on industry data
      - Interactive Shiny gadgets to instantly perform reliability
        analyses and generate reports

2.  ***Statistical Methods for Reliability Data in R***
    
      - Expanded package documentation and example vignettes
      - In-depth examples corresponding to each chapter in the
        ***SMRD*** text

3.  R package `teachingSMRD`
    
      - Examples from the ***SMRD*** reproduced as interactive shiny
        apps
      - Automatic generation of assigments and solutions for ***SMRD***
        exercises

# Background | The R Languange & R Packages

## The R Project for Statistical Computing

  - A statistical programming environment for data analysis and graphics

  - Developed by Ross Ihaka and Robert Gentleman at the University of
    Auckland

  - Open-source implementation of the ‘S’ language created by Becker et.
    al. at Bell Labs

  - A pre-eminent tool for statistics and data science

  - One of the fastest growing technical computing languages in the
    world
    
      - Used for data processing and visualization, computational
        statistics, and natural language processing etc.
    
      - Heavily used by Google, Facebook, Twitter, Microsoft, etc.

## R Packages

  - In R, the fundamental unit of shareable code is called a package

  - Packages bundle together code, data, documentation, and tests to
    easily share analysis methods with others

  - Currently 14289 packages are available on the Comprehensive R
    Archive Network ([CRAN](https://cran.r-project.org))

  - Many more available from the
    [Bioconductor](https://www.bioconductor.org) and
    [GitHub](https://github.com) repositories

  - The huge variety of packages is a key reason why R is so popular
    
      - Chances are that someone has already solved a problem that
        you’re working on
    
      - You can benefit from their work by downloading their package

# R Package `SMRD` | Development Process and Package Features

## `SMRD` - Development Process

  - Meeker developed a large collection of FORTRAN subroutines as part
    of contracted efforts

  - Meeker & Escobar wrapped the FORTRAN code into an S-Plus package
    called [*SPLIDA*](http://www.public.iastate.edu/~splida/')
    (**S**-**P**lus **LI**fe **D**ata **A**nalysis)

  - *SPLIDA* serves as the companion software for
    <a target=' 'href='https://www.amazon.com/Statistical-Methods-Reliability-William-Meeker/dp/0471143286'>Statistical
    Methods for Reliability Data</a> 1st ed.

  - Meeker began an effort to translate *SPLIDA* into R under the name
    *RSplida*
    
      - Not user-friendly - couldn’t be installed as a traditional R
        package
    
      - Difficult to use with modern IDE’s (i.e. RStudio, Visual Studio,
        Eclipse, etc.)

## `SMRD` - Development Process (cont.)

  - (**2015**) Freels & Meeker sign MOU to share FORTRAN code for
    purpose of developing an R package

  - Aim to publish `SMRD` to the CRAN in 2018

  - Remaining tasks to be completed before publishing <b>(%
    complete)</b>
    
      - (90%) Update older R & S-Plus idioms to modern equivalents
      - (75%) Update graphics objects
      - (75%) Document datasets
      - (75%) Update for modern use-cases - literate
        programming/interactivity
      - (50%) Ensure compatibility with modern dependencies
      - (15%) Document exported functions
      - (10%) Translate FORTRAN code over C++

## `SMRD` Package Features | Estimation/prediction methods for many types of failure data

  - Multiple failure events
  - Censored data (right, left, and interval censoring)
  - Truncated data (right, left, and interval truncation)
  - Failure data with explanatory variables (failure-time regression)
  - Repeated measures degradation data (linear & non-linear mixed
    effects)
  - Repairable system failure data (recurring events)
  - Physical/performance degradation data
  - Failure data with prior information (Bayesian reliability)
  - Reliability growth test data
  - Reliability test simulations

## `SMRD` Package Features | Minimal data pre-processing through flexible event definitions

<ul>

<li>

Organizations often use different terms to describe the same event

</li>

<ul>

<li>

‘Failure’ = ‘Failed’ = ‘Fail’ = ‘dead’ = ‘died’

</li>

<li>

‘right’ = ‘rcensored’ = ‘suspended’ = ‘alive’

</li>

<li>

‘left’ = ‘doa’ = ‘lcensored’

</li>

<li>

‘interval’ = ‘int’ = ‘icensored’ = ‘grouped’

</li>

</ul>

<li>

Many applications force users to recode these events

</li>

<li>

`SMRD` allows for flexible event definitions to utilize the data as-is

</li>

<li>

<focus>Event definitions can even be mixed</focus>

</li>

<li>

`SMRD` event definitions easily mapped to `survival` numeric definitions

</li>

</ul>

## `SMRD` Default Event Definitions

## `SMRD` Package Features | Easily Access Data from Multiple Sources

  - `SMRD` includes over 120 fully-documented datasets

  - For importing external data, `SMRD` leverages several other R
    packages

  - Excel files
    
      - `XLConnect`, `readxl`, `xlsx`

  - CSV/TSV files
    
      - `base`, `utils`, `readr`, `data.table`

  - Info, Minitab, S, SAS, SPSS, Stata, Systat and Weka files
    
      - `foreign`, `HMISC`

## `SMRD` Package Features | Faster workflows through literate programming

**Literate programming:** integrating text with snippets of executable
code in documents & presentations

  - In R, literate programming is supported by the `knitr` and
    `rmarkdown` packages
    
      - Weave code from multiple languages, \(\LaTeX\)-typeset
        equations, and text together in one document
      - Run and compile `code`, <green>\(\LaTeX\)</green>,
        <focus>text</focus> simultaneously
      - Everything is stored in a single file
      - Output to any of a number of presentation or document formats

## `SMRD` Package Features | Faster workflows through literate programming

  - With *SPLIDA*, many results were returned simultaneously
    
      - Graphics
      - Numeric values
      - Tables of results
      - Text summaries

  - For GUI-based software tools, presenting multiple results is
    <green>GOOD</green>

  - For tools emphasizing reproducible research and literate
    programming, presenting multiple results simultaneously is
    <red>BAD</red>
    
      - `SMRD` built to support literate programming - go from data to
        report fast
      - Ensure that specific results can be produced and called where
        desired

# A Quick `SMRD` Example | Analyzing the shockabsorber dataset

## Dataset: `shockabsorber`

  - This example demonstrates some of the `SMRD` functions to analyze
    the shockabsorber dataset used throughout the text

<!-- end list -->

``` r
DT::datatable(shockabsorber, options = list(pageLength = 6))
```

    PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.

<!--html_preserve-->

<div id="htmlwidget-9cd5365a3281a3ec4314" class="datatables html-widget" style="width:100%;height:auto;">

</div>

<script type="application/json" data-for="htmlwidget-9cd5365a3281a3ec4314">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38"],[6700,6950,7820,8790,9120,9660,9820,11310,11690,11850,11880,12140,12200,12870,13150,13330,13470,14040,14300,17520,17540,17890,18450,18960,18980,19410,20100,20100,20150,20320,20900,22700,23490,26510,27410,27490,27890,28100],["Mode1","Censored","Censored","Censored","Mode2","Censored","Censored","Censored","Censored","Censored","Censored","Censored","Mode1","Censored","Mode2","Censored","Censored","Censored","Mode1","Mode1","Censored","Censored","Censored","Censored","Censored","Censored","Mode2","Censored","Censored","Censored","Mode2","Mode1","Censored","Mode1","Censored","Mode1","Censored","Censored"],["Failure","Censored","Censored","Censored","Failure","Censored","Censored","Censored","Censored","Censored","Censored","Censored","Failure","Censored","Failure","Censored","Censored","Censored","Failure","Failure","Censored","Censored","Censored","Censored","Censored","Censored","Failure","Censored","Censored","Censored","Failure","Failure","Censored","Failure","Censored","Failure","Censored","Censored"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>miles<\/th>\n      <th>mode<\/th>\n      <th>event<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":6,"columnDefs":[{"className":"dt-right","targets":1},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[6,10,25,50,100]}},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

## Creating `life.data` Objects

  - Many of the methods in the package require a `life.data`-class
    object

<!-- end list -->

``` r
shock.ld <- frame.to.ld(frame = shockabsorber,
                        response.column = 1,
                        failure.mode.column = 2,
                        censor.column = 3,
                        time.units = 'Kilometers')
```

  - Since *SPLIDA* was written as a GUI, many functions to produce
    results and graphics already existed

  - Thus, once the `life.data` object has been created, many different
    plots and numeric results can be produced, each requiring only a
    single line of code

## Producing Results From `life.data` Objects

<div class="columns-2">

  - Nonparametric CDF plots

  - Parametric CDF plots

  - ML CDF and hazard plots

  - Explanatory variable plots

  - Multi-failure mode plots

  - Relative likelihood surfaces

  - Relative likelihood curves

<br/>

  - \(F(t)\) at specified values of \(t\)

  - \(h(t)\) at specified values of \(t\)

  - \(t^{-1}(p)\) at specified values of \(p\)

  - ML parameter estimates and standard errors

  - Logit and log transformed confidence intervals (pointwise and
    simultaneous)

</div>

## Nonparametric & Parametric CDF plots

``` r
plot(shock.ld)
plot(shock.ld, distribution = 'lognormal')
```

<img src="README_files/figure-gfm/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

## ML Plots \(F(t)\) & \(h(t)\)

``` r
mlehazplot(shock.ld,  distribution = 'lognormal', param.loc = 'topleft')
mleprobplot(shock.ld, distribution = 'weibull', param.loc = 'topleft')
```

<img src="README_files/figure-gfm/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

## ML Surface Plots

``` r
simple.contour(shock.ld, distribution = 'sev', threeD = T, original.par = F)
simple.contour(shock.ld, distribution = 'sev', show.confidence = F, zoom = 1.75)
```

<img src="README_files/figure-gfm/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

## ML Estimate Table | Generate tables automatically

``` r
tab <- print(mlest(shock.ld, distribution = 'weibull'))$mle
## xarray(table = tab)
```

\[
\begin{array}{rrrrr}   \hline  & MLE & Std.Err. & 95\% Lower & 95\% Upper \\    \hline mu & 10.23 & 0.11 & 10.01 & 10.45 \\    sigma & 0.32 & 0.07 & 0.20 & 0.50 \\    Weibull (eta) & 27718.72 & 3046.02 & 22347.77 & 34380.49 \\    Weibull (beta) & 3.16 & 0.73 & 2.01 & 4.97 \\     \hline \end{array} 
\]
