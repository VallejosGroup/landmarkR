# landmarkR <img src="man/figures/logo.png" align="right" width="150" alt = "landmarkR package logo"/>

Time-to-event analysis using a wide array of longitudinal and survival
sub-models.

<!-- badges: start -->

| Usage                                                                                                                                                | Release                                                                                                                         | Development                                                                                                                                                                                                                     |
|------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)                                                        | [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/landmarkR)](https://cran.r-project.org/package=landmarkR)            | [![R build status](https://github.com/VallejosGroup/landmarkR/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/VallejosGroup/landmarkR/actions/workflows/R-CMD-check.yaml)                         |
| [![License: GPL-3](https://img.shields.io/badge/License-GPL3-green.svg)](https://opensource.org/license/gpl-3-0)                                     | [![r-universe](https://vallejosgroup.r-universe.dev/badges/landmarkR)](https://vallejosgroup.r-universe.dev/landmarkR)          | [![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)  |
| [![Website](https://img.shields.io/website?url=https%3A%2F%2Fvallejosgroup.github.io%2FlandmarkR%2F)](https://vallejosgroup.github.io/landmarkR/)    |                                                                                                                                 | [![codecov](https://codecov.io/gh/VallejosGroup/landmarkR/graph/badge.svg?token=YUQ6PINJSO)](https://app.codecov.io/gh/VallejosGroup/landmarkR)                                                                                 |

<!-- badges: end -->

## Introduction

Time-to-event, or survival analysis, is used to analyse the time until an
_event of interest_ occurs. Common events include hospitalisation, equipment
failure, or a prisoner reoffending. Whilst classic survival methods assume model
covariates are static, it is often the case that longitudinal data related to
the outcome of interest are collected. Two main forms of survival analysis
incorporating time-dependent covariates exist, joint models and landmarking
[^1]. This package focuses on the latter.

For a set of landmark times, a survival model is fitted up to specified
horizon times. At landmark times, any time-dependent covariates must be
summarised. Most commonly, the last observation carried forward (LOCF) approach
is used. However, a more modern approach is to instead fit a linear mixed
effects model which accounts for observations being measured with error [^2].
However, any method which summarises longitudinal observations can be used. 
Moreover, whilst landmarking methods typically reply on Cox proportional
hazards models, nearly any survival model can also be used.

Whilst packages already exist which implement landmarking, these packages
implement specific longitudinal and survival models. The aim of `landmarkR` is
to support a wide array of longitudinal and survival sub-models whilst providing
a modular system which allows others to incorporate their own models. 

## Installation

We are planning to release the package on CRAN once the software is mature. For
now, you can get a CRAN-like experience by installing from our r-universe 

``` R
install.packages("landmarkR",
                 repos = c("https://vallejosgroup.r-universe.dev",
                           "https://cloud.r-project.org"))
```

Alternatively, the package can be built from source using `remotes`

``` R
# install.packages("remotes")
remotes::install_github("vallejosgroup/landmarkR", build_vignettes = TRUE)
```

## Getting started

We recommend starting with the `landmarkR` vignette, which provides an
overview of the package and how to use it. You can access the vignette in R by
calling

``` R
vignette("landmarkR")
```

Alternatively, you can view the vignette
[online](https://vallejosgroup.github.io/landmarkR/articles/landmarkR.html).

## References

[^1]: Rizopoulos D, Molenberghs G, Lesaffre EMEH. Dynamic predictions with time-dependent covariates in survival analysis using joint modeling and landmarking. Biometrical Journal. 2017;59(6):1261-1276. doi: [10.1002/bimj.201600238](https://doi.org/10.1002/bimj.201600238)
[^2]: Paige E, Barrett J, Stevens D, et al. Landmark models for optimizing the use of repeated measurements of risk factors in electronic health records to predict future disease risk. American Journal of Epidemiology. 2018;187(7):1530-1538. doi: [10.1093/aje/kwy018](https://doi.org/10.1093/aje/kwy018)

