# landmarkeR

Time-to-event analysis using a wide array of longitudinal and survival
sub-models.

<!-- badges: start -->

| Usage                                                                                                                                                | Release                                                                                                                         | Development                                                                                                                                                                                                                     |
|------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)                                                        | [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/landmarkeR)](https://cran.r-project.org/package=landmarkeR)          | [![R build status](https://github.com/VallejosGroup/landmarking/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/VallejosGroup/landmarking/actions/workflows/R-CMD-check.yaml)                    |
| [![License: GPL-3](https://img.shields.io/badge/License-GPL3-green.svg)](https://opensource.org/license/gpl-3-0)                                     |                                                                                                                                 | [![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip) |
|                                                                                                                                                      |                                                                                                                                 | [![codecov](https://codecov.io/gh/VallejosGroup/landmarking/graph/badge.svg?token=YUQ6PINJSO)](https://codecov.io/gh/VallejosGroup/landmarking)                                         |

<!-- badges: end -->

## Introduction

Time-to-event, or survival analysis, is used to analyse the time until an
_event of interest_ occurs. Common events include hospitalisation, equipment
failure, or a prisoner reoffending. Whilst classic survival methods assume model
covariates are static, it is often the case that longitudinal data are
collected. Two main forms of survival analysis in the presence of time-dependent
covariates exist, joint models and landmarking. This package focuses on the
latter. For a set of landmark times, a survival model is fitted up to given
horizon times. At landmark times, any time-dependent covariates must be
summarised. Most commonly, the last observation carried forward (LOCF) approach
is used. However, a more modern approach is to instead fit a linear mixed
effects model which accounts for observations being measured with error. 

Whilst packages already exist which implement landmarking, these packages
implement specific longitudinal and survival models. The aim of `landmarkeR` is
to support a wide array of longitudinal and survival sub-models using a system
which permits others to add their own models. 


## Installation

We are planning to release the package on CRAN once the software is mature. For
now, the package can be installed from this repository using `remotes`. 

``` R
# install.packages("remotes")
remotes::install_github("vallejosgroup/landmarking")
```
