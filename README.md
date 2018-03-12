<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/mpadge/distdecay.svg?branch=master)](https://travis-ci.org/mpadge/distdecay) [![Build status](https://ci.appveyor.com/api/projects/status/github/mpadge/distdecay?svg=true)](https://ci.appveyor.com/project/mpadge/distdecay) [![codecov](https://codecov.io/gh/mpadge/distdecay/branch/master/graph/badge.svg)](https://codecov.io/gh/mpadge/distdecay) [![Project Status: WIP](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#WIP)

An R package to use output from the [`bikedata` package](https://github.com/ropensci/bikedata) to analyse distance decay functions. The package mostly relies on two corresponding matrices of numbers of trips and distances between all pairs of stations. These data are bundled internally as `tripmats` and `distmats`, respectively.

### Installation

``` r
devtools::install_github("mpadge/distdecay")
```
