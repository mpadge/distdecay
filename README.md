<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/mpadge/distdecay.svg?branch=master)](https://travis-ci.org/mpadge/distdecay) [![Build status](https://ci.appveyor.com/api/projects/status/github/mpadge/distdecay?svg=true)](https://ci.appveyor.com/project/mpadge/distdecay) [![codecov](https://codecov.io/gh/mpadge/distdecay/branch/master/graph/badge.svg)](https://codecov.io/gh/mpadge/distdecay) [![Project Status: WIP](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#WIP)

An R package for analysis of distance decay functions.

Input: One square matrix of relationships between pairs of locations; one squre matrix of corresponding distances.

Output: Distance decay functions for each location, quantified as either covariance or mutual information coefficients.

The package is primarily intended to analyse \`\`flow'' matrices which quantify flows between locations, quantifed in such terms as rates or densities or movement or exchange.

### Installation

``` r
devtools::install_github("mpadge/distdecay")
```
