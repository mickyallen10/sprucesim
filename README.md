
# sprucesim

A Stand-Level Forest Growth and Yield Simulator for Norway Spruce

The goal of sprucesim is to to provide a framework for simulating even-aged stands of Norway Spruce under different thinning treatments. Additionally, the simulator can be used for testing new functions.

## Installation

Installation of `sprucesim` from github:

``` r
library(devtools)
devtools::install_github("mickyallen10/sprucesim",build_vignettes = TRUE)
```

Load `sprucesim` and dependent packages:

``` r
invisible( lapply( c("sprucesim", "tidyverse"), library, character.only = T))
```

## Forest Growth and Yield Simulation

This is a basic example which shows you how to run a stand simulation:

``` {r}
library(sprucesim)
library(tidyverse)
sprucesim(stand.df = SpruceStands,
         period.length=5,
         n.periods = 20,
         functions = list(
           fn.basalarea = basal_area,
           fn.domht = dom_height,
           fn.survival = survival,
           fn.qmd = qmd,
           fn.treerem = tree_reduction,
           fn.vol = volume)) %>% 
           wholestandDF() %>% 
           glimpse()
```
See the Vignette for more detailed instruction and examples.
