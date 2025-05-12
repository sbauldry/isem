### Purpose: Prepare Chapter 1 example data
### Author:  S Bauldry
### Date:    Apr 17, 2025

setwd("~/desktop")
library(tidyverse)
library(psych)


### Load prepared NSDUH data and drop missing cases
d1 <- read_csv("isem-nsduh-data.csv") |>
  na.omit()

### Table 1 - descriptive statistics
describe(d1)
