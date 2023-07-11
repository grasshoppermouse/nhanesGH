library(foreign)
library(tidyverse)
library(readxl)

source('data-raw/data_build_G.R')
source('data-raw/data_build_H.R')
source('data-raw/data_build_GH.R')
source('data-raw/design_build_dietary.R')

d_G <- G_data()
d_H <- H_data()
d_GH <- GH_data(d_G, d_H)

designsG <- all_designs(d_G)
designsH <- all_designs(d_H)
designsGH <- all_designs(d_GH)

usethis::use_data(d_G, d_H, d_GH, designsG, designsH, designsGH, overwrite = TRUE)
