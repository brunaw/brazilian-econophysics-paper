# --------------------------------------------------
# February, 2021
# Author: Bruna Wundervald
# --------------------------------------------------
library(rio)
library(tidyverse)

data_list <- import_list("data/results_paper.xlsx")

data_list[[1]] %>% glimpse()
data_list[[2]] %>% glimpse()
data_list[[3]] %>% glimpse()
