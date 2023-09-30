################
# Git assignment
# Tim Nehls
# 30.9.2023
################

# Load packages
library(tidyverse)
library(here)

# Load text
raw_text <- 
  readLines("http://www.sao.ru/lv/lvgdb/article/suites_dw_Table1.txt") %>% 
  as_tibble()

sep_line <- raw_text %>% 
  pull(value) %>% 
  grep(pattern = "--+")

