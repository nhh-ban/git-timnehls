################
# Git assignment
# Tim Nehls
# 30.9.2023
################

# Load packages ----
library(tidyverse)
library(here)

###### TASK 2 ######

# Load text ----
raw_text <- 
  readLines("http://www.sao.ru/lv/lvgdb/article/suites_dw_Table1.txt") %>% 
  as_tibble()

# Deal with separating line ----
sep_line <-
  raw_text %>% 
  pull(value) %>% 
  grep(pattern = "--.+")

raw_text <-
  raw_text %>% 
  slice(-sep_line)

# Deal with variable explanation ----
explanations <-
  raw_text %>%
  slice(1:(sep_line-3)) %>% # Take the variables before the separating line
  pull(value) %>% 
  str_split_fixed(pattern = " - ", n = Inf) %>% 
  gsub(pattern = ";",
       replacement = "") %>% # Remove the ; at the end of the explanation
  `colnames<-`(c("Variable","Explanation")) %>% # Give good names
  as_tibble() %>% 
  mutate(across(.fns = str_trim)) # Remove whitespace

# Delete everything before the beginning of the table ----
raw_text <-
  raw_text %>% 
  slice(-1:-(sep_line-2))

# Separate the columns of the table ----

# Get column names
column_names <-
  raw_text %>% 
  slice(1) %>% # 1st row is the names
  unlist(use.names = FALSE) %>% 
  str_split_1(pattern = "\\|") %>% # Get single names
  str_trim()

# Separate into columns
galaxy_table <-
  raw_text %>% 
  separate_wider_delim(
    cols = value, 
    delim = "|", 
    names = column_names
  ) %>% # Split single columns at the delimiter
  slice(-1) %>% 
  mutate(
    across(
      .fns = ~. %>% str_trim() %>% na_if("")
    ) # First remove whitespaces, then fill empty elements with NAs
  ) 

# Convert columns that contain numbers into numeric columns
galaxy_table <-
  galaxy_table %>%
  mutate(across(-c(name, md), as.numeric))


###### TASK 3 ######
galaxy_table %>% 
  ggplot(aes(x = log_lk)) +
  geom_histogram()
