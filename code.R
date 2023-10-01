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
findSeparatingLine <- function(text) {
  colname <- colnames(text)
  
  text %>% 
    pull(colname) %>% 
    grep(pattern = "--.+")
}


sep_line <- findSeparatingLine(raw_text)

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
getColumnNames <- function(text) {
  text %>% 
    slice(1) %>% # 1st row is the names
    unlist(use.names = FALSE) %>% 
    str_split_1(pattern = "\\|") %>% # Get single names
    str_trim()
}

column_names <- 
  raw_text %>% 
  getColumnNames()

# Separate into columns
separateColumns <- function(text, column_names) {
  text %>% 
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
}

galaxy_table <- 
  raw_text %>% 
  separateColumns(column_names)


# Convert columns that contain numbers into numeric columns
galaxy_table <-
  galaxy_table %>%
  mutate(across(-c(name, md), as.numeric))


###### TASK 3 ######
galaxy_table %>% 
  ggplot(aes(x = log_lk)) +
  geom_histogram()


###### TASK 4 ######

# Load data ----
galaxy_speeds <-
  readLines("https://www.sao.ru/lv/lvgdb/article/UCNG_Table4.txt") %>% 
  as_tibble()

# Deal with separating line ----
sep_line_speeds <- galaxy_speeds %>% 
  findSeparatingLine()

galaxy_speeds <-
  galaxy_speeds %>% 
  slice(-sep_line_speeds)

# Separate the columns of the table ----

# Get column names
column_names_speeds <-
  galaxy_speeds %>% 
  getColumnNames()

# Separate into columns
galaxy_speeds_table <- 
  galaxy_speeds %>% 
  separateColumns(column_names_speeds)

# Convert columns containing numbers into numeric
galaxy_speeds_table <-
  galaxy_speeds_table %>% 
  mutate(across(c(cz, error), as.numeric))

# Join velocity to galaxy_table ----

# Extract name and cz column
velocity_table <-
  galaxy_speeds_table %>% 
  select(name, cz)

# Join
galaxy_table <-
  galaxy_table %>% 
  left_join(velocity_table, by = join_by(name == name)) %>% 
  relocate(cz, .after = D)

# Plot velocity against distance ----
galaxy_table %>% 
  ggplot(aes(x = D, y = cz)) +
  geom_point() +
  geom_smooth(method = lm)

# Yes, there seems to be a roughly proportional relationship.

# Estimate Hubble's constant as H = v/D ----
hubble_constant <-
  mean(galaxy_table$cz, na.rm = T)/mean(galaxy_table$D, na.rm = T)

# Wikipedia lists the constant as being in between 68 and 74 
# (km/s)/Mpc; our result is fairly close to the upper estimation limit
