################
# Git assignment
# Tim Nehls
# 30.9.2023
################

# Load packages ----
library(tidyverse)
library(magrittr) # Needed for %$%
library(reshape2)

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

# If we want a tidy tibble, we need to remove md column,
# as it would turn the value column into a character column.
# We can store this information in another variable.
galaxy_table %>% 
  select(name, md) -> disturber_info

galaxy_table %>% 
  select(-md) %>%
  melt(id.vars = "name") %>% 
  as_tibble() %>%
  filter(!(variable == "m_b" & is.na(value))) -> tidy_galaxies


###### TASK 3 ######

# The size of the galaxies is reflected in m_b
# Idea: Plot a normal bell curve with mu = mean(m_b), sigma = sd(m_b)

tidy_galaxies %>% 
  filter(variable == "m_b") %>%
  summarise(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T)) -> stats_magnitudes

# Create plot
# As you can see, the plot is skewed to the right.
tidy_galaxies %>%
  filter(variable == "m_b") %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(
    aes(y = ..density..),
    colour = "#e9ecef",
    alpha = 0.6,
    binwidth = 1,
    na.rm = TRUE) +
  stat_function(fun = dnorm,
                args = list(
                  mean = stats_magnitudes$mean, 
                  sd = stats_magnitudes$sd
                ),
                colour = "red",
                geom = "point"
  ) +
  ylab("Relative frequency") +
  xlab("Absolute magnitude of the galaxy") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  ggtitle("Relative frequencies of galaxy magnitudes", 
          "Data taken from 'UPDATED NEARBY GALAXY CATALOG', 
          The Astronomical Journal, 145:101 (22pp), 2013 April")


# We can also calculate skewness and kurtosis.
# The distribution of magnitudes is skewed to the
# right and has fatter tails.
tidy_galaxies %>% 
  filter(variable == "m_b") %$% 
  c(
    skewness = moments::skewness(value),
    kurtosis = moments::kurtosis(value)
  )

# An explanation for this could be that galaxies with bigger
# magnitudes (the light emitted by the galaxy) are easier to
# observe, as the telescopes might not be sensitive enough
# for galaxies with little light emission.



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

# Again, we want to save the character columns into a
# separate tibble, as tidying the table would
# result in a character value column

galaxy_speeds_table %>% 
  select(name, bibcode, refs) -> speed_references

tidy_speeds <- galaxy_speeds_table %>% 
  select(name, cz, error) %>% 
  melt(id.vars = "name") %>% 
  as_tibble()

# Join velocity to galaxy_table ----

# Extract name and cz column
velocity_table <-
  tidy_speeds %>% 
  filter(variable == "cz")

# Join
tidy_galaxies <- 
  tidy_galaxies %>% 
  left_join(velocity_table, by = join_by(name == name))


# Plot velocity against distance ----
tidy_galaxies %>% 
  filter(variable.x == "D") %>% 
  ggplot(aes(x = value.x, y = value.y)) +
  geom_point() +
  geom_smooth(method = lm)

# Yes, there seems to be a roughly proportional relationship.

# Estimate Hubble's constant as H = v/D ----
hubble_constant <-
  mean(galaxy_table$cz, na.rm = T)/mean(galaxy_table$D, na.rm = T)

tidy_galaxies %>% 
  filter(variable.x == "D") %$% 
  c(mean(value.x, na.rm = T), # Mean of distances
    mean(value.y, na.rm = T)) %>% # Mean of speeds
  { .[2]/ .[1] } # Divide speeds by distances

# Wikipedia lists the constant as being in between 68 and 74 
# (km/s)/Mpc; our result is fairly close to the upper estimation limit
