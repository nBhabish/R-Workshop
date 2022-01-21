
# Installing packages and loading libraries ----

# * How to run a line of code and clear the console ----

install.packages("tidyverse")
install.packages("readxl")
install.packages("writexl")
install.packages("ggplot2")
install.packages("scales")
install.packages("corrplot")
library(corrplot)
library(tidyverse)
library(ggplot2)
library(readxl)
library(writexl)
library(scales)

# Setting up working directory ----

setwd("~/Desktop/BBR_bootcamp/") 

getwd()

# * Introduction to R Script, Console, and Environment ----

# Importing data from excel

gapminder_tbl <- read_xlsx("gapminder.xlsx")

# * talk about assignment operator ----

# Mathematical Operations ----

# * Addition ----

3+4

# * Subtraction ----

5-3

# * Multiplication ----

3*5

# *  Division ----

3/4

# * Assigning value to a variable a ----

a <- 3+4

a

# * Overwriting a ----

a <- 95/7

a


# Data Types ----

# * numeric ----

class(7)

# * character ----

class("wiseman")

# * factor ----

class(as.factor("wiseman"))

# * logical ----

class(TRUE)


# Manipulating the gapminder_tbl dataset ----

# * view() ----

gapminder_tbl %>% 
  view()

# * pipe operator: Pipe operator allows us to pass the output of a function as a input to the other one in sequence.

# * glimpse() ----

gapminder_tbl %>% 
  glimpse()

# * head() ----

gapminder_tbl %>% 
  head()

# * tail() ----

gapminder_tbl %>% 
  tail()


# * select() ----

gapminder_tbl %>% 
  select(country, continent, year, pop)

# * select() and everything() ----

gapminder_tbl %>% 
  select(continent, everything())

# * filter() ----

gapminder_tbl %>% 
  filter(country == France) # will give an error message

gapminder_france_tbl <- gapminder_tbl %>% 
  filter(country == "France")

# * filter() examples to work on----

# ** Extract observations only from Asia ----



# ** Extract observations from the year 1952 ----



# ** Extract observations that doesn't contain Europe as a continent ----


# * count() ----

gapminder_tbl %>% 
  count(continent, sort = TRUE)


# * mutate() ----

gapminder_tbl %>% 
  mutate(pop_increased_by_10 = pop + 10)

gapminder_tbl %>% 
  mutate(pop_icreased_by_10_times = pop * 10)

# * mutate(). Converting character var. to categorical var. 

gapminder_tbl

gapminder_fct_continent_tbl <- gapminder_tbl %>% 
  mutate(continent = as.factor(continent))

gapminder_fct_continent_tbl

gapminder_fct_continent_tbl %>% 
  mutate(continent = as.character(continent))


# * arrange() ----

gapminder_afg_asc <- gapminder_tbl %>% 
  filter(country == "Afghanistan") %>% 
  arrange(pop)

gapminder_afg_asc

gapminder_afg_desc <- gapminder %>% 
  filter(country == "Afghanistan") %>%
  arrange(desc(pop))

gapminder_afg_desc


# * ntile() ----

gapminder_tbl_bin <- gapminder_tbl %>% 
  mutate(gdpPercap_bin = ntile(gdpPercap, 3))

gapminder_tbl_bin %>% 
  view()

# ntile() takes in your entire column and decides what cut-points to use and bins it accordingly into however many bins you want.

# * ntile() and case_when() ----

gapminder_tbl %>% 
  mutate(
    gdpPercap_bin2 = case_when(
      gdpPercap > quantile(gdpPercap, 0.66) ~ "High",
      gdpPercap > quantile(gdpPercap, 0.33) ~ "Medium",
      TRUE ~ "Low"
    )
  ) 


# * group_by() and summarise() ----

gapminder_tbl %>% 
  filter(year == 1952) %>% 
  group_by(continent) %>% 
  summarise(population = sum(pop)) %>% 
  ungroup() %>% 
  arrange(desc(population))


# Visualization ----

# * non-formatted bar chart

gapminder_tbl %>% 
  filter(year == 1952) %>% 
  group_by(continent) %>% 
  summarise(total_population = sum(pop)) %>% 
  ungroup() %>% 
  mutate(continent = as.factor(continent)) %>%
  # Visualize
  
  ggplot(aes(continent, total_population))+
  geom_col(fill = "#2c3e50", width = 0.5)+
  
  scale_y_continuous(labels = scales::comma)+
  theme_minimal()+
  
  labs(title = "Population of Different Continents in 1952",
       x = "",
       y = "Population",
       subtitle = "",
       caption = "Data Source: Gapminder")


# * formatted bar chart

gapminder_tbl %>% 
  filter(year == 1952) %>% 
  group_by(continent) %>% 
  summarise(total_population = sum(pop)) %>% 
  ungroup() %>% 
  arrange(desc(total_population)) %>% 
  mutate(continent = as_factor(continent)) %>%
  # Visualize
  
  ggplot(aes(continent, total_population))+
  geom_col(fill = "#2c3e50", width = 0.5)+
  
  scale_y_continuous(labels = scales::comma)+
  theme_minimal()+
  
  labs(title = "Population of Different Continents in 1952",
       x = "",
       y = "Population",
       subtitle = "",
       caption = "Data Source: Gapminder")


# Saving our gapminder_france_tbl as an excel file ----

write_xlsx(gapminder_france_tbl, path = "gapminder_france.xlsx")


# Correlation Plot ----

gapminder_tbl %>% 
  select(year:gdpPercap) %>% 
  cor() %>% 
  corrplot(method = "number")

gapminder_tbl %>%
  select(year:gdpPercap) %>%
  cor() %>%
  corrplot(method = "color", order = "alphabet")



