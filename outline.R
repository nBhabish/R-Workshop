#' Introduction to install.packages() and library()
#' What exactly is working directory? Why do we need it?
#' setwd() and getwd()
#' What is R script, console, and environment
#' Reading in data from working directory
#' Mathematical Operations
#' Assignment Operator "<-"
#' Data Types: factor, character, numeric
#' Explain working directory| setwd() and getwd()
#' Show alternative to setwd() | Using ("~/")
#' Reading in/ importing excel file
#' Viewing the dataset
#' Using glimpse() for quick look | Why do we use glimpse()
#' Understanding pipe operator
#' CMD + Shift + M for pipe operator
#' Using head() 
#' Using select() and why select() | Reordering columns
#' Using filter()-filtering by rows
#' Using count()
#' Using mutate() to create a new column/ modify the same column
#' Using as.factor and as_factor
#' Converting from character to categorical variable and vice-versa
#' Arranging by ascending and descending order
#' Creating categories or binning
#' Shortcuts like 1) CMD/ CTRL +  click to view the dataset 2) Control + L to clear Console 3) rm(list =ls()) to clear environment
#' Show how to use brush manually to clear console and environment 
#' Correlation Plot 
#' How to use write_xlsx or write_csv
#' how to rename columns
#' Help


# How to run a line of code? ----------------------------------------------

# Cmd/Ctrl + return/enter

# Show how to run code manually, how to use brush

# Installing packages and loading libraries -------------------------------

install.packages("tidyverse")
install.packages("readxl")
install.packages("writexl")
install.packages("ggplot2")
install.packages("gapminder")
install.packages("scales")
library(tidyverse)
library(ggplot2)
library(readxl)
library(writexl)
library(gapminder)
library(scales)


# Reading in the data

setwd("/Users/bhabishyaneupane/Desktop/BBR_bootcamp")
getwd()

gapminder <- read_xlsx(path = "gapminder.xlsx")

# Mathematical Calculations -----------------------------------------------

#Additiion
3 + 4

# Subtraction
3-4

# Multiplication
3 * 4

# Division
3/4

# Raised to the power
3 ^ 4

a <- 0.5  #  "<-"  is called assignment operator. Assigns a name to a value

a

# Overwrite a 

a <- 0.5 + 0.7 + 0.8 + 0.9

a


# Data Types --------------------------------------------------------------


# numeric
class(7)

class(7.2)

# character
class("abcd")

# factor
class(as.factor("High"))

# Working on a dataset ----------------------------------------------------

setwd("/Users/bhabishyaneupane/Desktop/BBR_bootcamp")

gapminder <- read_xlsx(path = "~/Desktop/BBR_bootcamp/gapminder.xlsx")

gapminder # Use (Cmd/ Ctrl) + click on a data frame object to view it 

# Using glimpse() for quick look | Why do we use glimpse() ----------------

# Understanding pipe operator (%>%)  ---------------------------------------------
gapminder %>% 
    view()

# CMD + Shift + M for pipe operator ---------------------------------------

gapminder %>% 
    glimpse()

glimpse(gapminder)

# Using head()  -----------------------------------------------------------

gapminder %>% 
    head()

# Using select() and why select() -----------------------------------------

gapminder %>% 
    select(country, year, lifeExp)

# Renaming columns --------------------------------------------------------

gapminder %>% 
    rename(life_expectancy = lifeExp, population = pop)

# Reordering columns ------------------------------------------------------


# Let's say you want a couple of columns to appear before other columns

gapminder %>% 
    select(continent, year, everything())

gapminder %>% 
    select(starts_with("c"), everything())


# Using filter()-filtering by rows ------------------------------------------

gapminder %>% 
    filter(year == 1952)

gapminder %>%
    filter(continent == "Asia")

gapminder %>%
    filter(continent != "Asia")

gapminder_turkey_tbl <-gapminder %>% 
    filter(country == "France")



# Using count() -----------------------------------------------------------


# Let's say you want to find out how many times a particular continent has appeared in the dataset



gapminder %>% 
    count(continent, sort = TRUE) 



# Using mutate() to create a new column/ modify the same column -----------


# Let's say you want to convert character variable into a categorical variable

gapminder %>% 
    mutate(continent = as.factor(continent)) 

# gapminder %>% 
#     mutate(continent = as_factor(continent))



# Arranging by ascending and descending order ------------------------------

gapminder %>% 
    filter(country == "Afghanistan") %>% 
    arrange(pop)

gapminder %>% 
    filter(country == "Afghanistan") %>%
    arrange(desc(pop))



# Creating categories or binning -----------------------------------------------------

gapminder %>% 
    mutate(gdpPercap_bin = ntile(gdpPercap, 3)) %>% 
    view()

# Here ntile is breaking the groups into 33rd and 66th percentile
# if we do ntile(col_name, 4), it will the break the groups into



gapminder %>% 
    mutate(gdpPercap_bin = ntile(gdpPercap, 3)) %>% 
    mutate(
        gdpPercap_bin2 = case_when(
            gdpPercap_bin > quantile(gdpPercap_bin, 0.66) ~ "High",
            gdpPercap_bin > quantile(gdpPercap_bin, 0.33) ~ "Medium",
            TRUE ~ "Low"
        )
    ) %>% 
    view()


#' ntile takes in your entire column and decides what cut-points to use 
#' and bins it accordingly into however many bins you want

#' Shortcuts like:
#' 1) CMD/ CTRL +  click to view the dataset 
#' 2) Control + L to clear Console
#' 3) rm(list =ls()) to clear environment


# Show how to use brush manually to clear console and environment 

# Group by and summarise --------------------------------------------------


gapminder %>% 
   filter(year == 2007) %>% 
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

    labs(title = "Total Population of Five Continents in Year 2007",
         subtitle = "",
         caption = "Data Source: Gapminder")



gapminder %>% 
    filter(year == 2007) %>% 
    group_by(continent) %>% 
    summarise(total_population = sum(pop)) %>% 
    ungroup() %>% 
    mutate(continent = as.factor(continent)) %>%
    # Visualize
    
    ggplot(aes(continent, total_population))+
    geom_col(fill = "#2c3e50", width = 0.5)+
    
    scale_y_continuous(labels = scales::comma)+
    theme_minimal()+
    
    labs(title = "Total Population of Five Continents in Year 2007",
         subtitle = "",
         caption = "Data Source: Gapminder")






# Correlation Plot  -------------------------------------------------------

install.packages("corrplot")
library(corrplot)

gapminder %>% 
    select(year:gdpPercap) %>% 
    cor() %>% 
    corrplot(method = "number")
    

gapminder %>% 
    select(year:gdpPercap) %>% 
    cor() %>% 
    corrplot(method = "color", order = "alphabet")


# write_xlsx or write_csv -----------------------------------------------------------------

write_xlsx(gapminder_nepal_tbl, path = "~/Desktop/BBR_bootcamp/gapminder_nepal.xlsx")
write_csv()









