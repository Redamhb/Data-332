# Load required libraries
library(tidyverse)   
library(reshape2)    
library(ggplot2)     
library(tm)          
library(wordcloud)   
library(RColorBrewer)
library(syuzhet)     
library(tidytext)    
library(textdata)    

setwd("~/Documents/r_patient/data")  

# Load the dataset
file_path <- "Consumer_Complaints.csv"
df <- read_csv(file_path, show_col_types = FALSE)

# DATA CLEANING
df_clean <- df %>%
  select(Product, Issue, `Consumer complaint narrative`, Company) %>%
  filter(!is.na(Product) & !is.na(Issue) & !is.na(`Consumer complaint narrative`))  # Remove missing values
