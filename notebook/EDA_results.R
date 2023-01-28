# Clear environment
rm(list = ls())
graphics.off()

# Load the libraries
library(tidyverse)

# Set working directory
# setwd("D:/Project/SummerProject/notebook/")
setwd("G:/Other computers/HP/SummerProject/notebook")
# source the custom theme code
source('./custom_theme.R')
# Load the data
data <- read.csv('./../data/final_result.csv')

data %>% glimpse()

data %>% 
  group_by(time, type, vulnerable) %>% 
  summarise(avg_roa = mean(roa)) %>% 
  ggplot(aes(x = type, fill = factor(vulnerable),  y = avg_roa)) +
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(.~factor(time, labels = c('Pre COVID', 'Post COVID'))) +
  labs(x = 'Type', y = 'Average Return of Assets (%)', fill = 'Vulnerable',
      title = 'Average return of assets by bank type and vulnerability') +
  custom_theme()

data %>% 
  group_by(time, type, vulnerable) %>% 
  summarise(avg_roa = mean(leverage)) %>% 
  ggplot(aes(x = type, fill = factor(vulnerable),  y = avg_roa)) +
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(.~factor(time, labels = c('Pre COVID', 'Post COVID'))) +
  labs(x = 'Type', y = 'Average Leverage', fill = 'Vulnerable',
       title = 'Average leverage by bank type and vulnerability') +
  custom_theme()

data1 <- read.table('./../map_data/76694_1_5_20221207_131853_dat.txt', sep = '|', header = T)
data1 %>% head()
