# sweeping out 
rm(list = ls())
graphics.off()

# load the library
library(tidyverse)
library(readxl)

# set the working directory
setwd('D:/RKMVERI/S3/Term Project/')

# load the data
data <- read.csv('./data/bank_details2.csv')

# source the custom theme code
source('./notebook/custom_theme.R')

# head of the data
data %>% head()

# Plot1: Column chart of number of firms under bank taken loan
data %>% 
  ggplot(aes(x = Name.of.the.Bank, y = n_firms)) +
  geom_hline(yintercept = 50, color = 'gray') +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25))

plot1 <- data %>% 
  ggplot(aes(x = reorder(Name.of.the.Bank, -n_firms), y = n_firms, fill = Type)) +
  geom_hline(yintercept = 50, color = 'gray') +
  geom_bar(stat = 'identity') + custom_theme() +
  labs(x = '', y = 'Number of firms taken loan',
       title = 'Number of Company Undertaken by Bank') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25))

ggsave(plot = plot1, filename = './img/plot/n_firms_under_bank.png', device = 'png', dpi = 150,
       height = 1080, width = 1920, units = 'px')

# Plot2: Vulnerable bank (based on median value of MES)
data2 <- read_xlsx('./data/task1_output.xlsx', sheet = 'public_b_result')
