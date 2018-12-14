# PREAMBLE
setwd("C:/Users/oseager/Documents/QMUL/ECN206CW2")
library('ggplot2')
library('reshape2')
library('scales')

# ESTABLISHING INDIVIDUAL DATAFRAMES

gdp <- read.csv('gdp_data.csv', header = TRUE)
cp <- read.csv('cp_data.csv', header = TRUE)
m <- read.csv('m_data.csv', header = TRUE)
x <- read.csv('x_data.csv', header = TRUE)