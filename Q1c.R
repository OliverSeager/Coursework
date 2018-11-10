# PREAMBLE
setwd("C:/Users/oseager/Documents/R/ecn206_hw1")
library('ggplot2')
library('reshape2')
library('scales')

# GET DATA

data <- read.csv('country_gdppc_data.csv', header = TRUE)

# CHANGE COUNTRY NAMES FROM COLUMN DATA TO ROW NAMES

row.names(data) <- data$Country.Name

data <- data[, !names(data) %in% c("Country.Name")]

# CHANGING COLUMN NAMES TO NUMBERS

names(data) <- 1960:2017

# FINDING COUNTRIES FOR WHICH DATA MUST BE IMPUTED

to_impute <- c()

for(country in row.names(data)){
  na <- sum(is.na(data[country,]))
  
  if(na > 0){
    to_impute <- c(to_impute, country)
  }
}

# BUILD IMPUTATION FUNCTION

impute <- function(state, last_data_yr,yr_data_restarts, df){
  
  lg <- df[state,as.character(last_data_yr)]
  fg <- df[state,as.character(yr_data_restarts)]
  
  period <- yr_data_restarts - last_data_yr - 1
  
  R <- (fg/lg)^(1/period)
  
  for(i in (last_data_yr+1):(yr_data_restarts-1)){
    
    datum <- lg * R^(i - last_data_yr)
    
    df[state,as.character(i)] <- datum
  }
  
  return(df)
}

# IMPUTING DATA

data <- impute("Afghanistan",1981,2001,data)
data <- impute("Cambodia",1974,1993,data)
data <- impute("Iraq",1964,1968,data)
data <- impute("Iraq",1990,2004,data)
data <- impute("Iran",1990,1993,data)
data <- impute("Switzerland",1969,1980,data)

# DROPPING ERRONEOUS 1960 DATA

des <- names(data)[2:length(names(data))]

data <- data[,des]

# KEY STATS DATAFRAME

switch <- 0

for(year in names(data)){
  if(switch == 0){
    firstCol <- as.numeric(quantile(data[,year]))
    df <- data.frame(firstCol, row.names = c('Min','Q1',
                                            'Q2','Q3','Max'))
    switch <- 1
  }
  else{
    df <- cbind(df, as.numeric(quantile(data[,year])))
  }
}

names(df) <- names(data)

# CREATING MELT

dfT <- t(df)

melt <- melt(dfT)

names(melt) <- c('Year','Quartile',
                 'Per Capita Income (Current USD)')
# CREATING PLOT

ggplot(data = melt, aes(Year, `Per Capita Income (Current USD)`, 
                        color = Quartile)) + 
  geom_line() + guides(color = guide_legend(reverse=T)) + 
    scale_y_log10(breaks = c(31.5,100,315,1000,3150,
                             10000,31500,100000), 
                  labels = c('$31.50','$100','$315','$1,000','$3,150',
                             '$10,000','$31,500','$100,000')) + 
  scale_x_continuous(breaks = seq(1960,2015,5),
                     labels = as.character(seq(1960,2015,5))) +
  theme(axis.text.x=element_text(colour='black'),
        axis.text.y=element_text(colour='black'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank()) 

# CREATING QUARTILE RATIO PLOT

q3overq1 <- data.frame(date = 1961:2017, ratio = dfT[,'Q3']/dfT[,'Q1'])

melt2 <- melt(q3overq1, id.vars = 'date', measure.vars = 'ratio')

ggplot(data = melt2, aes(date, value)) + geom_line() + 
  scale_x_continuous(breaks = seq(1960,2015,5),
                     labels = as.character(seq(1960,2015,5))) +
  scale_y_continuous(breaks = seq(0,35,5), limits = c(0,35)) + 
  theme(axis.text.x=element_text(colour='black'),
        axis.text.y=element_text(colour='black')) +
  labs(y = 'Q3:Q1',x = 'Year')

# PRODUCING MEAN, SD, CV TABLE

table <- rep(0,18)

dim(table) <- c(3,6)

for(i in 1:6){
  year <- as.character(1955 + i*10)
  table[,i] <- c(mean(data[,year]),
                 sd(data[,year]),                       
                 sd(data[,year])/mean(data[,year]))
}