# PREAMBLE
setwd("C:/Users/oseager/Documents/R/ecn206_hw1")
library('ggplot2')
library('reshape2')
library('scales')

# GETTING DATA

data <- read.csv('tfp_data.csv', header = TRUE)

data['Date'] <- seq.Date(as.Date('1950-01-01'),
                         as.Date('2014-01-01'),'years')

names(data) <- c('Date', 'Actual TFP')

# GETTING PRE-GREAT RECESSION TREND GROWTH (GROSS)

R <- (data[58,'Actual TFP']/data[1, 'Actual TFP'])^(1/57)

trend <- c()

for(i in 0:64){
  trend <- c(trend, data[1, 'Actual TFP']*(R^i))
}

data['Pre-recession Trend'] <- trend

# ESTABLISHING MELT

melt <- melt(data, id.vars = 'Date', measure.vars = c('Actual TFP',
                                                      'Pre-recession 
                                                      Trend'))

post_rec_melt <- subset(melt, Date >= as.Date('2002-01-01'))

ggplot(data = melt, aes(Date, value, color = variable)) + 
  geom_line() + 
  scale_x_date(labels = date_format('%Y'), breaks = pretty_breaks(15)) +
  theme(axis.text.x=element_text(colour='black'),
        axis.text.y=element_text(colour='black'),
        legend.title = element_blank()) +
  labs(y = 'TFP',x = 'Year')

ggplot(data = post_rec_melt, aes(Date, value, color = variable)) + 
  geom_line() + 
  scale_x_date(labels = date_format('%Y'), breaks = pretty_breaks(15)) +
  theme(axis.text.x=element_text(colour='black'),
        axis.text.y=element_text(colour='black'),
        legend.title = element_blank()) +
  labs(y = 'TFP',x = 'Year')