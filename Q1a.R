# PREAMBLE
setwd("C:/Users/oseager/Documents/R/ecn206_hw1")
library('ggplot2')
library('reshape2')
library('scales')

# RECESSION DATA
  
  starts <- as.Date(c('1970-01-01', '1973-11-01', '1980-01-01',
                      '1981-07-01','1990-07-01','2001-03-01',
                      '2007-12-01'))
  
  ends <- as.Date(c('1970-11-01','1975-03-01','1980-07-01',
                    '1982-11-01','1991-03-01','2001-11-01',
                    '2009-06-01'))
  recessions.df <- data.frame('XMIN' = starts, 'XMAX' = ends)

  recessions <- NA
  
  for(i in 1:7){
    g <- (g + 
                      geom_rect(data = NULL, aes(xmin = recessions.df$XMIN[i],
                                                 xmax = recessions.df$XMAX[i],
                                                 ymin = -Inf, ymax = Inf),
                                fill = 'pink', color = NA, alpha = 0.015))
  }
# ESTABLISHING INDIVIDUAL DATAFRAMES

gdp <- read.csv('nominal_gdp_data.csv', header = TRUE)
c <- read.csv('nominal_consumption_data.csv', header = TRUE)
i <- read.csv('nominal_investment_data.csv', header = TRUE)
nx <- read.csv('nominal_nx_data.csv', header = TRUE)

# ESTABLISHING REFERENCE DATAFRAME

q1.df <- data.frame('Date' = as.Date(gdp$DATE))
q1.df['Consumption'] <- c$PCECA/gdp$GDPA
q1.df['Investment'] <- i$GPDIA/gdp$GDPA
q1.df['Net Exports'] <- nx$A019RC1A027NBEA/gdp$GDPA

# ESTABLISHING MELT

q1.melt <- melt(q1.df, id.vars = 'Date', measure.vars = c('Consumption',
                                                          'Investment',
                                                          'Net Exports'))

# PLOT

  ggplot(data = q1.melt, aes(Date, value, color = variable)) +
    geom_line(size = I(1)) +
    scale_y_continuous(breaks = seq(-0.2,0.8,0.1), 
                       limits = c(-0.2,0.8)) +
    xlab('Year') +
    ylab('Proportion of U.S. GDP') +
    scale_x_date(labels = date_format('%Y'), breaks = pretty_breaks(10)) +
    theme(axis.text.x=element_text(colour='black'),
          axis.text.y=element_text(colour='black'),
          legend.title = element_blank()) +
    geom_hline(yintercept=0, linetype='dashed', color = 'black', 
               size = 0.2, 
               alpha = 0.3) 

# 15yr mu DATAFRAME
  
  q1.df_mu <- data.frame('Period' = c("'70-'84","'85-'99","'00-'14"))
  
  q1.df_mu['Consumption'] <- c(mean(q1.df[1:15,2]),
                               mean(q1.df[16:30,2]),
                               mean(q1.df[31:45,2]))
  
  q1.df_mu['Investment'] <- c(mean(q1.df[1:15,3]),
                              mean(q1.df[16:30,3]),
                              mean(q1.df[31:45,3]))
  
  q1.df_mu['Net Exports'] <- c(mean(q1.df[1:15,4]),
                             mean(q1.df[16:30,4]),
                             mean(q1.df[31:45,4]))
  
# GENERAL STATS DATAFRAME
  
  q1.df_gen <- data.frame('Statistic' = c("Mean",
                                          "Standard Deviation",
                                          "Coefficient of Variation"))
  
  q1.df_gen['Consumption'] <- c(mean(q1.df$Consumption),
                                sd(q1.df$Consumption),
                                sd(q1.df$Consumption)/mean(q1.df$Consumption))
  
  q1.df_gen['Investment'] <- c(mean(q1.df$Investment),
                                sd(q1.df$Investment),
                                sd(q1.df$Investment)/mean(q1.df$Investment))
  
  q1.df_gen['Net Exports'] <- c(mean(q1.df$`Net Exports`),
                                sd(q1.df$`Net Exports`),
                                sd(q1.df$`Net Exports`)/mean(q1.df$`Net Exports`))