# PREAMBLE
setwd("C:/Users/oseager/Docugdpents/QMUL/ECN206CW2")
library('ggplot2')
library('reshape2')
library('scales')
library('data.table')

# ESTABLISHING INDIVIDUAL DATAFRAgdpES

gdp <- read.csv('gdp_data.csv', header = TRUE)
cp <- read.csv('cp_data.csv', header = TRUE)
m <- read.csv('m_data.csv', header = TRUE)
x <- read.csv('x_data.csv', header = TRUE)

# CUTTING DATA TO 1960 THROUGH 2018 Q1

gdp <- gdp[49:(nrow(gdp)-1),]
cp <- cp[1:(nrow(cp)-1),]
m <- m[49:(nrow(m)-1),]
x <- x[49:(nrow(x)-1),]

# GETTING TABULAR DATA

t <- rep(0,16)
dim(t) <- c(4,4)

t[1,] <- c(
  sd(gdp[1:96,2]),
  sd(gdp[97:233,2]),
  sd(gdp[1:96,2])/mean(gdp[1:96,2]),
  sd(gdp[97:233,2])/mean(gdp[97:233,2])
)

t[2,] <- c(
  sd(cp[1:96,2]),
  sd(cp[97:233,2]),
  sd(cp[1:96,2])/mean(cp[1:96,2]),
  sd(cp[97:233,2])/mean(cp[97:233,2])
)

t[3,] <- c(
  sd(m[1:96,2]),
  sd(m[97:233,2]),
  sd(m[1:96,2])/mean(m[1:96,2]),
  sd(m[97:233,2])/mean(m[97:233,2])
)

t[4,] <- c(
  sd(x[1:96,2]),
  sd(x[97:233,2]),
  sd(x[1:96,2])/mean(x[1:96,2]),
  sd(x[97:233,2])/mean(x[97:233,2])
)

# GETTING 1984-2007 AND 2008-2018 DATA

t2 <- rep(0,16)
dim(t2) <- c(4,4)

t2[1,] <- c(
  sd(gdp[97:192,2]),
  sd(gdp[193:233,2]),
  sd(gdp[97:192,2])/mean(gdp[97:192,2]),
  sd(gdp[193:233,2])/mean(gdp[193:233,2])
)

t2[2,] <- c(
  sd(cp[97:192,2]),
  sd(cp[193:233,2]),
  sd(cp[97:192,2])/mean(cp[97:192,2]),
  sd(cp[193:233,2])/mean(cp[193:233,2])
)

t2[3,] <- c(
  sd(m[97:192,2]),
  sd(m[193:233,2]),
  sd(m[97:192,2])/mean(m[97:192,2]),
  sd(m[193:233,2])/mean(m[193:233,2])
)

t2[4,] <- c(
  sd(x[97:192,2]),
  sd(x[193:233,2]),
  sd(x[97:192,2])/mean(x[97:192,2]),
  sd(x[193:233,2])/mean(x[193:233,2])
)

# CREATING A RUNNING MACRO-VOL INDEX

mvol <- data.frame('Date' = rep(NA,214), 'MVI' = rep(0,214), 'CA' = rep(0,214))

mvol[,1] <- seq(1964.75,2018,0.25)

gdpcv <- rep(0,214)
cpcv <- rep(0,214)

for(i in 20:233){
  gdpcv[(i-19)] <- sd(gdp[(i-19):i,2])/mean(gdp[(i-19):i,2])
  cpcv[(i-19)] <- sd(cp[(i-19):i,2])/mean(cp[(i-19):i,2])
}

mvol[,2] <- gdpcv + cpcv

# GETTING CA DATA

ca1 <- read.csv('ca1_data.csv', header = TRUE)
ca2 <- read.csv('ca2_data.csv', header = TRUE)

ca_all <- c(ca1[20:156,2]/100,(ca2[,2]/100000))

mvol[,3] <- ca_all

# ESTABLISHING MELT

melt <- melt(mvol, id.vars = 'Date', measure.vars = c('MVI','CA'))

# PLOT

ggplot(data = melt, aes(Date, value, color = variable)) +
  geom_line() + 
  
  scale_y_continuous(breaks = seq(-2,4)) + 
  
  xlab('Year') +
  
  ylab("Current Account ($100bn's), MVI") + 
  
  theme(axis.text=element_text(size=9,colour = 'black'),
        axis.title=element_text(size=10,face="bold"),
        legend.title = element_blank()) + 
  
  scale_x_continuous(breaks = seq(1965,2020,5)) +
  
  geom_hline(yintercept=0, linetype='dashed', color = 'black', 
             size = 0.2, alpha = 0.3) +
  geom_vline(xintercept = 1984, size = 0.2, alpha = 0.3)