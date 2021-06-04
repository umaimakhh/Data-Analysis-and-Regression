library(vcd)
library(ca)


#Read in Iris Data
setwd("C:/Users/Umaima/Desktop/")
library(xlsx)
library(tidyverse)

library("readxl")
# xls files
my_data <- read_excel("sport.xls")

df <- read.xsl("sport.xsl",sheet=1)
dt <- as.table(as.matrix(my_data))
str(dt)
mosaic(dt, shade=TRUE)

head(dt)
str(dt)

fit = ca(dt)
summary(fit)
plot(fit)
plot(fit,mass=T,contrib = "absolute",map="rowgreen",arrows=c(T,T))



library(ca)

