library(shiny)
library(plotly)
library(corrplot)
library(grnnet)

vertebrae.db <- readRDS(file = "data/dvb.rds")
vertebrae.female <- vertebrae.db[vertebrae.db$SEX == 'Female', ]
vertebrae.male <- vertebrae.db[vertebrae.db$SEX == 'Male', ]
codenames <- colnames(vertebrae.db[ , -c(1:3)])
outnames <- codenames
