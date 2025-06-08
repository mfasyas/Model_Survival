setwd("F:\\Coding\\R\\Model_Survival")
# Ganti dengan directory anda

library(survival)
library(survminer)
library(dplyr)
library(Rcpp)

data <- read.csv("Data_Project_Modsur3.csv")
head(data)

View(data)
