# rm(list=ls())
# Comandi per il data import

library(tidyverse)

train <- read_csv("dataset_stima.csv")
test <- read_csv("dataset_previsione.csv")
