


library(shiny)
library(tidyverse)
library(ggplot2)
library(shinyjs)

# assign dataset as df
hearts = read.csv(file = "./heart_failure_clinical_records_dataset.csv")