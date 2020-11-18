

library(shiny)
library(tidyverse)
library(ggplot2)
library(shinyjs)

# assign dataset as df
hearts_full = read.csv(file = "./heart_failure_clinical_records_dataset.csv")


# select cols for df
hearts = hearts_full %>% 
  
  # select(., age, anaemia, diabetes, high_blood_pressure, smoking, serum_creatinine, sex, time, DEATH_EVENT) %>% 

  # change 0 and 1 values for values that can be interpreted if the dataframe is called to be read
  mutate(., sex = gsub(pattern = 0, replacement = "Female", x = sex, ignore.case = F)) %>% 
  mutate(., sex = gsub(pattern = 1, replacement = "Male", x = sex, ignore.case = F))
  # mutate(., anaemia = gsub(pattern = 0, replacement = "No", x = anaemia, ignore.case = F)) %>% 
  # mutate(., anaemia = gsub(pattern = 1, replacement = "Yes", x = anaemia, ignore.case = F)) %>% 
  # mutate(., diabetes = gsub(pattern = 0, replacement = "No", x = diabetes, ignore.case = F)) %>% 
  # mutate(., diabetes = gsub(pattern = 1, replacement = "Yes", x = diabetes, ignore.case = F)) %>% 
  # mutate(., high_blood_pressure = gsub(pattern = 0, replacement = "No", x = high_blood_pressure, ignore.case = F)) %>% 
  # mutate(., high_blood_pressure = gsub(pattern = 1, replacement = "Yes", x = high_blood_pressure, ignore.case = F)) %>% 
  # mutate(., smoking = gsub(pattern = 0, replacement = "No", x = smoking, ignore.case = F)) %>% 
  # mutate(., smoking = gsub(pattern = 1, replacement = "Yes", x = smoking, ignore.case = F))
  
head(hearts)


hearts_2 = hearts_full %>% 
  select(., 
         Age = age,
         `Creatinine Phosphokinase` = creatinine_phosphokinase,
         Platelets = platelets,
         
         )

head(hearts_2)













