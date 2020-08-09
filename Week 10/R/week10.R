#R Studio API Code
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Libraries
library(tidyverse)





# Data Import and Cleaning
health_tbl <- read_tsv("../data/w4inhome_dvn.tab") %>% 
  select(attractiveness = H4MH8,
         pregnancies = H4TR9,
         gender = BIO_SEX4) %>% 
  mutate(gender = factor(gender, levels = c(1,2), labels = c("Male", "Female")))

# Analysis
model <- lm(pregnancies ~ attractiveness * gender, data = health_tbl) #The specification first*second indicates the cross of first and second. This is the same as first + second + first:second.
plot(model)
summary(model)

#Visualization
ggplot(health_tbl, aes(x = attractiveness, y = pregnancies, color = gender)) +
  geom_smooth(method = "lm") +
  geom_point()
