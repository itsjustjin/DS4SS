#R Studio API Code
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Libraries
library(tidyverse)
library(lubridate)
library(GGally)

#Data Import and Cleaning
week7_tbl <- read_csv("../data/week3.csv") %>% 
  mutate(timeStart = ymd_hms(timeStart),
         condition = factor(condition, levels = c("A", "B", "C"), labels = c("Block A", "Block B", "Control")),
         gender = factor(gender, levels = c("M", "F"), labels = c("Male", "Female"))) %>% 
  filter(q6 == 1) %>% 
  select(-q6)


#Visualization
ggpairs(week7_tbl[, 5:13]) #only want correlations of q1-q5 and q7-q10
ggplot(week7_tbl, aes(x = timeStart, y = q1)) +
  geom_point() +
  scale_x_datetime("Date of Experiment") +
  scale_y_continuous("Q1 Score")
ggplot(week7_tbl, aes(x = q1, y = q2, color = gender)) +
  geom_jitter()
ggplot(week7_tbl, aes(x = q1, y = q2)) +
  geom_jitter() +
  facet_grid(.~ gender) +
  scale_x_continuous("Score on Q1") +
  scale_y_continuous("Score on Q2")
week7_tbl %>% mutate(timediff = difftime(timeEnd, timeStart, units  = c("secs"))) %>% 
  ggplot(aes(x = gender, y = timediff)) +
  geom_boxplot() +
  scale_x_discrete("Gender") +
  scale_y_continuous("Time Elapsed (secs)")
ggplot(week7_tbl, aes(x = q5, y = q7, color = condition)) +
  geom_jitter(width = 0.05) +
  geom_smooth(se = F, method = "lm") +
  scale_x_continuous("Score on Q5") +
  scale_y_continuous("Score on Q7") +
  scale_color_discrete("Experimental Condition") +
  theme(panel.background = element_blank(), 
        legend.position = "bottom",
        legend.background = element_rect("#DFDFDF"))