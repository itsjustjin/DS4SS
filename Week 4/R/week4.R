#R Studio API Code
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Data Import
library(tidyverse)
week4_df <- read_delim("../data/week4.dat", delim = "-", col_names = c("casenum", "parnum", "stimver","datadate", "qs"))
glimpse(week4_df)
week4_df_qs <- separate(week4_df, "qs", c("q1", "q2", "q3", "q4", "q5")) #into = paste0("q", 1:5, " - ") #splits qs into five variables
week4_df_qs[,5:9] <- sapply(week4_df_qs[ , 5:9], as.numeric) #coerce new variables into numeric class
week4_df_qs[,5:9][week4_df_qs[, 5:9] == 0] <- NA #convert zero values within these varibles into NA




week4_df_qs$datadate <- as.POSIXct(week4_df_qs$datadate, format = "%b %d %Y, %H:%M:%S" )  #convert datadate column into POSIXct
#or load lubridate and run week4_df$datadate <- mdy_hms(week4_df$datadate)

#Data Analysis
q2_over_time_df <- spread(week4_df_qs[, c("parnum", "stimver", "q2")], stimver, q2) #splits parnum into a single row
sum(complete.cases(q2_over_time_df)) / nrow(q2_over_time_df) #proportion of useable cases
