#Data Import and Cleaning
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

raw_df <- read.csv("../data/week3.csv")
raw_df$timeStart <- as.POSIXct(raw_df$timeStart)
raw_df$timeEnd <- as.POSIXct(raw_df$timeEnd)
clean_df <- raw_df[raw_df$timeStart >= as.POSIXct("2017-07-01"),]                    # clean_df <- subset(raw_df, timeStart >= as.POSIXct("2017-07-01"))
clean_df <- clean_df[clean_df$q6 == "1",]                                            # subset(clean_df, q6 == 1)

#Analysis
clean_df$timeSpent <- difftime(clean_df$timeStart, clean_df$timeEnd, units = "secs") #clean_df$timeSpent <- (clean_df$timeEnd - clean_df$timeStart) * 60
hist(as.numeric(clean_df$timeSpent))                                                 #hist(unclass(clean_df$timeSpent))
frequency_tables_list <- lapply(clean_df[,5:14], table)
lapply(frequency_tables_list, barplot)
sum(clean_df$q1 >= clean_df$q2 && clean_df$q2 != clean_df$q3)
