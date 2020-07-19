#R Studio API Code
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Data Import
library(tidyverse)
Adata_tbl <- read_delim("../data/Aparticipants.dat", delim = "-", col_names = c("casenum", "parnum", "stimver","datadate", "qs"))
Anotes_tbl <- read_csv("../data/Anotes.csv", col_names = TRUE)
Bdata_tbl <- read_delim("../data/Bparticipants.dat", delim = "\t", col_names = c("casenum", "parnum", "stimver","datadate", paste0("q", 1:10)))
Bnotes_tbl <- read_tsv("../data/Bnotes.txt", col_names = TRUE)

#Data Cleaning
Adata_tbl <- Adata_tbl %>% 
  separate("qs", into = paste0("q", 1:5), sep = " - ") %>%  #separate qs column into q1:q5
  mutate_at(paste0("q", 1:5), as.numeric) %>% #mutate_at(vars(q1:q5), as.numeric) #convert q1:q5 to numeric. We use mutate_at for multiple columns
  mutate(datadate = as.POSIXct(Adata_tbl$datadate, format = "%b %d %Y, %H:%M:%S")) #  mutate(datadate = mdy_hms(Adata_tbl$datadate))
    
Aaggr_tbl <- Adata_tbl %>%  #creates new table, aggregates mean scores by parnum
  group_by(parnum) %>% 
  summarize_at(vars(q1:q5),mean) #finds mean for each q
Baggr_tbl <- Bdata_tbl %>% 
  group_by(parnum) %>% 
  summarize_at(vars(q1:q5),mean)
Aaggr_tbl <- Adata_tbl %>% left_join(Anotes_tbl, by = "parnum") #or Aaggr_tbl <- left_join(Aaggr_tbl, Anotes_tbl, by = "parnum")
Baggr_tbl <- Bdata_tbl %>% left_join(Bnotes_tbl, by = "parnum")
bind_rows(A = Aaggr_tbl, B= Baggr_tbl,.id="dataset") %>% #.id essentially creates a col that differentiates A and B
  group_by(dataset) %>%
  filter(is.na(notes)) %>%
  summarize(n()) #gets the count from each group
