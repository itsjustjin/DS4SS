#R Studio API Code
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Data Import
library(stringi)
citations <- stri_read_lines("../data/citations.txt") # Convert the text file citations.txt into a vector of strings called citations.
citations_txt <- citations[!stri_isempty(citations)]  # Create a new vector called citations_txt containing only non-blank lines from citations
length(citations) - length(citations_txt)             # Using citations and citations_txt, print to console the number of blank lines eliminated.

#Data Cleaning
library(stringr)
library(tidyverse)
library(rebus)
sample(citations_txt, 10)                             # Using the sample() function, display a random draw of 10 citations to the console. You may want to run this command several times to get a sense of what cites look like in this file
citations_tbl <- tibble(line = 1:length(citations_txt), cite = citations_txt) %>%  #Using an appropriate library and function, create a new tibble called citations_tbl containing two columns: line which indicates line number in the source file and cite which contains the citation text you just imported.
  mutate(cite = str_replace_all(cite, UP_QUOTATION_MARK, "")) %>%         # Remove all quotations marks, including double and single, from all citations.
  mutate(year = as.numeric(str_match(cite, OPEN_PAREN %R% capture(repeated(DGT, 4)))[,2])) %>%  # Create a new variable called year that contains the year of publication.
  mutate(page_start = as.numeric(str_match(cite, capture(one_or_more(DGT)) %R% "-" %R% one_or_more(DGT))[,2])) %>%  #Create a new variable called page_start that contains the first page of each citation.
  mutate(perf_ref = str_detect(str_to_lower(cite), "performance")) %>%    #Create a new variable called perf_ref that contains TRUE for any citation in which there is a reference to the word “performance”, regardless of capitalization, and FALSE when there is not.
  mutate(title = str_match(cite, CLOSE_PAREN %R% DOT %R% SPACE %R% capture(one_or_more(negated_char_class(DOT))))[,2]) %>% #Create a new variable called title that contains the citation title. one_or_more(negated_char_class) essentially means go until this. (here it would be DOT)
  mutate(first_author = str_match(cite, START %R%
                                    one_or_more(ALPHA) %R%                #a capital letter
                                    "," %R% 
                                    optional(SPACE) %R%
                                    one_or_more(ALPHA) %R%
                                    DOT %R% optional(SPACE)))             #Create a new variable called first_author that contains the last name and any initials of the first author of each citation.
  