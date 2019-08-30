## Gathering Patent Data from Google Patent
## https://patents.google.com/
##
## Author : Junghwan Yun
## Contact : junghwan.alfred.yun@gmail.com

source("R/init.R")
source("R/getData.R")
check_packages(c("tidyverse","pbapply", "openxlsx"))

# func_get_total_patent_information(list_patent_number = tbl_patent_number$patent_number,
#                                   fromLocal = T,
#                                   Local_path = "HTML/")
list_patent_whole_info <- readRDS("RDS/patent_parsed.rds")


list_patent_whole_info$US10288439$similar_document


# Similar Document 
tbl_patent_similar_document <- pblapply(list_patent_whole_info, 
                                        function(patent){
                                          output <- patent$similar_document %>% 
                                            add_column(patent_number = patent$patent_number, .before = T)
                                        }) %>% bind_rows()

# Text Part of Patent
tbl_patent_doc <- pblapply(list_patent_whole_info, 
                             function(patent){
                               output <- tibble(patent_number = patent$patent_number,
                                                title = patent$title,
                                                abstract = patent$abstract)
                             }) %>% bind_rows()
