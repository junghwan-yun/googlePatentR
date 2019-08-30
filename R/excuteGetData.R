## Gathering Patent Data from Google Patent
## https://patents.google.com/
##
## Author : Junghwan Yun
## Contact : junghwan.alfred.yun@gmail.com

source("R/init.R")
source("R/getData.R")

## WSL에서 실행하는 경우 실행
# Sys.setenv(R_INSTALL_STAGED = FALSE)
check_packages(c("tidyverse","pbapply","rvest","XML", "RCurl", "openxlsx"))

## 특허번호 리스트 수집 - USPTO
list_patent_number <- paste0("US", func_get_patent_number(page_url = "http://patft.uspto.gov/netacgi/nph-Parser?Sect1=PTO2&Sect2=HITOFF&u=%2Fnetahtml%2FPTO%2Fsearch-adv.htm&r=0&f=S&l=50&d=PTXT&OS=TTL%2F%22Artificial+Intelligence%22+AND+APT%2F1&RS=%28TTL%2F%22Artificial+Intelligence%22+AND+APT%2F1%29&Query=TTL%2F%22Artificial+Intelligence%22+AND+APT%2F1&PrevList1=Prev.+50+Hits&TD=238&Srch1=%28%28%22Artificial+Intelligence%22.TI.%29+AND+%28A+or+B%3F%29.KD.%29"))
list_patent_number <- list_patent_number[30:length(list_patent_number)]


## 특허 문서 다운로드
list_download_result <- pbmapply(func_download_html,
                                 patent_number = list_patent_number,
                                 output_path = "HTML/")

## 수집된 특허 테이블 확인 
tbl_patent_number <- tibble(patent_number = names(list_download_result),
                            exist = list_download_result)

# Web에서 데이터 수집
func_get_title(patent = "US10288439")

# Local에서 데이터 수집
func_get_title(patent = read_html("HTML/US10288439.html"), DIRECTLY = T)

func_get_abstract(patent = "US10288439")
func_get_inventor(patent = "US10288439")
func_get_assignee(patent = "US10288439")
func_get_issued_date(patent = "US10288439")
func_get_submitted_date(patent = "US10288439")
func_get_cpc(patent = "US10288439")
func_get_description(patent = "US10288439")
func_get_claims(patent = "US10288439")
func_get_citation(patent = "US10288439")
func_get_cited(patent = "US10288439")
func_get_non_patent_citation(patent = "US10288439")
func_get_similar_document(patent = "US10288439")
func_get_legal_events(patent = "US10288439")


# TODO Read From Local Path
output <- func_get_total_patent_information(list_patent_number = "US10288439",
                                            fromLocal = T,
                                            Local_path = "HTML/")

# TODO Read From HTTPS Path
output <- func_get_total_patent_information(list_patent_number = "US10288439")



# TODO Get All Patent From Local 
output <- func_get_total_patent_information(list_patent_number = tbl_patent_number$patent_number,
                                            fromLocal = T,
                                            Local_path = "HTML/")
saveRDS(output, "RDS/patent_parsed.rds")



# TODO Extract Title and export xlsx
output_title <- lapply(output,
                       function(x){
                         
                         tbl_output <- tibble(patent_number = x$patent_number,
                                              title = x$title)
                         return(tbl_output)
                       })

tbl_title <- output_title %>% bind_rows()
openxlsx::write.xlsx(tbl_title, "../tbl_title.xlsx")
