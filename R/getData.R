## Gathering Patent Data from Google Patent
## https://patents.google.com/
##
## Author : Junghwan Yun
## Contact : junghwan.alfred.yun@gmail.com

##--


#' WORK : Extract Patent Number from USPTO Page
#' 
#' @param page_url USPTO site advanced search result url 
#' @return vector type patent numbers
func_get_patent_number <- function(page_url){
  
  cat(sprintf("\n || Start Get Patent Number From USPTO || \n\n"))
  
  total_patent_number <- page_url %>%
    RCurl::getURL() %>%
    XML::htmlParse(asText = T) %>%
    XML::xpathSApply("/html/body/i/strong[3]", XML::xmlValue) %>% as.numeric()
  
  if(total_patent_number %% 50 == 0){
    total_page_length <- total_patent_number%/%50
  } else{
    total_page_length <- (total_patent_number%/%50) + 1
  }
    
  list_page_url <- paste0(page_url, paste0("&p=",c(1:total_page_length)))
  
  list_patent_number <- pblapply(list_page_url,
                               function(x){
                                 patent_number <- read_html(x, encoding = "UTF-8") %>% 
                                   html_nodes("form[name='refine']+table td:nth-child(3n+2) a") %>%
                                   html_text()
                                 return(patent_number) 
  }) %>%
    unlist() %>%
    str_remove_all(",")
  
  cat(sprintf("\n || COMPLETE || \n\n"))
  
  return(list_patent_number)
}

## -- 


#' WORK : Download HTML from Google Patent
#' 
#' @param patent_number Individual Character Patent Number. National Code + Number - "US9687171" 
#' @param output_path Local Path 
#' @return Result True / False
func_download_html <- function(patent_number, output_path){
  exceptionHandling = tryCatch({
    
    suppressWarnings(download.file(file.path("https://patents.google.com/patent", patent_number),
                                   file.path(output_path, paste0(patent_number, ".html")), quiet = T))
    
    return(TRUE)
  },
  error = function(e) {
    
    return(FALSE)
  })
}

## -- 


#' WORK : Get Title from input Patent
#' 
#' @param patent Single Patent Number or Patent Html Object 
#' @param DIRECTLY Default = TRUE / If use Universal Function, Insert FALSE   
#' @return Character type Title  
func_get_title <- function(patent, DIRECTLY = TRUE){
  
  if(DIRECTLY == TRUE){
    patent_html <- read_html(file.path("https://patents.google.com/patent", patent),
                             encoding = "UTF-8")
  } else {
    patent_html <- patent
  }
  
  title <- patent_html %>%
    html_nodes("meta[name='DC.title']") %>%
    html_attr("content") %>% 
    str_remove_all("\n") %>% 
    str_remove_all("  ") 
  
  return(title)
}

## -- 


#' WORK : Get Abstract from input Patent
#' 
#' @param patent Single Patent Number or Patent Html Object 
#' @param DIRECTLY Default = TRUE / If use Universal Function, Insert FALSE   
#' @return Character type abstract  
func_get_abstract <- function(patent, DIRECTLY = TRUE){
  
  if(DIRECTLY == TRUE){
    patent_html <- read_html(file.path("https://patents.google.com/patent", patent),
                             encoding = "UTF-8")
  } else {
    patent_html <- patent
  }
  
  abstract <- patent_html %>%
    html_nodes("meta[name='DC.description']") %>%
    html_attr("content") %>% 
    str_remove_all("\n") %>% 
    str_remove_all("  ") 
  
  return(abstract)
}



#' WORK : Get Inventor from input Patent
#' 
#' @param patent Single Patent Number or Patent Html Object 
#' @param DIRECTLY Default = TRUE / If use Universal Function, Insert FALSE   
#' @return Character type inventor
func_get_inventor <- function(patent, DIRECTLY = TRUE){
  
  if(DIRECTLY == TRUE){
    patent_html <- read_html(file.path("https://patents.google.com/patent", patent),
                             encoding = "UTF-8")
  } else {
    patent_html <- patent
  }
  
  inventor <- patent_html %>%
    html_nodes("meta[scheme='inventor']") %>%
    html_attr("content") %>% 
    str_remove_all("\n") %>% 
    str_remove_all("  ") 
  
  return(inventor)
}



#' WORK : Get Assignee from input Patent
#' 
#' @param patent Single Patent Number or Patent Html Object 
#' @param DIRECTLY Default = TRUE / If use Universal Function, Insert FALSE   
#' @return Character type assignee
func_get_assignee <- function(patent, DIRECTLY = TRUE){
  
  if(DIRECTLY == TRUE){
    patent_html <- read_html(file.path("https://patents.google.com/patent", patent),
                             encoding = "UTF-8")
  } else {
    patent_html <- patent
  }
  
  assignee <- patent_html %>%
    html_nodes("meta[scheme='assignee']") %>%
    html_attr("content") %>% 
    str_remove_all("\n") %>% 
    str_remove_all("  ") 
  
  return(assignee)
}




#' WORK : Get Issued_date from input Patent 
#' 
#' @param patent Single Patent Number or Patent Html Object 
#' @param DIRECTLY Default = TRUE / If use Universal Function, Insert FALSE   
#' @return Date type issued_date
func_get_issued_date <- function(patent, DIRECTLY = TRUE){
  
  if(DIRECTLY == TRUE){
    patent_html <- read_html(file.path("https://patents.google.com/patent", patent),
                             encoding = "UTF-8")
  } else {
    patent_html <- patent
  }
  
  issued_date <- patent_html %>%
    html_nodes("meta[scheme='issue']") %>%
    html_attr("content") %>% 
    str_remove_all("\n") %>% 
    str_remove_all("  ") 
  
  return(issued_date)
}




#' WORK : Get Submitted_date from input Patent
#' 
#' @param patent Single Patent Number or Patent Html Object 
#' @param DIRECTLY Default = TRUE / If use Universal Function, Insert FALSE   
#' @return Date type submitted_date
func_get_submitted_date <- function(patent, DIRECTLY = TRUE){
  
  if(DIRECTLY == TRUE){
    patent_html <- read_html(file.path("https://patents.google.com/patent", patent),
                             encoding = "UTF-8")
  } else {
    patent_html <- patent
  }
  
  submitted_date <- patent_html %>%
    html_nodes("meta[scheme='dateSubmitted']") %>%
    html_attr("content") %>% 
    str_remove_all("\n") %>% 
    str_remove_all("  ") 
  
  return(submitted_date)
}



#' WORK : Get CPC Classification from input Patent
#' 
#' @param patent Single Patent Number or Patent Html Object 
#' @param DIRECTLY Default = TRUE / If use Universal Function, Insert FALSE   
#' @return Character type CPC
func_get_cpc <- function(patent, DIRECTLY = TRUE){
  
  if(DIRECTLY == TRUE){
    patent_html <- read_html(file.path("https://patents.google.com/patent", patent),
                             encoding = "UTF-8")
  } else {
    patent_html <- patent
  }
  
  cpc <- patent_html %>%
    html_nodes("li[itemprop='cpcs']:last-child span[itemprop='Code']") %>%
    html_text()
  
  return(cpc)
}



#' WORK : Get Decription from input Patent
#' 
#' @param patent Single Patent Number or Patent Html Object 
#' @param DIRECTLY Default = TRUE / If use Universal Function, Insert FALSE   
#' @return Character type Decription
func_get_description <- function(patent, DIRECTLY = TRUE){
  
  if(DIRECTLY == TRUE){
    patent_html <- read_html(file.path("https://patents.google.com/patent", patent),
                             encoding = "UTF-8")
  } else {
    patent_html <- patent
  }
  
  description <- patent_html %>%
    html_nodes("section[itemprop='description']") %>%
    html_text() 
  
  return(description)
}



#' WORK : Get claims from input Patent
#' 
#' @param patent Single Patent Number or Patent Html Object 
#' @param DIRECTLY Default = TRUE / If use Universal Function, Insert FALSE   
#' @return Character type Claims
func_get_claims <- function(patent, DIRECTLY = TRUE){
  
  if(DIRECTLY == TRUE){
    patent_html <- read_html(file.path("https://patents.google.com/patent", patent),
                             encoding = "UTF-8")
  } else {
    patent_html <- patent
  }
  
  claims <- patent_html %>%
    html_nodes("section[itemprop='claims']") %>%
    html_text() 
  
  return(claims)
}




#' WORK : Get Citation from input Patent
#' 
#' @param patent Single Patent Number or Patent Html Object 
#' @param DIRECTLY Default = TRUE / If use Universal Function, Insert FALSE   
#' @return Character type citation patent number
func_get_citation <- function(patent, DIRECTLY = TRUE){
  
  if(DIRECTLY == TRUE){
    patent_html <- read_html(file.path("https://patents.google.com/patent", patent),
                             encoding = "UTF-8")
  } else {
    patent_html <- patent
  }
  
  citation <- patent_html %>%
    html_nodes("tr[itemprop='backwardReferences']
                            span[itemprop='publicationNumber']") %>%
    html_text() 
  
  if(length(citation) == 0 ){
    citation <- "NULL"
  }
  
  return(citation)
}


#' WORK : Get Cited Patent Number from input Patent
#' 
#' @param patent Single Patent Number or Patent Html Object 
#' @param DIRECTLY Default = TRUE / If use Universal Function, Insert FALSE   
#' @return Character type citation patent number
func_get_cited <- function(patent, DIRECTLY = TRUE){
  
  if(DIRECTLY == TRUE){
    patent_html <- read_html(file.path("https://patents.google.com/patent", patent),
                             encoding = "UTF-8")
  } else {
    patent_html <- patent
  }
  
  citation <- patent_html %>%
    html_nodes("tr[itemprop='forwardReferences']
                            span[itemprop='publicationNumber']") %>%
    html_text() 
  
  if(length(citation) == 0 ){
    citation <- "NULL"
  }
  
  return(citation)
}


#' WORK : Get Non Patent Citation information from input Patent
#' 
#' @param patent Single Patent Number or Patent Html Object 
#' @param DIRECTLY Default = TRUE / If use Universal Function, Insert FALSE   
#' @return Character type information
func_get_non_patent_citation <- function(patent, DIRECTLY = TRUE){
  
  if(DIRECTLY == TRUE){
    patent_html <- read_html(file.path("https://patents.google.com/patent", patent),
                             encoding = "UTF-8")
  } else {
    patent_html <- patent
  }
  
  citation <- patent_html %>%
    html_nodes("tr[itemprop='detailedNonPatentLiterature']
                          span[itemprop='title']") %>%
    html_text() 
  
  if(length(citation) == 0 ){
    citation <- "NULL"
  }
  
  return(citation)
}


#' WORK : Get similar document from input Patent
#' 
#' @param patent Single Patent Number or Patent Html Object 
#' @param DIRECTLY Default = TRUE / If use Universal Function, Insert FALSE   
#' @return Tibble type information
func_get_similar_document <- function(patent, DIRECTLY = TRUE){
  
  if(DIRECTLY == TRUE){
    patent_html <- read_html(file.path("https://patents.google.com/patent", patent),
                             encoding = "UTF-8")
  } else {
    patent_html <- patent
  }
  
  if (patent_html %>% 
      html_nodes("tr[itemprop='similarDocuments']
                           td[itemprop='title']") %>%
      html_text() %>% length() == 0 ){
    
    document = "NULL"
  } else {
    document <- tibble(title = patent_html %>% 
                         html_nodes("tr[itemprop='similarDocuments']
                           td[itemprop='title']") %>%
                         html_text(),
                       link = patent_html %>%
                         html_nodes("tr[itemprop='similarDocuments'] a") %>% 
                         html_attr("href")) %>% 
      mutate(title = str_trim(title)) %>% 
      mutate(type = str_extract(link, "/[a-z]*/") %>% str_remove_all("/")) %>% 
      mutate(document_number = str_remove_all(link, str_c("/",type)) %>%
               str_extract("/[a-zA-Z0-9]*") %>% 
               str_remove_all("/")) %>% 
      select(-link)
  }
  
  return(document)
}




#' WORK : Get legal events from input Patent
#' 
#' @param patent Single Patent Number or Patent Html Object 
#' @param DIRECTLY Default = TRUE / If use Universal Function, Insert FALSE   
#' @return Tibble type information
func_get_legal_events <- function(patent, DIRECTLY = TRUE){
  
  if(DIRECTLY == TRUE){
    patent_html <- read_html(file.path("https://patents.google.com/patent", patent),
                             encoding = "UTF-8")
  } else {
    patent_html <- patent
  }
  
  if (patent_html %>% html_nodes("tr[itemprop='legalEvents']") %>% length() == 0 ){
    
    document = "NULL"
  } else {
    nodeset <- patent_html %>% 
      html_nodes("tr[itemprop='legalEvents']")
    
    document <- lapply(nodeset,
                       function(x){
                         
                         tibble(date = x %>% 
                                  html_nodes("time[itemprop='date']") %>%
                                  html_text(),
                                code = x %>% 
                                  html_nodes("td[itemprop='code']") %>%
                                  html_text(),
                                title = x %>% 
                                  html_nodes("td[itemprop='title']") %>%
                                  html_text(), 
                                description = x %>% 
                                  html_nodes("p[itemprop='attributes']") %>%
                                  html_text() %>% 
                                  str_trim() %>% 
                                  str_remove_all("\n              ") %>% 
                                  str_c(collapse = "    ")
                         )
                         
                       }) %>% bind_rows()
    
  }
  
  return(document)
}




#' WORK : Get Total Patent Information
#' 
#' @param list_patent_number List Patent Number
#' @param fromLocal if get HTML from local, set TRUE / Default = FALSE
#' @param Local_path Local Html Path / Use with fromLocal
#' @return list type Total Information of Single Patent
func_get_total_patent_information <- function(list_patent_number, fromLocal = FALSE, Local_path = NULL){
  
  if (fromLocal == F){
    patent_output <- pblapply(list_patent_number,
                              function(patent_number){
                                patent_html <- read_html(file.path("https://patents.google.com/patent", patent_number), encoding = "UTF-8")
                                
                                patent_title <- func_get_title(patent_html)
                                patent_abstract <- func_get_abstract(patent_html)
                                patent_issued_date <- func_get_issued_date(patent_html)
                                patent_submitted_date <- func_get_submitted_date(patent_html)
                                patent_inventor <- func_get_inventor(patent_html)
                                patent_assignee <- func_get_assignee(patent_html)  
                                patent_description <- func_get_description(patent_html)
                                patent_claim <- func_get_claims(patent_html)
                                patent_cpc <- func_get_cpc(patent_html)
                                patent_citation <- func_get_citation(patent_html)
                                patent_cited <- func_get_cited(patent_html)
                                patent_non_patent_citation <- func_get_non_patent_citation(patent_html)
                                patent_similar_document <- func_get_similar_document(patent_html)
                                patent_legal_events <- func_get_legal_events(patent_html)
                                
                                list_output <- list(patent_number = patent_number,
                                                    title = patent_title,
                                                    abstract = patent_abstract,
                                                    issued_date = patent_issued_date,
                                                    submitted_date = patent_submitted_date,
                                                    inventor = patent_inventor,
                                                    assignee = patent_assignee, 
                                                    description = patent_description,
                                                    claim = patent_claim, 
                                                    cpc = patent_cpc, 
                                                    citation = patent_citation,
                                                    cited = patent_cited,
                                                    non_patent_citation = patent_non_patent_citation,
                                                    similar_document = patent_similar_document,
                                                    legal_events = patent_legal_events )
                                
                                return(list_output)
                              })
    } else {
      patent_output <- pblapply(list_patent_number,
                                function(patent_number){
                                  patent_html <- read_html(paste0(file.path(Local_path, patent_number), ".html"), encoding = "UTF-8")
                                  
                                  patent_title <- func_get_title(patent_html, DIRECTLY = TRUE)
                                  patent_abstract <- func_get_abstract(patent_html, DIRECTLY = TRUE)
                                  patent_issued_date <- func_get_issued_date(patent_html, DIRECTLY = TRUE)
                                  patent_submitted_date <- func_get_submitted_date(patent_html, DIRECTLY = TRUE)
                                  patent_inventor <- func_get_inventor(patent_html, DIRECTLY = TRUE)
                                  patent_assignee <- func_get_assignee(patent_html, DIRECTLY = TRUE)  
                                  patent_description <- func_get_description(patent_html, DIRECTLY = TRUE)
                                  patent_claim <- func_get_claims(patent_html, DIRECTLY = TRUE)
                                  patent_cpc <- func_get_cpc(patent_html, DIRECTLY = TRUE)
                                  patent_citation <- func_get_citation(patent_html, DIRECTLY = TRUE)
                                  patent_cited <- func_get_cited(patent_html, DIRECTLY = TRUE)
                                  patent_non_patent_citation <- func_get_non_patent_citation(patent_html, DIRECTLY = TRUE)
                                  patent_similar_document <- func_get_similar_document(patent_html, DIRECTLY = TRUE)
                                  patent_legal_events <- func_get_legal_events(patent_html, DIRECTLY = TRUE)
                                  
                                  list_output <- list(patent_number = patent_number,
                                                      title = patent_title,
                                                      abstract = patent_abstract,
                                                      issued_date = patent_issued_date,
                                                      submitted_date = patent_submitted_date,
                                                      inventor = patent_inventor,
                                                      assignee = patent_assignee, 
                                                      description = patent_description,
                                                      claim = patent_claim, 
                                                      cpc = patent_cpc, 
                                                      citation = patent_citation,
                                                      cited = patent_cited,
                                                      non_patent_citation = patent_non_patent_citation,
                                                      similar_document = patent_similar_document,
                                                      legal_events = patent_legal_events )
                                  
                                  return(list_output)
                                })
      
      
    
    }
  
  names(patent_output) <- list_patent_number
  
  return(patent_output)
}






## -- 

pbmapply <- function(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE) {
  
  pb <- pbapply::startpb()
  on.exit(closepb(pb))
  NN <- max(sapply(list(...), length))
  STEP <- 1 / NN
  VALUE <- 0
  
  env <- new.env()
  env$FUN <- FUN
  
  suppressMessages(trace(quote(FUN), exit = quote({
    env <- parent.frame(6)
    env$VALUE <- env$VALUE + env$STEP
    pbapply::setpb(parent.frame(6)$pb, env$VALUE)
  }), where = env, print = FALSE))
  
  
  mapply(env$FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY, USE.NAMES = USE.NAMES)
  
}
