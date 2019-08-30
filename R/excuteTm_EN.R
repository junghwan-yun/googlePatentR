## Gathering Patent Data from Google Patent
## https://patents.google.com/
##
## Author : Junghwan Yun
## Contact : junghwan.alfred.yun@gmail.com

source("R/init.R")
check_packages(c("tidyverse","pbapply", "tidytext"))
check_packages(c("udpipe", "topicmodels", "tm"))


tbl_patent_text_origin <- tbl_patent_doc


## 텍스트 데이터 기본 전처리
tbl_patent_text_adj <- tbl_patent_text_origin %>% 
  mutate(abstract_adj = abstract) %>% 
  mutate(abstract_adj = str_remove_all(abstract_adj, "\\n"),
         abstract_adj = str_remove_all(abstract_adj, "\\r"),
         abstract_adj = tolower(abstract_adj)) %>% 
  mutate(abstract_adj = str_trim(abstract_adj))

## 형태소 분석기 : Udpide Init
#en_model <- udpipe_download_model(language = "english")
udpipe_en_model <- udpipe_load_model(file = "MODEL/EN/english-ewt-ud-2.4-190531.udpipe")


## 형태소 분석 수행
udpipe_patent_doc <- udpipe(x = tbl_patent_text_adj$abstract_adj,
                            object = udpipe_en_model) %>% 
  as_tibble()



## 형태소 명사 추출
tbl_patent_noun_text <- udpipe_patent_doc %>% 
  filter(upos == "NOUN") %>% 
  select(udpipe_doc_id = doc_id, lemma) %>% 
  group_by(udpipe_doc_id) %>% 
  distinct(lemma) %>% 
  summarise(noun_text = paste(lemma, collapse = " "))



## 기존 문서번호와 결합
tbl_patent_doc_adj <- left_join(tbl_patent_text_adj %>% 
                                  select(patent_number, abstract, abstract_adj) %>% 
                                  add_column(udpipe_doc_id = udpipe_patent_doc %>% 
                                               distinct(doc_id) %>% 
                                               pull()),
                                tbl_patent_noun_text,
                                by = "udpipe_doc_id") %>% 
  select(-udpipe_doc_id)


## Tokenize
tbl_patent_word_adj <- tbl_patent_doc_adj %>% 
  unnest_tokens(word, noun_text) 

tbl_patent_word_count_total <- tbl_patent_word_adj %>% 
  count(patent_number, word, sort = T)


## 2회 미만 찾기 , 길이 2 이하 찾기 
tbl_stop_word <- tbl_patent_word_adj %>% 
  count(word, sort = T) %>% 
  mutate(nchar = nchar(word)) %>% 
  filter( n < 2 | nchar < 2) 


tf_noun_patent_filtered <- anti_join(tbl_patent_word_count_total,
                                     tbl_stop_word, by = "word")

dtm_noun_patent <- tf_noun_patent_filtered %>% 
  cast_dtm(document = patent_number,
           term = word,
           value =  n)


## LDA 실행
model_lda_post_whole <- LDA(dtm_noun_patent,
                            k = 3,
                            method = "Gibbs")


as.data.frame(terms(model_lda_post_whole, 30), stringsAsFactors=FALSE)

## 토픽 개수의 결정 방법
# http://www.rpubs.com/MNidhi/NumberoftopicsLDA




## LDA 시각화 
check_packages(c("LDAvis"))



phi <- posterior(model_lda_post_whole)$terms %>% as.matrix
theta <- posterior(model_lda_post_whole)$topics %>% as.matrix
vocab <- colnames(phi)
doc_length <- tf_noun_patent_filtered %>% 
  group_by(patent_number) %>% 
  summarise(word_cnt = n()) %>% 
  select(word_cnt) %>% 
  pull()


temp_frequency <- as.matrix(dtm_noun_patent)
freq_matrix <- data.frame(ST=colnames(temp_frequency),
                          Freq=colSums(temp_frequency))
rm(temp_frequency)

## 시각화 파일 생성
json_lda <- createJSON(phi = phi,
                       theta = theta,
                       vocab = vocab,
                       doc.length = doc_length,
                       term.frequency = freq_matrix$Freq)

## 결과물 저장
serVis(json_lda,
       out.dir="LDA-Visualize",
       open.browser=FALSE)
