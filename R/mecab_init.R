## Gathering Patent Data from Google Patent
## https://patents.google.com/
##
## Author : Junghwan Yun
## Contact : junghwan.alfred.yun@gmail.com



## RmecabKo 라이브러리 설치 (https://bitbucket.org/eunjeon/mecab-ko/src/master/README.md)
# wget https://bitbucket.org/eunjeon/mecab-ko/downloads/mecab-0.996-ko-0.9.2.tar.gz
# tar zxfv mecab-ko-XX.tar.gz
# cd mecab-ko-XX
# ./configure 
# make
# make check
# sudo make install

## RmecabKo Ko dic 소개 (https://bitbucket.org/eunjeon/mecab-ko-dic/src/master/)
# tar zxfv mecab-ko-dic-XX.tar.gz
# cd mecab-ko-dic-XX
# ./configure 
# make
# sudo make install

## 사용자 사전 설정 
# https://bitbucket.org/eunjeon/mecab-ko-dic/src/df15a487444d88565ea18f8250330276497cc9b9/final/user-dic/README.md

## 오류시 대처 방법
# https://springofmylife.tistory.com/entry/elasticsearch%EC%9D%80%EC%A0%84%ED%95%9C%EB%8B%A2-%EC%84%A4%EC%B9%98
# ldconfig
# ldconfig -p | grep /usr/local/lib

## Stopwords 목록
# https://www.ranks.nl/stopwords/korean
# https://gist.github.com/spikeekips/40eea22ef4a89f629abd87eed535ac6a
# https://cran.r-project.org/web/packages/stopwords/README.html