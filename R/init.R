## Gathering Patent Data from Google Patent
## https://patents.google.com/
##
## Author : Junghwan Yun
## Contact : junghwan.alfred.yun@gmail.com


## EC2 처음 업로드 한 경우 반드시 실행
#system("sudo apt install -y libcurl4-openssl-dev libssl-dev libxslt1-dev")
#system("sudo apt install -y libssl-dev libsasl2-dev libssh2-1-dev")


check_packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  result <- sapply(pkg, require, character.only = TRUE)
  print(result)
}

options(scipen = 999)
