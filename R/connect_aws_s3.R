## Author : Junghwan Yun
## Contact : junghwan.alfred.yun@gmail.com

source("R/init.R")
check_packages(c("tidyverse", "aws.s3"))

Sys.setenv("AWS_ACCESS_KEY_ID" = 'MY-ACCESS-KEY',
           "AWS_SECRET_ACCESS_KEY" = 'MY-ACCESS-SECRET-KEY',
           "AWS_DEFAULT_REGION" = 'MY-S3-REGION')

bucketlist()

result_s3_upload <- lapply(list.files("HTML/"),
                           function(files){
                             put_object(file = file.path("HTML/", files),
                                        object = files,
                                        bucket = "google-patents-html")
                           })

list_files_bucket_exist <- get_bucket("google-patents-html")


