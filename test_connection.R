library(mongolite)
library(jsonlite)
library(tidyverse)




data <- fromJSON("rawInfo.json") 


## Connect to the database

username <- Sys.getenv("username")
password <- Sys.getenv("password")
data_base <- Sys.getenv("data_base")
cluster <- Sys.getenv("cluster")
url <- paste0("mongodb+srv://",username,":",password,"@",cluster,".rjzoaxj.mongodb.net/",data_base,"?retryWrites=true&w=majority")

# url <-"mongodb+srv://immanuelwilliams:66ShA2CVMPN67Kc8@cluster0.rjzoaxj.mongodb.net/stability?retryWrites=true&w=majority"

mongo <- mongo(url = url,
               collection = "emans_info", 
               db = "stability")




mongo$insert(data)

mongo$find('{}')
