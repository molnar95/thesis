library(xml2)
library(rvest)
library(tidyverse)
library(dplyr)

library(svMisc)


setwd("C:/Users/molna/Desktop/Szakdolgozat/AvilágTitkai/htmls")


#########################################
# Read pages and collect article URLs   #
#########################################

# set keywords and search urls
key1 <- "koronavírus"
link1 <- paste0("https://avilagtitkai.com/articles/search?string=", key1, "&page=")

key2 <- "covid"
link2 <- paste0("https://avilagtitkai.com/articles/search?string=", key2, "&page=")

key3 <- "karantén"
link3 <- paste0("https://avilagtitkai.com/articles/search?string=", key3, "&page=")

key4 <- "vuhan"
link4 <- paste0("https://avilagtitkai.com/articles/search?string=", key4, "&page=")

key5 <- "vírus"
link5 <- paste0("https://avilagtitkai.com/articles/search?string=", key5, "&page=")

key6 <- "járvány"
link6 <- paste0("https://avilagtitkai.com/articles/search?string=", key6, "&page=")

key7 <- "vakcina"
link7 <- paste0("https://avilagtitkai.com/articles/search?string=", key7, "&page=")

link <- link7
key <- key7

# iterating on search urls and save 
# article links
# TODO: set link number!!
links <- c()
pb <- txtProgressBar(min=0, max=5, style=3)
for (i in 1:10){
  
  link <- paste0(link, i)
  pages <- read_html(link)
  link <- xml_text(as.vector(pages %>% html_nodes(xpath = "//div/a//@href")))
  
  words <- c("https://avilagtitkai.com", "#")
  link <- link[sapply(link, function(x) any(!(x %in% words)))]
  
  link <- paste0('https://avilagtitkai.com', link)
  links <- append(link, links)
  
  setTxtProgressBar(pb, i)
}
links


# download links HTML-s
# TODO: set key number!
pb <- txtProgressBar(min=0, max=length(links), style=3)
for (i in 1:length(links)){
  download_html(links[i], file= paste(key, "_", i, ".html", sep = ""), mode="wb")
  
  setTxtProgressBar(pb, i)
}


#########################
# Extract HTML elements #
#########################


# title extractor:
title_ext <- function(elem){
  tit <- xml_text(elem %>% html_nodes("title"))
  
  return(tit)
}


# date extractor:
date_ext <- function(elem){
  date <- xml_text(elem %>% html_nodes(xpath="//span[@class='date-icon icon-clock']"))[1]
  date <- gsub(',.*', '', date)
  
  return(date)
}


# link extractor:
link_ext <- function(elem){
  link <- xml_text(elem %>% html_nodes(xpath="//meta[@property='og:url']/@content"))
  
  return(link)
}


# article extractor:
text_ext <- function(elem){
  text <- xml_text(elem %>% html_nodes("p"))
  text <- paste(text, collapse = ' ')
  text <- gsub('\n', '', text)
  
  return(text)
}  


# create dataframe from HTML elements:
df_creator <- function(elem){
  title <- title_ext(elem)
  date <- date_ext(elem)
  text <- text_ext(elem)
  link <- link_ext(elem)
  
  df <- data.frame(link, title, date, text)
  
  return(df)
}



#######################
# Process HTML files  #
#######################

# Read HTMLs, process, save dataframes:
html_to_df <- function(folder){
  setwd(paste0('C:/Users/molna/Desktop/Szakdolgozat/AvilágTitkai/', folder))
  linkfile <- list.files(path=".", pattern=".html$", full.names=FALSE, recursive=FALSE)
  
  i = 1
  while(i <= length(linkfile)){
    datas <- df_creator(read_html(linkfile[i]))
    saveRDS(datas, 
            paste0('C:/Users/molna/Desktop/Szakdolgozat/AvilágTitkai/data', 
                   '/', 'dataframe', '_', i, '.RData'))
    print(i)
    i = i +1
  }
} 


# Read .rds files and append them in one df:
df_append <- function(folder){
  setwd(paste0('C:/Users/molna/Desktop/Szakdolgozat/AvilágTitkai/', folder))
  
  df <- list.files(pattern = ".RData") %>%
    map(readRDS) %>%
    data.table::rbindlist() 
  
  df <- as.data.frame(df)
  
  write.table(df, 'C:/Users/molna/Desktop/Szakdolgozat/AvilágTitkai/avilagtitkai_articles.csv', sep = '%%', fileEncoding = "utf-8")
  
  return(df)
}


###################
# Function calls  #
###################

html_to_df('htmls')
df_append('data')

