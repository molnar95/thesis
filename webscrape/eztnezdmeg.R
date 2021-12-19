library(xml2)
library(rvest)
library(tidyverse)
library(dplyr)

library(svMisc)

setwd("C:/Users/molna/Desktop/Szakdolgozat/Eztnézdmeg/htmls1")


#########################################
# Read pages and collect article URLs   #
#########################################


# set keywords and search urls
key1 <- "koronavírus" # 142
key2 <- "covid"       # 41
key3 <- "karantén"    # 29
key4 <- "vuhan"       # 4
key5 <- "vírus"       # 150
key6 <- "járvány"     # 113
key7 <- "vakcina"     # 67

key <- key7
iter <- 67

# iterating on search urls and save 
# article links
# TODO: set link number!!
links <- c()
pb <- txtProgressBar(min=0, max=10, style=3)
for (i in 1:iter){
  link <- paste0("http://eztnezdmeg.com/page/", i, "/?s=", key)
  pages <- read_html(link)
  
  link <- xml_text(as.vector(pages %>% html_nodes(xpath = "//div/a//@href")))
  link <- link[!grepl("page|kapcsolat|kategoria|jogi-nyilatkozat|facebook|themeruby|#|javascript", link)]

  links <- append(link, links)
  
  setTxtProgressBar(pb, i)
}

links <- unique(links)


# download links HTML-s
# TODO: set key number!
pb <- txtProgressBar(min=0, max=length(links), style=3)
for (i in 1:length(links)){
  download_html(links[i],file= paste(key, "_", i, ".html", sep = ""), mode="wb")
  
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
  date <- xml_text(elem %>% html_nodes(xpath="//meta[@itemprop='datePublished']/@content"))
  
  return(date)
}

# link extractor:
link_ext <- function(elem){
  link <- xml_text(elem %>% html_nodes(xpath="//meta[@property='og:url']/@content"))
  link <- link[1]
  
  return(link)
}

# article extractor:
text_ext <- function(elem){
  text <- xml_text(elem %>% html_nodes("h4"))
  text <- paste(text, collapse = ' ')
  
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
  setwd(paste0('C:/Users/molna/Desktop/Szakdolgozat/Eztnézdmeg/', folder))
  linkfile <- list.files(path=".", pattern=".html$", full.names=FALSE, recursive=FALSE)
  
  i = 1
  while(i <= length(linkfile)){
    datas <- df_creator(read_html(linkfile[i], encoding = "utf-8"))
    saveRDS(datas, 
            paste0('C:/Users/molna/Desktop/Szakdolgozat/Eztnézdmeg/data', 
                   '/', 'dataframe', '_', i, '.RData'))
    print(i)
    i = i +1
  }
} 


# Read .rds files and append them in one df:
df_append <- function(folder){
  setwd(paste0('C:/Users/molna/Desktop/Szakdolgozat/Eztnézdmeg/', folder))
  
  df <- list.files(pattern = ".RData") %>%
    map(readRDS) %>%
    data.table::rbindlist() 
  
  df <- as.data.frame(df)
  
  write.table(df, 'C:/Users/molna/Desktop/Szakdolgozat/Eztnézdmeg/eztnezdmeg_articles.csv', sep = '%%', fileEncoding = "utf-8")
  
  return(df)
}


###################
# Function calls  #
###################

html_to_df('htmls1')
df_append('data')






