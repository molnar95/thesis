library(xml2)
library(rvest)
library(tidyverse)
library(dplyr)

library(svMisc)

setwd("C:/Users/molna/Desktop/Szakdolgozat/KimondottIgazságok/htmls")


#########################################
# Read pages and collect article URLs   #
#########################################


links <- c()
pb <- txtProgressBar(min=0, max=32, style=3)
for (i in 1:32){
  link <- paste0("http://kimondottigazsagok.com/page/", i, "/?s=koronav%C3%ADrus")
  pages <- read_html(link)
  link <- xml_text(as.vector(pages %>% html_nodes(xpath = "//div/a//@href")))
  
  words <- c("http://titkokszigete.hu/", "http://titkokszigete.hu")
  link <- link[!grepl("page|kapcsolat|kategoria|facebook|themeruby|#", link)]
  link <- link[sapply(link, function(x) any(!(x %in% words)))]
  
  links <- append(link, links)
  
  setTxtProgressBar(pb, i)
}
links


no <- c("http://kimondottigazsagok.com/", "http://kimondottigazsagok.com/?s=koronav%C3%ADrus")
links <- links[!duplicated(links)]
links <- links[!(links %in% no)] 


#################################
# Read links and download HTMLs #
#################################

pb <- txtProgressBar(min=0, max=length(links), style=3)
for (i in 1:length(links)){
  download_html(links[i],file= paste(i, ".html", sep = ""), mode="wb")
  
  setTxtProgressBar(pb, i)
}


#########################
# Extract HTML elements #
#########################

# title extractor:
title_ext <- function(elem){
  tit <- xml_text(elem %>% html_nodes("title"))
  tit <- gsub("\\| Kimondott Igazságok.*","", tit)
  
  return(tit)
}

# date extractor:
date_ext <- function(elem){
  json <- xml_text(b %>% html_nodes("script"))
  json <- paste(json, collapse = '')
  json <- gsub('^.*"datePublished\\s*|\\s*T.*$', '', json)
  date <- gsub("\":\"","", json)
  
  return(date)
}
date_ext(b)

# article extractor:
text_ext <- function(elem){
  text <- xml_text(b %>% html_nodes(xpath="//div[@class='inner-post-entry']//p"))
  text <- paste(text, collapse = '')
  
  return(text)
}

# link extractor:
link_ext <- function(elem){
  link <- elem %>% html_nodes("link") %>% xml_attr('href')
  link <- link[5]
  
  return(link)
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
  setwd(paste0('C:/Users/molna/Desktop/Szakdolgozat/KimondottIgazságok/', folder))
  linkfile <- list.files(path=".", pattern=".html$", full.names=FALSE, recursive=FALSE)
  
  i = 1
  while(i <= length(linkfile)){
    datas <- df_creator(read_html(linkfile[i]))
    saveRDS(datas, 
            paste0('C:/Users/molna/Desktop/Szakdolgozat/KimondottIgazságok/data', 
                   '/', 'dataframe', '_', i, '.RData'))
    print(i)
    i = i +1
  }
} 


# Read .rds files and append them in one df:
df_append <- function(folder){
  setwd(paste0('C:/Users/molna/Desktop/Szakdolgozat/KimondottIgazságok/', folder))
  
  df <- list.files(pattern = ".RData") %>%
    map(readRDS) %>%
    data.table::rbindlist() 
  
  df <- as.data.frame(df)
  
  write.table(df, 'C:/Users/molna/Desktop/Szakdolgozat/KimondottIgazságok/kimondottigazsagok_articles.csv', sep = '%%', fileEncoding = "utf-8")
  
  return(df)
}


###################
# Function calls  #
###################

html_to_df('htmls')
df_append('data')




#############
# Próbák    #
#############

link <- paste0("http://kimondottigazsagok.com/page/", 2, "/?s=koronav%C3%ADrus")
pages <- read_html(link)
link <- xml_text(as.vector(pages %>% html_nodes(xpath = "//h2/a//@href")))


b <- read_html("C:/Users/molna/Desktop/Szakdolgozat/KimondottIgazságok/htmls/112.html")
b
