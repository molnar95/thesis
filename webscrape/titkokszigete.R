library(xml2)
library(rvest)
library(tidyverse)
library(dplyr)

library(svMisc)

setwd("C:/Users/molna/Desktop/Szakdolgozat/TitkokSzigete/htmls")


#########################################
# Read pages and collect article URLs   #
#########################################

key1 <- "koronavírus" #51
key2 <- "covid"       #17
key3 <- "karantén"    #8
key4 <- "vuhan"       #3
key5 <- "vírus"       #59
key6 <- "járvány"     #51
key7 <- "vakcina"     #12


links <- c()
pb <- txtProgressBar(min=0, max=12, style=3)
for (i in 1:12){
  link <- paste0("http://titkokszigete.hu/page/", i, "/?s=", key7)
  pages <- read_html(link)
  link <- xml_text(as.vector(pages %>% html_nodes(xpath = "//div/a//@href")))
  
  words <- c("http://titkokszigete.hu/", "http://titkokszigete.hu")
  link <- link[!grepl("page|kapcsolat|kategoria|facebook|themeruby|#", link)]
  link <- link[sapply(link, function(x) any(!(x %in% words)))]
  
  links <- append(link, links)
  
  setTxtProgressBar(pb, i)
}

links <- unique(links)

links



pb <- txtProgressBar(min=0, max=length(links), style=3)
for (i in 1:length(links)){
  download_html(links[i], file= paste(key7, "_", i, ".html", sep = ""), mode="wb")
  
  setTxtProgressBar(pb, i)
}


#########################
# Extract HTML elements #
#########################

# title extractor:
title_ext <- function(elem){
  tit <- xml_text(elem %>% html_nodes("title"))
  tit <- gsub("\\| Titkok Szigete.*","", tit)
  
  return(tit)
}


# article extractor:
text_ext <- function(elem){
  text <- xml_text(elem %>% html_nodes(xpath="//div[@class='main-site-wrap']//p"))
  text <- paste(text, collapse = '')
  text <- gsub("2019 Titkokszigete.hu - Minden jog fenntartva!", "", text)

  return(text)
}  


# date extractor:
date_ext <- function(elem){
  date <- elem %>% html_nodes("time") %>% xml_attr('datetime')
  date <- date[1]
  date <- gsub("\\T.*","", date)
  
  return(date)
}


# link extractor:
link_ext <- function(elem){
  link <- elem %>% html_nodes("link") %>% xml_attr('href')
  link <- link[3]
  
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
  setwd(paste0('C:/Users/molna/Desktop/Szakdolgozat/TitkokSzigete/', folder))
  linkfile <- list.files(path=".", pattern=".html$", full.names=FALSE, recursive=FALSE)
  
  i = 1
  while(i <= length(linkfile)){
    datas <- df_creator(read_html(linkfile[i]))
    saveRDS(datas, 
            paste0('C:/Users/molna/Desktop/Szakdolgozat/TitkokSzigete/data', 
                   '/', 'dataframe', '_', i, '.RData'))
    print(i)
    i = i +1
  }
} 


# Read .rds files and append them in one df:
df_append <- function(folder){
  setwd(paste0('C:/Users/molna/Desktop/Szakdolgozat/TitkokSzigete/', folder))
  
  df <- list.files(pattern = ".RData") %>%
    map(readRDS) %>%
    data.table::rbindlist() 
  
  df <- as.data.frame(df)
  
  saveRDS(df, 'C:/Users/molna/Desktop/Szakdolgozat/TitkokSzigete/titkokszigete_articles.rds')
  
  write.table(df, 'C:/Users/molna/Desktop/Szakdolgozat/TitkokSzigete/titkokszigete_articles.csv', sep = '%%', fileEncoding = "utf-8")
  
  return(df)
}


###################
# Function calls  #
###################

html_to_df('htmls')
df_append('data')



