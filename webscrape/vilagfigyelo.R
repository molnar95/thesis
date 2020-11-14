library(xml2)
library(rvest)
library(tidyverse)
library(dplyr)

library(svMisc)


#########################
# Extract HTML elements #
#########################

# title extractor:
title_ext <- function(elem){
  tit <- xml_text(elem %>% html_nodes("title"))
  tit <- gsub("- Világ Figyelő.*","", tit)
  
  return(tit)
}


# date extractor:
date_ext <- function(elem){
  date <- xml_text(elem %>% html_nodes("time"))
  
  return(date)
}


# url extractor:
link_ext <- function(elem){
  link <- elem %>% html_nodes("link") %>% xml_attr('href')
  link <- link[2]
  
  return(link)
}


# article extractor:
text_ext <- function(elem){
  text <- xml_text(b %>% html_nodes("p"))
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
  setwd(paste0('C:/Users/molna/Desktop/Szakdolgozat/Világfigyelő/', folder))
  linkfile <- list.files(path=".", pattern=".html$", full.names=FALSE, recursive=FALSE)
  
  i = 1
  while(i <= length(linkfile)){
    datas <- df_creator(read_html(linkfile[i]))
    saveRDS(datas, 
            paste0('C:/Users/molna/Desktop/Szakdolgozat/Világfigyelő/data', 
                   '/', 'dataframe', '_', i, '.RData'))
    print(i)
    i = i +1
  }
} 


# Read .rds files and append them in one df:
df_append <- function(folder){
  setwd(paste0('C:/Users/molna/Desktop/Szakdolgozat/Világfigyelő/', folder))
  
  df <- list.files(pattern = ".RData") %>%
    map(readRDS) %>%
    data.table::rbindlist() 
  
  df <- as.data.frame(df)
  
  write.table(df, 'C:/Users/molna/Desktop/Szakdolgozat/Világfigyelő/vilagfigyelo_articles.csv', sep = '%%', fileEncoding = "utf-8")
  
  return(df)
}


###################
# Function calls  #
###################

html_to_df('htmls')
df_append('data')

