library(xml2)
library(rvest)
library(tidyverse)
library(dplyr)

library(svMisc)

setwd("C:/Users/molna/Desktop/Szakdolgozat/TudásFája/htmls")


#########################################
# Read pages and collect article URLs   #
#########################################

key1 <- "koronavírus" #72
key2 <- "covid"       #34
key3 <- "karantén"    #27
key4 <- "vuhan"       #2
key5 <- "vírus"       #88
key6 <- "járvány"     #57
key7 <- "vakcina"     #11  


links <- c()
pb <- txtProgressBar(min=0, max=11, style=3)
for (i in 1:11){
  link <- paste0("https://www.tudasfaja.com/page/", i, "/?s=", key7)
  pages <- read_html(link)
  link <- xml_text(as.vector(pages %>% html_nodes(xpath = "//div/a//@href")))
  
  words <- c("https://www.tudasfaja.com/")
  link <- link[sapply(link, function(x) any(!(x %in% words)))]
  link <- link[!grepl("category|#|javascript|?s=koronav%C3%ADrus|?s=covid", link)]
  link <- link[!grepl("?s=karant%C3%A9n|?s=vuhan|?s=v%C3%ADrus|?s=vakcina|?s=j%C3%A1rv%C3%A1ny", link)]
  
  links <- append(link, links)
  
  setTxtProgressBar(pb, i)
}
links

links <- unique(links)

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
  
  return(tit)
}


# date extractor:
date_ext <- function(elem){
  date <- elem %>% html_nodes("time") %>% xml_attr('datetime')
  date <- substring(date, 1, 10)
  return(date[1])
}


# link extractor:
link_ext <- function(elem){
  link <- elem %>% html_nodes("link") %>% xml_attr('href')
  link <- link[4]
  
  return(link)
}


# article extractor:
text_ext <- function(elem){
  text <- elem %>% html_nodes(xpath = "//div[@class='td-post-content td-pb-padding-side']") %>% html_text()
  
  text <- gsub("var td_screen_width = document.body.clientWidth.*?                   }", '', text)
  text <- gsub("td_screen_width.*?push", '', text)
  text <- gsub("\n", '', text)
  text <- gsub("\r", '', text)
  text <- gsub("if ( ({});", '', text, fixed = TRUE)
  text <- gsub("}", '', text)
  text <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", text, perl=TRUE)
  text <- gsub("(adsbygoogle.*?);", "", text,  perl = TRUE)
  
  return(text)
}


# advertisement count:
adv_ext <- function(elem){
  text <- as.character(elem)
  count <- str_count(elem, "window.adsbygoogle")
  
  return(count)
}


# create dataframe from HTML elements:
df_creator <- function(elem){
  title <- title_ext(elem)
  date <- date_ext(elem)
  text <- text_ext(elem)
  link <- link_ext(elem)
  num_adv <- adv_ext(elem)
  
  df <- data.frame(link, title, date, text, num_adv)
  
  return(df)
}



#######################
# Process HTML files  #
#######################

# Read HTMLs, process, save dataframes:
html_to_df <- function(folder){
  setwd(paste0('C:/Users/molna/Desktop/Szakdolgozat/TudásFája/', folder))
  linkfile <- list.files(path=".", pattern=".html$", full.names=FALSE, recursive=FALSE)
  
  i = 1
  while(i <= length(linkfile)){
    datas <- df_creator(read_html(linkfile[i],  encoding = "utf-8"))
    saveRDS(datas, 
            paste0('C:/Users/molna/Desktop/Szakdolgozat/TudásFája/data', 
                   '/', 'dataframe', '_', i, '.RData'))
    print(i)
    i = i +1
  }
} 


# Read .rds files and append them in one df:
df_append <- function(folder){
  setwd(paste0('C:/Users/molna/Desktop/Szakdolgozat/TudásFája/', folder))
  
  df <- list.files(pattern = ".RData") %>%
    map(readRDS) %>%
    data.table::rbindlist() 
  
  df <- as.data.frame(df)
  
  saveRDS(df, "C:/Users/molna/Desktop/Szakdolgozat/TudásFája/tudasfaja_articles.rds")
  
  write.table(df, 'C:/Users/molna/Desktop/Szakdolgozat/TudásFája/tudasfaja_articles.csv', sep = '%%', fileEncoding = "utf-8")
  
  return(df)
}


###################
# Function calls  #
###################

html_to_df('htmls')
df_append('data')



