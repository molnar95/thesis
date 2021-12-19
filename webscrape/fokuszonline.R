library(xml2)
library(rvest)
library(tidyverse)
library(dplyr)

library(svMisc)

setwd("C:/Users/molna/Desktop/Szakdolgozat/FókuszOnline/htmls")


#########################################
# Read pages and collect article URLs   #
#########################################


# set keywords and search urls
key1 <- "koronavírus"
key2 <- "covid"
key3 <- "karantén"
key4 <- "vuhan"
key5 <- "vírus"
key6 <- "járvány"

category_1 <- "aktualis"
category_2 <- "eletmod"

# iterating on search urls and save 
links <- c()
pb <- txtProgressBar(min=1, max=2, style=3)
for (i in 1:2){
  link <- paste0("https://fokusz.online/", category_2, "/oldal/", i)
  pages <- read_html(link)
  link <- xml_text(as.vector(pages %>% html_nodes(xpath = "//div/a//@href")))
  
  words <- c("https://fokusz.online/")
  link <- link[sapply(link, function(x) any(!(x %in% words)))]
  
  link <- link[!grepl("egyeb|gasztro|nagyvilag|praktikak|eletmod|aktualis", link)]

  
  links <- append(link, links)
  
  setTxtProgressBar(pb, i)
}

links <- unique(links)


# download links HTML-s
pb <- txtProgressBar(min=0, max=length(links), style=3)
for (i in 1:length(links)){
  download_html(links[i],file= paste(category_2, "_", i, ".html", sep = ""), mode="wb")
  
  setTxtProgressBar(pb, i)
}



#########################
# Extract HTML elements #
#########################

#b <- read_html('aktualis_16.html')

# title extractor:
title_ext <- function(elem){
  tit <- xml_text(elem %>% html_nodes("title"))
  tit <- gsub("- Fókusz Online", "", tit)
  
  return(tit)
}
title_ext(b)


# date extractor:
date_ext <- function(elem){
  date <- xml_text(elem %>% html_nodes(xpath="//meta[@itemprop='datePublished']/@content"))
  
  return(date)
}


# link extractor:
link_ext <- function(elem){
  link <- xml_text(elem %>% html_nodes(xpath="//meta[@property='og:url']/@content"))
  
  return(link)
}
link_ext(b)


# article extractor:
text_ext <- function(elem){
  text <- xml_text(elem %>% html_nodes("p"))
  text <- paste(text, collapse = ' ')
  text <- gsub('\r\n\r\n', '', text)
  text <- gsub('adsbygoogle.*?\r\n', '', text)
  text <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", text, perl=TRUE)
  text <- gsub('\r\n', '', text)
  text <- gsub('\r', '', text)
  
  return(text)
}  
text_ext(b)


# create dataframe from HTML elements:
df_creator <- function(elem){
  title <- title_ext(elem)
  date <- "NA"
  text <- text_ext(elem)
  link <- link_ext(elem)
  
  df <- data.frame(link, title, date, text)
  
  return(df)
}

as.character(b)

str_count(as.character(b), "window.adsbygoogle")

#######################
# Process HTML files  #
#######################

# Read HTMLs, process, save dataframes:
html_to_df <- function(folder){
  setwd(paste0('C:/Users/molna/Desktop/Szakdolgozat/FókuszOnline/', folder))
  linkfile <- list.files(path=".", pattern=".html$", full.names=FALSE, recursive=FALSE)
  
  i = 1
  while(i <= length(linkfile)){
    datas <- df_creator(read_html(linkfile[i]))
    saveRDS(datas, 
            paste0('C:/Users/molna/Desktop/Szakdolgozat/FókuszOnline/data', 
                   '/', 'dataframe', '_', i, '.RData'))
    print(i)
    i = i +1
  }
} 


# Read .rds files and append them in one df:
df_append <- function(folder){
  setwd(paste0('C:/Users/molna/Desktop/Szakdolgozat/FókuszOnline/', folder))
  
  df <- list.files(pattern = ".RData") %>%
    map(readRDS) %>%
    data.table::rbindlist() 
  
  df <- as.data.frame(df)
  
  df <- saveRDS(df, file="C:/Users/molna/Desktop/Szakdolgozat/FókuszOnline/fokuszonline_articles.rds")
  
  write.table(df, 'C:/Users/molna/Desktop/Szakdolgozat/FókuszOnline/fokuszonline_articles.csv', sep = '%%', fileEncoding = "utf-8")
  
  return(df)
}


###################
# Function calls  #
###################

html_to_df('htmls')
df_append('data')

