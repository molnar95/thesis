library(xml2)
library(rvest)
library(tidyverse)
library(dplyr)

library(svMisc)

setwd("C:/Users/molna/Desktop/Szakdolgozat/Alternatívhírek/htmls")


#########################################
# Read pages and collect article URLs   #
#########################################

# set keywords and search urls
key1 <- "koronavírus" #55
key2 <- "covid"       #70
key3 <- "karantén"    #5
key4 <- "vuhan"       #0
key5 <- "vírus"       #55
key6 <- "járvány"     #45
key7 <- "vakcina"     #15  


links <- c()
for (i in seq(0, 15, 5)) {
  link <- paste0("https://www.alternativhirek.com/search?q=", key7, "&updated-max=2020-12-15T07:38:00-08:00&max-results=20&start=", i,
                 "&by-date=true")
  pages <- read_html(link)
  
  link <- unique(xml_text(as.vector(pages %>% html_nodes(xpath="//article//a//@href"))))
  link <- link[link != ""]
  link <- link[!grepl("#comment-form|blogger", link)]
  
  links <- append(link, links)
}
links


# download links HTML-s
# TODO: set key number!
pb <- txtProgressBar(min=0, max=length(links), style=3)
for (i in 1:length(links)){
  download_html(links[i], file= paste(key7, "_", i, ".html", sep = ""), mode="wb")
  
  setTxtProgressBar(pb, i)
}


#########################
# Extract HTML elements #
#########################

# Title extractor:
title_ext <- function(elem){
  tit <- xml_text(elem %>% html_nodes("title"))
  tit <- gsub("- ALTERNATÍV HÍREK", "", tit)
  tit <- gsub("\n", "", tit)
  
  return(tit)
}


# Article extractor:
text_ext <- function(elem){
  text <- xml_text(elem %>% html_nodes(xpath="//article//span"))
  text <- paste(text, collapse = " ")

  if (grepl("Szerkesztette:", text, fixed = TRUE)) {
      text <- gsub("\\Szerkesztette:.*", "", text)
  } else if (grepl("Fordította és írta:", text, fixed = TRUE)) {
      text <- gsub("\\Fordította és írta:.*", "", text)
  } else if (grepl("Fordította:", text, fixed = TRUE)) {
      text <- gsub("\\Fordította:.*", "", text)
  } else if (grepl("Forrás:", text, fixed = TRUE)) {
      text <- gsub("\\Forrás:.*", "", text) 
  } else if (grepl("◢ BLOG:", text, fixed = TRUE)) {
      text <- gsub("\\◢ BLOG:.*", "", text) 
  }
    
  return(text)
}


# Date extractor:
date_ext <- function(elem){
  date <- xml_text(elem %>% html_nodes(xpath="//abbr[@itemprop='datePublished']//@title"))[1]
  date <- gsub("T.*", "", date)

  return(date)
}


# Link extractor:
link_ext <- function(elem){
  link <- xml_text(elem %>% html_nodes(xpath = "//input[@class='post-share-link ']//@value"))
  
  return(link)
}


# Create df from HTML elements:
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
  setwd(paste0('C:/Users/molna/Desktop/Szakdolgozat/Alternatívhírek/', folder))
  linkfile <- list.files(path=".", pattern=".html$", full.names=FALSE, recursive=FALSE)
  
  i = 1
  while(i <= length(linkfile)){
    datas <- df_creator(read_html(linkfile[i],  encoding = "utf-8"))
    saveRDS(datas, 
            paste0('C:/Users/molna/Desktop/Szakdolgozat/Alternatívhírek/data', 
                   '/', 'dataframe', '_', i, '.RData'))
    print(i)
    i = i +1
  }
} 


# Read .rds files and append them in one df:
df_append <- function(folder){
  setwd(paste0('C:/Users/molna/Desktop/Szakdolgozat/Alternatívhírek/', folder))
  
  df <- list.files(pattern = ".RData") %>%
    map(readRDS) %>%
    data.table::rbindlist() 
  
  df <- as.data.frame(df)
  
  saveRDS(df, "C:/Users/molna/Desktop/Szakdolgozat/Alternatívhírek/alternativhirek_articles.rds")
  
  write.table(df, 'C:/Users/molna/Desktop/Szakdolgozat/Alternatívhírek/alternativhirek_articles.csv', sep = '%%',
              fileEncoding = "utf-8")
  
  return(df)
}


###################
# Function calls  #
###################

html_to_df('htmls')
df_append('data')

