library(xml2)
library(rvest)
library(tidyverse)
library(dplyr)

url <- "https://www.minden-egyben.com/?kw=koronav%C3%ADrus&oldal="
setwd("C:/Users/molna/Desktop/Szakdolgozat/Mindenegyben.Blog/htmls")


#########################################
# Read pages and collect article URLs   #
#########################################

links <- c()
for (i in 1:110){
  pages <- read_html(paste0(url, i))
  urls <- as.vector(pages %>% html_nodes("a.postbox_details")) %>% html_attr("href")
  
  links <- c(links, urls)
}
links


for (i in 1:length(links)){
  download_html(links[i],file= paste(i, ".html", sep = ""), mode="wb")
}


#########################
# Extract HTML elements #
#########################

# Title extractor:
title_ext <- function(elem){
  tit <- xml_text(elem %>% html_nodes("div.title_row"))
  tit <- gsub("\t\\s*|\n", "", title)
  
  return(tit)
}
title_ext(b)

# Article extractor:
text_ext <- function(elem){
  text <- xml_text(elem %>% html_nodes(xpath="//div[@class='post_content_holder']//p"))
  text <- paste(textbox, collapse = '')
  
  return(text)
}
text_ext(b)

# Date extractor:
date_ext <- function(elem){
  year <- xml_text(elem %>% html_nodes("span.year"))
  month <- xml_text(elem %>% html_nodes("span.month"))
  day <- xml_text(elem %>% html_nodes("span.day"))
  
  date <- paste0(year, '.', month, '.', day, '.')
  
  return(date)
}
date_ext(b)


# Link extractor:
link_ext <- function(elem){
  link <- xml_text(elem %>% html_nodes(xpath = "//link/@href"))[2]
  
  return(link)
}
link_ext(b)


# Create df from HTML elements:
df_creator <- function(elem){
  title <- title_ext(elem)
  date <- date_ext(elem)
  text <- text_ext(elem)
  link <- link_ext(elem)
  
  df <- data.frame(link, title, date, text)
  
  return(df)
}
df_creator(b)


#######################
# Process HTML files  #
#######################

# Read HTMLs, process, save dataframes:
html_to_df <- function(folder){
  setwd(paste0('C:/Users/molna/Desktop/Szakdolgozat/Mindenegyben.Blog/', folder))
  linkfile <- list.files(path=".", pattern=".html$", full.names=FALSE, recursive=FALSE)
  
  i = 1
  while(i <= length(linkfile)){
    datas <- df_creator(read_html(linkfile[i]))
    saveRDS(datas, 
            paste0('C:/Users/molna/Desktop/Szakdolgozat/Mindenegyben.Blog/data', 
                   '/', 'dataframe', '_', i, '.RData'))
    print(i)
    i = i +1
  }
} 


# Read .rds files and append them in one df:
df_append <- function(folder){
  setwd(paste0('C:/Users/molna/Desktop/Szakdolgozat/Mindenegyben.Blog/', folder))
  
  df <- list.files(pattern = ".RData") %>%
    map(readRDS) %>%
    data.table::rbindlist() 
  
  df <- as.data.frame(df)
  
  write.table(df, 'C:/Users/molna/Desktop/Szakdolgozat/Mindenegyben.Blog/mindenegyben_articles.csv', sep = '%%', fileEncoding = "utf-8")
  
  return(df)
}


###################
# Function calls  #
###################

html_to_df('htmls')
df_append('data')









######################
# Elements  - TEST   #
######################

#xml_text(b %>% html_nodes("div.post_content_holder"))
#(b %>% html_nodes(xpath="//div[@class='post_content_holder']//preceding-sibling::p"))
#text <- xml_text(b %>% html_nodes(xpath="//div/preceding-sibling::p"))
#xml_text(b %>% html_nodes(xpath="//div/preceding-sibling::p"))

"""
text <- function(elem){
  textbox <- xml_text(elem %>% html_nodes(xpath='//div/following-sibling::p'))
  textbox <- paste(textbox, collapse = '')
  
  return(textbox)
}
text(b)
"""



