#R Studio API Code
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Library Imports
library(tidyverse)
library(ggplot2)
library(xml2)
library(rvest)

# Data Import
scholar_html <- read_html("https://scholar.google.com/scholar?start=0&q=%22covid-19%22+source:psychology&hl=en&as_sdt=0,24")
page_num <- scholar_html %>%
  html_nodes("#gs_ab_md .gs_ab_mdw") %>%
  html_text() %>%
  str_extract("\\d+")
page_var <- seq(0, page_num, 10)
list_of_pages <- str_c("https://scholar.google.com/scholar", '?start=', page_var, "&q=%22covid-19%22+source:psychology&hl=en&as_sdt=0,24")

profile_tbl <- c()
for (url in list_of_pages){
  html <- read_html(url)

  title_list <- c()
  title_nodes <- html %>% 
    html_nodes(".gs_rt") 
  
  for (i in 1:length(title_nodes)){
    title <- title_nodes[i] %>%
      html_node("a") %>%
      html_text()
    title_list <- append(title_list, title)
  }
  
  author <- html %>%
    html_nodes(".gs_a") %>%
    html_text() %>%
    str_extract("^\\w(\\w|\\s|\\,)*\\w")
  
  journal <- html %>%
    html_nodes(".gs_a") %>%
    html_text() %>%
    str_extract('\\-(\\s.+)\\d') %>%
    str_extract("\\D+")
  
  year <- html %>%
    html_nodes(".gs_a") %>%
    html_text() %>%
    str_extract("\\d{4}")
  
  link_list <- c()
  link_nodes <- html %>% 
    html_nodes(".gs_rt") 
  
  for (i in 1:length(link_nodes)){
    link <- link_nodes[i] %>%
      html_node("a") %>%
      html_attr("href")
    link_list <- append(link_list, link)
  }

  tbl <- as_tibble(bind_cols(title = title_list, author = author, journal = journal, year = year, link = link_list))
  profile_tbl <- bind_rows(profile_tbl, tbl)
}

profile_tbl <- profile_tbl %>%
  mutate(journal = str_remove_all(journal, "[…]|\\-") %>% 
           str_extract("[A-Z].+") %>%
           str_to_lower()) %>%
  na.omit(journal) 

# Data analysis 
table <- profile_tbl %>%
  group_by(journal) %>% 
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  filter(n %in% head(n, 10))

top_tbl <- profile_tbl %>% 
  filter(journal %in% table$journal) %>%
  group_by(journal) %>%
  count(year)

# Visualization
ggplot(top_tbl, aes(x = year, y = n, fill = journal)) +
  geom_col() + 
  xlab("publication year") + 
  ylab("number of publication") + 
  labs(fill = "top 10 journals")






