setwd("C:/Users/Nathan/Download/dissertacoes/output")
getwd()
library (rvest)
library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)
library(xlsx)
link= 'https://www.mpf.mp.br/grandes-casos/lava-jato/acoes/lavajato-acoes-view'
site=read_html(link)
table= site %>%
  html_nodes(".p-3 div , p" ) %>% 
  ##select table element
  html_text()
table[7]