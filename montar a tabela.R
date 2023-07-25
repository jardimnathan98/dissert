library (rvest)
library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)

link= 'https://pt.wikipedia.org/wiki/Lista_de_pessoas_envolvidas_na_Opera%C3%A7%C3%A3o_Lava_Jato'
site=read_html(link)
table= site %>%
  html_nodes("td, th") %>% 
  ##select table element
  html_text()
table[3]
table_excel<-matrix(0,210,5)
i<-1
k<-0
while (i<210) {
  table_excel[i,1]<-table[i+2+k]
  table_excel[i,2]<-table[i+2 +1+k]
  table_excel[i,3]<-table[i+2+ 2+k]
  table_excel[i,4]<-table[i+2 +3+k]
  table_excel[i,5]<-table[i+2 +4+k]
  i<-i+1
  k<-k+4
  }

table[968]

table[8]
a<-c(table[1:35],table[31],table[36:length(a)] )
a<-c(a[1:40], table[31],a[41:length(a)])
a<-c(a[1:45], table[31],a[46:length(a)])
a<-c(a[1:401], '',a[402:length(a)])
a<-c(a[1:797], a[793:795],a[798:length(a)])
a<-c(a[1:807], a[803:805],a[808:length(a)])
a<-c(a[1:817], a[813:815],a[818:length(a)])
a<-c(a[1:827], a[823:825],a[828:length(a)])
a<-c(a[1:837], a[833:835],a[838:length(a)])
a<-c(a[1:882], a[878:880],a[883:length(a)])
a<-c(a[1:912], a[908:910],a[913:length(a)])
a<-c(a[1:927], a[923:925],a[928:length(a)])
a<-c(a[1:952], a[948:950],a[953:length(a)])
a<-c(a[1:972], a[968:970],a[973:length(a)])
table<-a
length(a)
tail(table)


link= 'https://pt.wikipedia.org/wiki/Lista_de_pessoas_condenadas_na_Opera%C3%A7%C3%A3o_Lava_Jato'
site=read_html(link)
table= site %>%
  html_nodes("th , .sortable td") %>% 
  ##select table element
  html_text()
table_excel<-matrix(0,210,8)
i<-1
k<-0
while (i<210) {
  table_excel[i,1]<-table[i+k]
  table_excel[i,2]<-table[i +1+k]
  table_excel[i,3]<-table[i+ 2+k]
  table_excel[i,4]<-table[i +3+k]
  table_excel[i,5]<-table[i +4+k]
  table_excel[i,6]<-table[i+ 5+k]
  table_excel[i,7]<-table[i +6+k]
  table_excel[i,8]<-table[i+7+k]
   i<-i+1
  k<-k+7
}
table[1]
table[9]
