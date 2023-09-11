library(tidytext)
library(arrow)
library(stringr)
library(tidyverse)
library(dplyr)
library(tokenizers)
library(udpipe)
library(tm)
library(lubridate)
library(plyr)
library(tm)
setwd("C:/Users/Nathan/Downloads/dissertacoes")
#data <- arrow::read_parquet("news.parquet")
load("news.Rdata")


# Função para contar os zeros em uma coluna
contar_zeros<- function(col) {
  sum(col == 0)
  
}

################
merge_dtm<-function(dtm_tot){
  last_matriz<- as.data.frame(as.matrix(dtm_tot[[1]]))
  now_matriz<- as.data.frame(as.matrix(dtm_tot[[2]]))
  
  dif_name<-setdiff(colnames(now_matriz), colnames(last_matriz))
  
  filler<-matrix( 0, nrow = nrow(last_matriz), ncol = length(dif_name))
  
  colnames( filler)<- dif_name
  last_matriz<-cbind(last_matriz,filler)
  
 new_matriz<-rbind.fill(last_matriz,now_matriz)
 
 new_matriz[-(1:nrow(last_matriz)),][is.na(new_matriz[-(1:nrow(last_matriz)),])] <- 0

  dtm_tot<- as(as.matrix(new_matriz), "CsparseMatrix")
 
 return(dtm_tot)
}


#####################
linhas_contendo_lava <- data %>%
  filter_all(any_vars(grepl("lava jato", ., ignore.case = TRUE))) 


# Baixar o modelo para o português
modelo_portugues <- udpipe_download_model(language = "portuguese")

# Carregar o modelo
ud_model <- udpipe_load_model(modelo_portugues$file_model)

lemmatize_text <- function(texto, ud_model) {
  tokens <- udpipe_annotate(ud_model, x = texto)
  lemas <-as.data.frame(tokens)$lemma
  return(lemas)
}





linhas_contendo_lava$lemma<-0
# linhas_contendo_lava$lemma de exemplo
linhas_contendo_lava$lemma <- "Este é um exemplo de linhas_contendo_lava$lemma com algumas palavras. 1234"

# 1. Converter para minúsculas
linhas_contendo_lava$lemma <- tolower(linhas_contendo_lava$Text)

# 2. Remover pontuações e números
linhas_contendo_lava$lemma <- gsub("[[:punct:]]|\\d+", " ", linhas_contendo_lava$lemma)

# 3. Remover espaços em branco extras
linhas_contendo_lava$lemma <- gsub("\\s+", " ", linhas_contendo_lava$lemma)

# 5. Remoção de stopwords (exemplo com stopwords em inglês)
stopwords <- stopwords("pt")  # Substitua "en" pelo idioma desejado



for (position  in 1:nrow(linhas_contendo_lava)) {
  # 4. Tokenização (dividir em palavras)
  tokens <- unlist(strsplit(linhas_contendo_lava$lemma[1], " "))
 
   tokens <- tokens[!tokens %in% stopwords]
  lema1<- lemmatize_text(tokens, ud_model)
 
lema1<-na.omit(lema1)
  # Criar 2-grams a partir da lista de tokens
  lema2 <- c()
  for (j in 1:(length(lema1) - 1)) {
    lema2 <- c(ngram_tokens, paste(lema1[j], lema1[j+1], sep = "."))
  }
  
  }

# Carregue a biblioteca 'tm' para processamento de texto


# Crie um corpus a partir dos seus documentos
corpus <- Corpus(VectorSource(list1))

corpus2 <- Corpus(VectorSource(list2))
###### vamos tirar mensalmente os termos##############


matriz3<-DocumentTermMatrix(DataframeSource(matriz))


# Crie a matriz de frequência de termos (TF)
dtm <- DocumentTermMatrix(corpus)
dtm2 <- DocumentTermMatrix(corpus2)
as.DocumentTermMatrix(matriz)

# Crie uma lista de DataFrames, um para cada mês




linhas_contendo_lava$year<-year(linhas_contendo_lava$Dates)
linhas_contendo_lava$month<-month(linhas_contendo_lava$Dates)
linhas_contendo_lava$day<-day(linhas_contendo_lava$Dates)


line<-which(linhas_contendo_lava$year == 2012)# sumir com isso
linhas_contendo_lava<-linhas_contendo_lava[-line,]
dtm_tot<-list()

for(ano in 2015:2023){
  lines<-which(linhas_contendo_lava$year == ano) 
for(mes in 1:12){
  lines_month<- which(linhas_contendo_lava$month[lines] == mes)
  
  corpus <- Corpus(VectorSource(list1[lines_month]))
  
  dtm <- DocumentTermMatrix(corpus)
  
  matriz <- as.data.frame(as.matrix(dtm))
  
  tf<- 1+ log(colSums(matriz))
  Tv <- nrow(matriz)- apply(matriz, 2, contar_zeros)
  Tv <- nrow(matriz)- apply(matriz, 2, contar_zeros)
  idf<-log(nrow(matriz)/Tv)
  tf_idfv<-tf * idf
  
  tf_idfv <- sort(tf_idfv, decreasing = TRUE) 
  
  max<-0.5*ncol(matriz)
  
  max<-as.integer(max)
  if(which( names(tf_idfv) == "\"prisão\",") < max | which( names(tf_idfv) == "\"prender\",") < max){
    cols<-which(colnames(matriz) %in% names(tf_idfv[1:max])) 
    
    matriz<-matriz[,cols]
    
    
  }else{
    cols<-which(colnames(matriz) %in% names(tf_idfv[1:max]))
    cols<-c(cols, which( names(tf_idfv) == "\"prisão\","), which( names(tf_idfv) == "\"prender\",")  )
    matriz<-matriz[,cols]
  }
  
  dtm_sparse <- as(as.matrix(matriz), "CsparseMatrix")
  dtm_tot<- c(dtm_tot, dtm_sparse)
  # Combine as duas DTMs usando merge
  dtm_tot <- merge_dtm(dtm_tot)
}
  
   
}

      # Defina o número de tópicos desejados
      num_topics <- 30
 ##################### identificação###############
      
      
      # Crie o modelo LDA
      lda_model <- LDA(dtm, k = num_topics)
      lda_model2 <- LDA(dtm2, k = num_topics)
      
      # Imprima as palavras mais frequentes em cada tópico
      terms(lda_model, 15)
      
      
      # Carregue os pacotes necessários
      library(wordcloud)
      library(RColorBrewer)
      
      # Obtenha as palavras-chave mais importantes para cada tópico
      palavras_chave_por_topico <- terms(lda_model, 50)  # 20 é o número de palavras-chave a serem exibidas por tópico
      
      # Crie uma nuvem de palavras para cada tópico
      par(mfrow = c(2, 2))  # Defina o número de colunas e linhas para organizar as nuvens de palavras
      dev.off()
      for (i in 5:8) {  
        # Ajuste o número de tópicos a serem exibidos
        palavra_chave_titulo <- paste("Tópico", i)
        wordcloud(words = palavras_chave_por_topico[, i], freq = lda_model@gamma[1:50, i ], scale = c(3.5, 0.25),
                  min.freq = 2, max.words = 50, random.order = FALSE, rot.per = 0.3,
                  colors = brewer.pal(8, "Dark2"), main = palavra_chave_titulo)
      }
     
       lda_model$beta
      save.image("news.Rdata")   
      