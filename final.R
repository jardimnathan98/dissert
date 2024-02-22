# procurar baumer de novo
# odbe -> braskem
#qg-> enauta
#ag e camargo correa-> ccr vendem em 20-21
# alpargatas -> camargo corre e dps jbs 23/11/2015
#ivespar era da oas, olhar data 'IVPR4B.SO , não tem preços
#CRT Concessionaria Rio Teresopolis SA em carioca engenhario e queiroz galvao CRTE5.SA, não tem preços
# caixa n era listado durante a lava jato
ab<-which(is.na(returns$`Investimentos e Participacoes em Infraestrutura SA INVEPAR`))
setwd("C:/Users/Nathan/Downloads/dissertacoes")
library(tidytext)
library(arrow)
library(stringr)
library(dplyr)
library(tokenizers)
library(udpipe)
library(tm)
library(lubridate)
library(tm)
library(topicmodels)
library(readxl)
load("final.RData")
#setwd("/home/nathan/Downloads/disertacao")
data <- arrow::read_parquet("news.parquet")
linhas_contendo_ccr <- data %>%
  filter_all(any_vars(grepl("ccr", ., ignore.case = TRUE))) 

linhas_contendo_oas <- data %>%
  filter_all(any_vars(grepl(" oas ", ., ignore.case = TRUE))) 

linhas_contendo_oas <- linhas_contendo_oas %>%
  filter_all(any_vars(grepl(" ações ", ., ignore.case = TRUE))) 

linhas_contendo_ag_ccr <- linhas_contendo_ccr %>%
  filter_all(any_vars(grepl("andrade gutierrez", ., ignore.case = TRUE))) 

linhas_contendo_lava <- data %>%
  filter_all(any_vars(grepl("lava jato", ., ignore.case = TRUE))) 

linhas_contendo_brf <- linhas_contendo_lava  %>%
  filter_all(any_vars(grepl("brf", ., ignore.case = TRUE))) 

linhas_contendo_cvm <- data %>%
  filter_all(any_vars(grepl("cvm", ., ignore.case = TRUE))) 

linhas_contendo_anp <- data %>%
  filter_all(any_vars(grepl(" anp ", ., ignore.case = TRUE))) 

linhas_contendo_cade <- data %>%
  filter_all(any_vars(grepl(" cade ", ., ignore.case = TRUE))) 

linhas_contendo_ibama <- data %>%
  filter_all(any_vars(grepl(" ibama ", ., ignore.case = TRUE))) 
#ibama
#agencia de energia
#linhas_contendo_aneel <- data %>%
#  filter_all(any_vars(grepl(" aneel ", ., ignore.case = TRUE))) #agencia de energia
#ministerio do trabalho
linhas_contendo_MPT <- data %>%
  filter_all(any_vars(grepl(" mpt ", ., ignore.case = TRUE))) #ministerio do trabalho






#################



shares<- read.csv2("Cópia de all br comps shares.csv")
prices<-read.csv2("Cópia de all br comps ordinary share.csv")
book<-read_excel("Cópia de book to market.xlsx")
comps<-prices[,1:11]
indiciadas<-comps[c(14,39,54, 67,71,75, 142,145,151,161,216,285,292),1]
sectors<-read_excel("sectors_B3.xlsx")


################size
prices<-prices[-(1:8),-(1:12)]
prices<-prices[-(2),]
date_prices<-as.Date(prices[,1], format = "%d/%m/%Y")
colnames(prices)<-c("Date", prices[1,-1])
prices<-prices[-(1),]
prices[,1]<-date_prices[-1]

# Aplicar a substituição para todas as colunas do data frame
for( i in 2:ncol(prices)){
  if(prices[1,i]!= "#N/A"){
    prices[,i] <- gsub(",", ".", prices[,i])
  }
}

## concertar dias faltantes
for (i  in 2:ncol(prices)) {
  a<-as.numeric(prices[,i])
b<-which(is.na(a))
print(i)
while (sum(b[-1]-lag(b)[-1]) != length(b)-1) {
  a[b]<-a[b+1]
  b<-which(is.na(a))
  if(length(b) == 0){
    b<-c(1,2)# só para sair do looping
  }
}
prices[,i]<-a
}
  


a<-which(month(prices[,1]) == 7 & day(prices[,1]) ==31 | month(prices[,1]) == 8 & day(prices[,1]) ==1 | month(prices[,1]) == 8 & day(prices[,1]) ==2)

b<-c(1, diff(a) != 1)
b<-which(b==1)
a<-a[b]
prices_ff<-prices[a,]

#mexer com shares
# Crie a série de tempo
data_inicial <- as.Date("2008-07-04")
data_final <- as.Date("2022-07-04")
date_shares<-  seq(data_inicial, data_final, by = "1 month")

shares<-shares[-(1:3),13:14]
count<-table(shares[,1])
incomp<-which(count< 168)# alguns vieram sem nada do reuters
count<-as.data.frame(count)
count<-as.data.frame(count)
incomp<-count[incomp,]

incomp<-which(shares[,1] %in% incomp[,1])
shares<-shares[-incomp,]
date_shares<-date_shares[-169]
date_shares<-as.data.frame(date_shares)
shares_panel<-date_shares
for( i in 1: (nrow(shares)/nrow(date_shares))){
  date_shares<-cbind(date_shares, shares[(1 +(i-1)*nrow(date_shares) ): (i*nrow(date_shares)),])
}

#View(shares[169:336,])

names<-date_shares[1,seq(2,ncol(date_shares),2)]
date_shares<-date_shares[,-seq(2,ncol(date_shares),2)]

names<-which(comps[,2]%in% names)

colnames(date_shares)<-c("Date", comps[names,1])
date_shares_ff<-date_shares[seq(1, nrow(date_shares), 12),]# pegar 1 por ano
date_shares_ff<-rbind(date_shares_ff, date_shares[nrow(date_shares),]) # botar o ultimo

date_shares_ff <- date_shares_ff[rev(seq_len(nrow(date_shares_ff))), ]
a<-which(year(prices_ff[,1])%in% year(date_shares_ff[,1]))
prices_ff<-prices_ff[a,]
size<-date_shares_ff
# botar prices com os que tem shares
names1<-colnames(date_shares_ff)
names2<-colnames(prices_ff)
a<-which(names2%in% names1)
prices_ff<-prices_ff[,a]

#botar em numerico
for(i in 2:ncol(date_shares_ff)){
  size[,i]<-as.numeric(date_shares_ff[,i])*as.numeric(prices_ff[,i])#olhar os que tiveram NA no meio
}



# criar os high e small


############# book to market
data_inicial <- as.Date("2008-07-04")
data_final <- as.Date("2023-07-04")
date_book<-  seq(data_inicial, data_final, by = "year")
book<-book[-(1:3),13:15]
book<-book[1:6085,]
count<-table(book[,1])
incomp<-which(count< 16)# alguns vieram sem nada do reuters
count<-as.data.frame(count)
count<-as.data.frame(count)
incomp<-count[incomp,]
incomp<-which(t(as.data.frame(book[,1])) %in% incomp[,1])
book<-book[-incomp,]
date_book<-as.data.frame(date_book)
book_panel<-date_book
for( i in 1: (nrow(book)/nrow(date_book))){
  date_book<-cbind(date_book, book[(1 +(i-1)*nrow(date_book) ): (i*nrow(date_book)),])
}

names<-date_book[1,seq(2,ncol(date_book),3)]
date_book<-date_book[,-seq(2,ncol(date_book),3)]
names<-which(comps[,2]%in% names)
names<-comps[names,1]
price_to_book<-date_book[,seq(1,ncol(date_book),2)]
names_duplicado <-names[c(rep(seq_along(names), each = 2))]# 0: Book Value Per Share, 1:Price To Book Value Per Share (Daily Time Series Ratio)
colnames(date_book)<-c("Date",names_duplicado)

colnames(price_to_book)<-c("Date",names)

price_to_book <- price_to_book[rev(seq_len(nrow(price_to_book))), ]
price_to_book<-price_to_book[-1,]




############ fazer os portfolios( so fazer as separações que acho os fatores)
big_low_port<-list()
big_med_port<-list()
big_high_port<-list()

small_low_port<-list()
small_med_port<-list()
small_high_port<-list()

for(i in 1:nrow(size)){# vai de 22 a 08
  mediana<-size[i,-1]
  sep<-price_to_book[i,-1]
off<-which(is.na(mediana))
mediana<-mediana[,-off] 
off<-which(sep == "NULL")
sep<-sep[,-off] 
### garantir que não tem nada perdido
names1<-colnames(mediana)
names2<-colnames(sep)
names3<-which(names1%in%names2)
names4<-which(names2%in%names1)
mediana<-mediana[names3]
sep<-sep[names4]
med<-median(as.numeric(mediana))

points_SB<-which(as.numeric(mediana)>=med)
big<-mediana[,points_SB]
small<-mediana[,-points_SB]

# Calcular a terça parte usando o quantile
third_L<- quantile(as.numeric(big), 0.3333)
third_H <- quantile(as.numeric(big), 0.6666)
points_BL<-which(as.numeric(big)<=third_L)
points_BH<-which(as.numeric(big)>=third_H)
BIG_LOW<-big[,points_BL]
BIG_HIGH<-big[,points_BH]
BIG_MEDIUM<-big[,-c(points_BH, points_BL)]

third_L<- quantile(as.numeric(small), 0.3333)
third_H <- quantile(as.numeric(small), 0.6666)

points_SL<-which(as.numeric(small)<=third_L)
points_SH<-which(as.numeric(small)>=third_H)
SMALL_LOW<-big[,points_SL]
SMALL_HIGH<-big[,points_SH]
SMALL_MEDIUM<-big[,-c(points_SH, points_SL)]

big_low_port<-append(big_low_port,list(colnames(BIG_LOW)))
big_med_port<-append(big_med_port,list(colnames(BIG_MEDIUM)))
big_high_port<-append(big_high_port,list(colnames(BIG_HIGH)))

small_low_port<-append(small_low_port,list(colnames(SMALL_LOW)))
small_med_port<-append(small_med_port,list(colnames(SMALL_MEDIUM)))
small_high_port<-append(small_high_port,list(colnames(SMALL_HIGH)))
}
################# construir os fatores dado que agora tenho já qual asset é em cada período
#primeiro transformo prices em retornos
returns<-prices
for(i in 2:ncol(returns)){
    intermed<-returns[-1,i]
    intermed2<-lag(returns[,i])
    intermed2<-intermed2[-1]
    which(is.na(as.numeric(intermed2)))
    returns[-nrow(returns),i]<-(as.numeric(intermed2) - as.numeric(intermed))/as.numeric(intermed)
    
}
returns<-returns[-(1:93),]
## Fazer os fatores

length(small_low_port)
SMB<-returns[,1:2]
SMB[,2]<-0
colnames(SMB)<-c("Date", "fator")

HML<-returns[,1:2]
HML[,2]<-0
colnames(HML)<-c("Date", "fator")

dates<-date_book[,1]

dates<-rev(dates)

ret_port<-returns[,1:7]
ret_port[,2:7]<-0
colnames(ret_port)<-c("Date", "BL", "BM", "BH", "SL", "SM", "SH")
  # talvez seja bom olhar com mais calma se teve IPO no meio
for(i in 1:length(small_low_port)){
  
pos<-which(returns[,1]<=dates[i] & returns[,1]>=dates[i+1])# ideia, o primeiro é 23 e o segundo 22 e o portflio roda de 22 a 23
ret_pos<-returns[pos,]
ret_pos <- replace(ret_pos, is.na(ret_pos), 0)# como os portfolios são anuais, apenas estou resolvendo problemas internos deles
namesBH<-which(t(as.data.frame(colnames(ret_pos))) %in% t(as.data.frame(big_high_port[i])))
ret_pos_BH<-ret_pos[,namesBH]

namesBM<-which(t(as.data.frame(colnames(ret_pos))) %in% t(as.data.frame(big_med_port[i])))
ret_pos_BM<-ret_pos[,namesBM]

namesBL<-which(t(as.data.frame(colnames(ret_pos))) %in% t(as.data.frame(big_low_port[i])))
ret_pos_BL<-ret_pos[,namesBL]

namesSH<-which(t(as.data.frame(colnames(ret_pos))) %in% t(as.data.frame(small_high_port[i])))
ret_pos_SH<-ret_pos[,namesSH]

namesSM<-which(t(as.data.frame(colnames(ret_pos))) %in% t(as.data.frame(small_med_port[i])))
ret_pos_SM<-ret_pos[,namesSM]

namesSL<-which(t(as.data.frame(colnames(ret_pos))) %in% t(as.data.frame(small_low_port[i])))
ret_pos_SL<-ret_pos[,namesSL]

namesBL<-which(t(as.data.frame(colnames(size))) %in% t(as.data.frame(big_low_port[i])))
weightBL<-size[i,namesBL]
weightBL<-weightBL/sum(weightBL)

namesBM<-which(t(as.data.frame(colnames(size))) %in% t(as.data.frame(big_med_port[i])))
weightBM<-size[i,namesBM]
weightBM<-weightBM/sum(weightBM)

namesBH<-which(t(as.data.frame(colnames(size))) %in% t(as.data.frame(big_high_port[i])))
weightBH<-size[i,namesBH]
weightBH<-weightBH/sum(weightBH)

namesSL<-which(t(as.data.frame(colnames(size))) %in% t(as.data.frame(small_low_port[i])))
weightSL<-size[i,namesSL]
weightSL<-weightSL/sum(weightSL)

namesSM<-which(t(as.data.frame(colnames(size))) %in% t(as.data.frame(small_med_port[i])))
weightSM<-size[i,namesSM]
weightSM<-weightSM/sum(weightSM)

namesSH<-which(t(as.data.frame(colnames(size))) %in% t(as.data.frame(small_high_port[i])))
weightSH<-size[i,namesSH]
weightSH<-weightSH/sum(weightSH)

ret_port[pos,2]<-as.matrix(ret_pos_BL)%*%t(weightBL)
ret_port[pos,3]<-as.matrix(ret_pos_BM)%*%t(weightBM)
ret_port[pos,4]<-as.matrix(ret_pos_BH)%*%t(weightBH)

ret_port[pos,5]<-as.matrix(ret_pos_SL)%*%t(weightSL)
ret_port[pos,6]<-as.matrix(ret_pos_SM)%*%t(weightSM)
ret_port[pos,7]<-as.matrix(ret_pos_SH)%*%t(weightSH)
}

SMB[,2]<-(ret_port[,2] + ret_port[,3] + ret_port[,4] - ret_port[,5] - ret_port[,6] - ret_port[,7])/3
HML[,2]<-(ret_port[,4] + ret_port[,7] - ret_port[,2] - ret_port[,5])/2


################################## Eventos #####################################
##################################### revisar os casos
lava_jato_ind<-read.csv2("lava jato ind3.csv")[1:313,]
lava_jato_ind2<-read_excel("lava jato indic2.xlsx")[1:313,]
lava_jato_ind<-cbind(lava_jato_ind[,2], lava_jato_ind2[,-(1:2)])
cvm_ind<- read_excel("Cópia de tabela_ind2.xlsx",sheet = "Planilha1", col_names = F)
anp_ind<- read_excel("Cópia de tabela_ind2.xlsx",sheet = "Planilha2", col_names = F)
a<-which(is.na(anp_ind[,1]))
anp_ind<-anp_ind[-a,]
cade_ind<- read_excel("Cópia de tabela_ind2.xlsx",sheet = "Planilha3", col_names = F)
ibama_ind<- read_excel("Cópia de tabela_ind2.xlsx",sheet = "Planilha4", col_names = F)
mpt_ind<- read_excel("Cópia de tabela_ind2.xlsx",sheet = "Planilha6", col_names = F)


linhas_contendo_MPT_ind<-linhas_contendo_MPT[as.numeric(t(mpt_ind[,1])),]
linhas_contendo_MPT_ind$firm<-(mpt_ind[,2])

linhas_contendo_anp_ind<-linhas_contendo_anp[as.numeric(t(anp_ind[,1])),]
linhas_contendo_anp_ind$firm<-(anp_ind[,2])

linhas_contendo_cade_ind<-linhas_contendo_cade[as.numeric(t(cade_ind[,1])),]
linhas_contendo_cade_ind$firm<-(cade_ind[,2])
linhas_contendo_ibama_ind<-linhas_contendo_ibama[as.numeric(t(ibama_ind[,1])),]
linhas_contendo_ibama_ind$firm<-(ibama_ind[,2])

linhas_contendo_cvm_ind<-linhas_contendo_cvm[as.numeric(t(cvm_ind[,8])),]
linhas_contendo_cvm_ind$firm<-(cvm_ind[,3])
for(i in 1:nrow(linhas_contendo_cvm_ind)){
  if(linhas_contendo_cvm_ind$)
}

#### arrumar ibov
ibov<-read.csv2("ibo diario.csv")
colnames(ibov)<-c("Date",ibov[1,-1])
ibov<-ibov[-1,]
date<-as.Date(ibov[,1],format = "%d/%m/%Y")
ibov[,1]<-date

ibov <- ibov[rev(seq_len(nrow(ibov))), ]


returns<-left_join(returns, ibov, by = "Date")
returns$retornos <- gsub(",", ".", returns$retornos)
rf<-read.csv2("bonds daily.csv")
rf<-rf[-(1:26),-(1:2)]
colnames(rf)<-rf[1,]

rf<-rf[-1,]
rf<-rf[,c(1,4)]

rf[,2]  <- gsub(",", ".", rf[,2]  ) 

returns$r_f<-0

pos_rf<-which( as.Date(rf[,1], format = "%d/%m/%Y" ) %in% returns$Date )
pos_rt<-which( returns$Date %in%  as.Date(rf[,1], format = "%d/%m/%Y" ))


returns$r_f[pos_rt]<-rf[pos_rf,2]

returns$r_f<- as.numeric(returns$r_f)/100

returns$r_f<- (1 + as.numeric(returns$r_f))^(1/252) - 1 # tirando do anualizado
returns$retornos<- (1+as.numeric(returns$retornos))/(1+returns$r_f)-1 
port_lava2<-c(comps[67,1], comps[151,1], comps[78,1] )# desconsidero alpargatas
port_lava2<-which(colnames(returns) %in% port_lava2)
port_lava2<-returns[,c(1, port_lava2)]

##################### construir ipca
ipca<-read_excel("ipca.xlsx", sheet = "First Release Data")
ipca<-ipca[3:148,]
datas <- seq(as.Date("01-01-2024", format = "%d-%m-%Y"), as.Date("01-01-2012", format = "%d-%m-%Y"), by = "-1 month")
ipca<-cbind(datas, ipca[-1,-1])
ipca$acc<-0
ipca[,3]<- (1 +as.numeric(ipca[,3])/100)^(1/12) - 1
for (i in 1:nrow(ipca)) {
  ipca[i,4]<- prod((1+ as.numeric(ipca[i:nrow(ipca),3]))) -1
  
}

##########################

####################
############# pegar datas da lava jato
timeline<-c()
padrao_data <- "\\d{2}/\\d{2}/\\d{4}"

datas_encontradas <- grep(padrao_data, lava_jato_ind[,1], value = TRUE)
datas_encontradas <-  which(lava_jato_ind[,1] %in% datas_encontradas)
lava_jato_ind_tot <- lava_jato_ind[datas_encontradas,]

firms_lava<-c("odbe", "qg", "camargo correa", "andrade gutierrez")
firms_lava2<-c(comps[67,1], comps[151,1], comps[78,1], comps[78,1]  ) # desconsidero alpargatas
########## for começa aqui
#olhar transpetro
evento<- 121
{
   valores <- unlist(strsplit(lava_jato_ind_tot[evento,3], ","))
   #valores<-valores[1]
     # Remover espaços e converter para minúsculas
     valores <- trimws(valores) # o+ não saia
   
     
     # Remover símbolos da string
     valores <- trimws(gsub("[^[:alnum:]]", " ", valores))
   a<-which(valores == "transpetro")
   if(length(a)>0){
     valores[a]<- lava_jato_ind_tot[2,3]
   }
     
   indiciadas<-which(tolower(trimws(gsub("[^[:alnum:]]", " ", comps[,1]))) %in% tolower(valores) )
   indiciadas2<-which(firms_lava %in% valores )
   indiciadas2<-firms_lava2[indiciadas2]
   indiciadas2<-which(comps[,1] %in% indiciadas2 )
   indiciadas<-c(indiciadas, indiciadas2)
   date<-lava_jato_ind_tot[evento,1]
  if( year(as.Date(date, format = "%d/%m/%Y")) <2018 & 39 %in% indiciadas ){
   a<- which(indiciadas ==39)
   indiciadas[a]<- c(48, 155)
   }
   
     if(length(indiciadas) > 0){
         
       returns_ind<-returns[, c(1, indiciadas+1, 402,403)]
         
           weight_ind<-which(colnames(size) %in% colnames(returns[indiciadas+1])  )
           weight_ind<-size[, c(1,weight_ind)]
           inter<-weight_ind
           if(ncol(weight_ind)>2){
for(i in 2:ncol(weight_ind) ){
                   inter[,i]<- weight_ind[,i]/(rowSums(weight_ind[,-1]))
                 }
               weight_ind<-inter
            } 
          if(ncol(weight_ind)==2){
            weight_ind[,2]<- 1
             }
           
             
             date<-lava_jato_ind_tot[evento,1]
             returns_port<-returns_ind[,-c((ncol(returns_ind)-1), ncol(returns_ind) ) ]  
             returns_ind$port<-0
             for (i in 1:nrow(weight_ind)) {
                 pos<-which(returns_port[,1]<=weight_ind[i,1] & returns_port[,1]>=weight_ind[i+1,1]) 
                 port<-c()
                if(ncol(returns_port)>2){
                     for (j  in 2:(ncol(returns_port)-1)) {
                         port1<- returns_port[pos,j] *(weight_ind[i+1,j]) 
                         port<-cbind(port,port1)
                       }
                     port<-rowSums(port)
                   } else{
                       port<- returns_port[pos,2] *(weight_ind[i+1,2]) 
                     }
                 
                   returns_ind$port[pos]<- port
                   
                  }
             
               ####### faço a regressão que já tenho.
              # retiro dias que a bolsa fechou e teve erro
               na<-which(is.na(returns_ind$retornos))
               returns_ind<-returns_ind[-na,]
               
                 reg_ind<-which(returns_ind$Date == as.Date(date, format = "%d/%m/%Y"))
                 if(length(reg_ind) == 0){
                     reg_ind<-which(returns_ind$Date == as.Date(date, format = "%d/%m/%Y") -1)
                   }
                 reg_ind<-rbind(returns_ind[(reg_ind-5):(reg_ind+5),], returns_ind[(reg_ind+60):(reg_ind+60+504),] )
                 
                   tam<-ncol(reg_ind)
                   d<-matrix(0,nrow = nrow( reg_ind), ncol =11)
                   reg_ind<-cbind(reg_ind,d)
                   names<-c("d5", "d4","d3", "d2", "d1", "d0", "d_1", "d_2", "d_3", "d_4", "d_5")
                   
                     colnames(reg_ind)<-c(colnames( reg_ind[,1:tam]), names   )
                      reg_ind$retornos<-(1+as.numeric(reg_ind$retornos))/(1+reg_ind$r_f)
                      reg_ind$port<-(1+as.numeric(reg_ind$port))/(1+reg_ind$r_f)
                     for(j in 1:11){
                        
                           reg_ind[j,tam+j]<-1
                           
                          }
                    reg<-lm(port ~ retornos +d5 + d4 + d3 + d2 + d1 + d0 +  d_1 + d_2 +  d_3 + d_4 + d_5 , data = reg_ind)
                     sumar<-summary(reg)
                      ## alternativo fazer da maneira tradicional
                       reg2<-lm(port ~ retornos , data = reg_ind)
                       abn<-reg_ind[1:11, 1:(tam+1)]
                       abn[,tam+1]<- abn$port-(reg2$coefficients[1] + reg2$coefficients[2]*abn$retornos)
                     
                         ########## vou olhar as imagens um por um para ter intuição
                         abn2<-reg$coefficients[-(1:2)]
                         plot(seq(-5,5,1),abn2,)
                         # vamos fazer 2 meses antes
                           
                           #######3 fatores
                           
                           reg_ind<-left_join(reg_ind, SMB, by = "Date")
                           reg_ind<-left_join(reg_ind, HML, by = "Date")
                           colnames(reg_ind)<-c(colnames(reg_ind[,1:(ncol(reg_ind)-2)]), "SMB", "HML")
                           reg3<-lm(port ~ retornos+ SMB + HML +d5 + d4 + d3 + d2 + d1 + d0 +  d_1 + d_2 +  d_3 + d_4 + d_5 , data = reg_ind)
                           summary(reg3)
                           
                            }
   }
###########olhar os casos para ter intuição

########## após olhar, faço os SCAR


#save.image("final.RData")
