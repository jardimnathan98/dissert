#setwd("/home/nathan/Downloads/disertacao")
setwd("C:/Users/Nathan/Downloads/dissertacoes/disertacao")
library(stargazer)
library(tidyr)
library(stringr)
library(dplyr)
library(lubridate)
library(readxl)
eventos_panel<-read.csv("eventos_panel2.csv")[-1,-1]
eventos_panel$janela<-eventos_panel$janela * (-1)

eventos_panel_ind<-read.csv("eventos_ind.csv")[-1,-1]
eventos_panel$janela<-eventos_panel$janela * (-1)

ibov<-read.csv("ibov diario.csv")
petrobras<-read.csv("petrobras.csv")
eletrobras<-read.csv("petrobras.csv")
ogx<-read.csv("ogx.csv")
ogx<-ogx[-(1:256),]# nao dar incositencia temporal
mendes<-read.csv("mendes.csv")
mendes<-mendes[(1:2348),]# nao dar incositencia temporal
qg<-read.csv("enauta.csv")
ecorod<-read.csv("ecorod.csv")
jurong<-read.csv('jurong.csv')
caixa<-read.csv('caixa.csv')
maersk<-read.csv('maersk.csv')
tenaris<-read.csv('tenaris.csv')
saipem<-read.csv('saipem.csv')
bb<-read.csv('bb.csv')
odbe<-read.csv('odbe.csv')
linha<-which(odbe[,2] == "30/12/2021")
odbe<-odbe[-(1:linha),]
linha<-which(odbe[,2] == "05/01/2010")
odbe<-odbe[(1:linha),]
########
ibov<-ibov[-(1:27),(1:10)]
colnames(ibov)<-ibov[1,]
date<-as.Date(ibov[,1],format = "%d-%b-%Y")
date_form<- format(date,format = "%d/%m/%Y")
ibov[,1]<-date_form
ibov<-ibov[-1,]

###agora é botar ibov e retornos dos bonds e fazer o capm  
qg<-qg[,-1]

eventos_panel$ibrx<- NA
eventos_panel_ind$ibrx<- NA
put_ibx<-function(eventos_panel, tipo){
  for(i in 1: nrow(eventos_panel)){
    linha <- which(ibov[,1] ==eventos_panel[i,5+ tipo])
    if (length(linha) == 0){
      print(i)
      linha <- which(as.Date(ibov[,1],format = "%d/%m/%Y") == as.Date(eventos_panel[i,5+ tipo], format = "%d/%m/%Y")-1 )
      
      
    } 
    if (length(linha) == 0){
      print(i)
      linha <- which(as.Date(ibov[,1],format = "%d/%m/%Y") == as.Date(eventos_panel[i,5+ tipo], format = "%d/%m/%Y")-2 )
      
      
    } 
    if (length(linha) == 0){
      print(i)
      linha <- which(as.Date(ibov[,1],format = "%d/%m/%Y") == as.Date(eventos_panel[i,5+ tipo], format = "%d/%m/%Y") -3)
      
      
    } 
    
    eventos_panel$ibrx[i]<-ibov$`%Chg`[linha]
    
  }
  
  return(eventos_panel)
}
 
eventos_panel<-put_ibx(eventos_panel, 0)
eventos_panel_ind<-put_ibx(eventos_panel_ind, 1)

eventos_panel$ibrx<- as.numeric(gsub(",", ".", gsub("%", "", eventos_panel$ibrx)) )
eventos_panel_ind$ibrx<- as.numeric(gsub(",", ".", gsub("%", "", eventos_panel_ind$ibrx)) )

############################### unir dados com ibov para regressao
library(dplyr)
colnames(ibov)<-colnames(petrobras[,-1])




join_firma<-function(empresa){
    window<-left_join(empresa[-1,c(2,5)], ibov[,c(1,4)], by= "Exchange.Date")

  
    ######## concertar os que faltam
  na_posit<-is.na(window[,c(1,3)])
  rows_with_na<-window[,c(1,3)][rowSums(na_posit) > 0, ]
  for (i in 1:nrow(rows_with_na)) {
    line1<-  which(as.Date(ibov[,1],format = "%d/%m/%Y") == as.Date(rows_with_na[i,1], format = "%d/%m/%Y") -1)
    if(length(line1) == 0){
      line1<-  which(as.Date(ibov[,1],format = "%d/%m/%Y") == as.Date(rows_with_na[i,1], format = "%d/%m/%Y") -2)
    }
    if(length(line1) == 0){
      line1<-  which(as.Date(ibov[,1],format = "%d/%m/%Y") == as.Date(rows_with_na[i,1], format = "%d/%m/%Y") -3)
    }
    if(length(line1) == 0){
      line1<-  which(as.Date(ibov[,1],format = "%d/%m/%Y") == as.Date(rows_with_na[i,1], format = "%d/%m/%Y") -4)
    }
    if(length(line1) == 0){
      line1<-  which(as.Date(ibov[,1],format = "%d/%m/%Y") == as.Date(rows_with_na[i,1], format = "%d/%m/%Y") -5)
    }
    line2<-  which(as.Date(window[,1],format = "%d/%m/%Y") == as.Date(rows_with_na[i,1], format = "%d/%m/%Y"))
    
    window[line2,3]<-ibov$X.Chg[line1]
  }
  
  window[,3]<- gsub(",", ".", gsub("%", "",window[,3] )) 
  window[,2]<- gsub(",", ".", gsub("%", "",window[,2] )) 
  window[,2]<-as.numeric(  window[,2])/100
 window[,3]<-as.numeric(  window[,3])/100
  
colnames(window)<-c("date", "firm", "ibov")
window[is.na(window)] <- 0
return(window)

    }
petrobras_window<-join_firma(petrobras)

eletrobras_window<-join_firma(eletrobras)

qg_window<-join_firma(qg)

mendes_window<-join_firma(mendes)#olhar e ogx tbm
ogx_window<-join_firma(ogx)
odbe_window<-join_firma(odbe)

################# concertar eventos_panel

eventos_panel$X.Chg  <- gsub(",", ".", gsub("%", "",eventos_panel$X.Chg  )) 
eventos_panel$X.Chg<-as.numeric( eventos_panel$X.Chg)/100

eventos_panel$ibrx  <- gsub(",", ".", gsub("%", "",eventos_panel$ibrx  )) 
eventos_panel$ibrx<-as.numeric( eventos_panel$ibrx)/100


eventos_panel$ibrx<- ifelse(is.na(eventos_panel$ibrx), 0, eventos_panel$ibrx)
eventos_panel$X.Chg<- ifelse(is.na(eventos_panel$X.Chg), 0, eventos_panel$X.Chg)

eventos_panel_ind$X.Chg  <- gsub(",", ".", gsub("%", "",eventos_panel_ind$X.Chg  )) 
eventos_panel_ind$X.Chg<-as.numeric( eventos_panel_ind$X.Chg)/100

eventos_panel_ind$ibrx  <- gsub(",", ".", gsub("%", "",eventos_panel_ind$ibrx  )) 
eventos_panel_ind$ibrx<-as.numeric( eventos_panel_ind$ibrx)/100


eventos_panel_ind$ibrx<- ifelse(is.na(eventos_panel_ind$ibrx), 0, eventos_panel_ind$ibrx)
eventos_panel_ind$X.Chg<- ifelse(is.na(eventos_panel_ind$X.Chg), 0, eventos_panel_ind$X.Chg)

b<-which(eventos_panel$janela == 0)


eventos_std<-eventos_panel[b+1,]



for (i in  1:(49)) {
  if(as.Date(eventos_std$Exchange.Date[i],format = "%d/%m/%Y") - as.Date(eventos_std$Exchange.Date[i+1], format = "%d/%m/%Y") <8 )
    eventos_std<- eventos_std[-i,]
  
}






index<-which( eventos_panel$evento %in% eventos_std$evento  )

eventos_panel<-eventos_panel[index,]
eventos_std<-eventos_std[1:49, c(1, 5)]
eventos_std<-eventos_std[-(35:46), ]

################## agora é fazer a regressão da janela

eventos_panel$alpha<-0

eventos_panel$beta<-0
eventos_panel$sigma2<-0
SIGMA<-matrix(0, ncol = nrow(eventos_panel), nrow= nrow(eventos_panel))
reg_events<-function(firm, firm_window, eventos_panel, SIGMA){
#firm<-"petrobras"
#firm_window<-petrobras_window     
 res<-list()
i <- which(grepl(firm, eventos_panel[,1]))[1]

   while (eventos_panel[i,1] ==firm) {
  if(eventos_panel[i,3] == 0){
   # i<-5
    line<-which(as.Date(firm_window[,1],format = "%d/%m/%Y") == as.Date(eventos_panel[i,5], format = "%d/%m/%Y"))
    
   df<-firm_window[((line+90):(line +360)),]
 
    reg<-lm(firm ~ ibov, data = df)
    coef<-reg$coefficients
    res1<-list(reg$residuals)
    names(res1[[1]])<-as.character(df$date)
   
     res<-append(res,res1 )
    sigma2<-t(reg$residuals)%*%reg$residuals/(nrow(df) -2)

     window_event<-seq((i-3),i+4,1)

for(j in 1:length(window_event)){
  SIGMA[window_event[j],window_event[j]]<-sigma2
}
    
     
     eventos_panel$alpha[ window_event]<-coef[1]
     eventos_panel$beta[ window_event]<-coef[2]
     eventos_panel$sigma2[ window_event]<-sigma2
     }
  
  i<-i+1
  
}
ret<-list(eventos_panel, SIGMA, res )

return(ret )
}

########### rodar o looping para eventos aqui


eventos_panel<-reg_events("eletrobras", eletrobras_window, eventos_panel, SIGMA)[[1]]
eventos_panel<-reg_events("petrobras", petrobras_window, eventos_panel, SIGMA)[[1]]
eventos_panel<-reg_events("ogx", ogx_window, eventos_panel, SIGMA)[[1]]
eventos_panel<-reg_events("queiroz galvao", qg_window, eventos_panel, SIGMA)[[1]]

SIGMA<-reg_events("eletrobras", eletrobras_window, eventos_panel, SIGMA)[[2]]
SIGMA<-reg_events("petrobras", petrobras_window, eventos_panel, SIGMA)[[2]]
SIGMA<-reg_events("ogx", ogx_window, eventos_panel, SIGMA)[[2]]
SIGMA<-reg_events("queiroz galvao", qg_window, eventos_panel, SIGMA)[[2]]

res<-list()
res<-reg_events("petrobras", petrobras_window, eventos_panel, SIGMA)[[3]]
res<-c(res, reg_events("ogx", ogx_window, eventos_panel, SIGMA)[[3]] )


eventos_panel$abnormal<-0

eventos_panel$abnormal <- eventos_panel$X.Chg - (eventos_panel$beta * eventos_panel$ibrx + eventos_panel$alpha)


########################resolvendo overlapping e ic#####################


ic_CAR<-function(firms, eventos_panel){
  firms<-c("petrobras", "ogx", "queiroz galvao")
  eventos_int<-which(eventos_panel$empresa %in% firms)
  
  eventos_int<-eventos_panel[eventos_int,]
  datas_iguais <- eventos_int$Exchange.Date[duplicated(eventos_int$Exchange.Date)]
  
  for(k in 1:length(datas_iguais)){
    eventos_ig<-which(eventos_int$Exchange.Date %in% datas_iguais[1])# some evento 38 
    for(j in 1: length( eventos_ig)){
      res1<-res[[eventos_int$evento[eventos_ig[1]]]]
      res2<-res[[eventos_int$evento[eventos_ig[2]]]]
      if(legth(eventos_ig) >2){
        res3<-res[[eventos_int$evento[eventos_ig[3]]]]
      }
      
      
    }
    nomes_comuns <- intersect(names(res1), names(res2))
    SIGMA[eventos_ig[1], eventos_ig[2]]<-  cov(res1[nomes_comuns] , res2[nomes_comuns])
     }
  
  ######### resolver o resto do teste t aqui
res[[eventos_int$evento[eventos_ig[1]]]]
  eventos_int$evento[eventos_ig[2]]
  
  eventos_ig<-eventos_int[eventos_ig,]

  
  reg<-lm(firm ~ ibov, data = df)
  coef<-reg$coefficients
  sigma2<-t(reg$residuals)%*%reg$residuals/(nrow(df) -2)
  
  index<-c()
  SCAR<-matrix(0, ncol = 2, nrow = 8)
  SCAR[,1]<- seq(-4,3,1)
  for(j in 0:7){
    index1<-which(eventos_int$janela == j-4)
    index<-c(index, index1)
    sum(eventos_int$abnormal[index])
    SCAR[j+1,2] <-sum(eventos_int$abnormal[index])/sum(eventos_int$sigma2[index]) 
    
  }
  
  
  return(SCAR)
}


############## agora os AR do cross section

table_events<-function(firm, eventos_panel){
  i <- which(grepl(firm, eventos_panel[,1]))
  eventos_int<-eventos_panel[i,]
  
  table<-matrix(0, ncol=2, nrow = 8)
 table[,1]<-seq(-3,4,1)*(-1)

     for (i in 0:7) {
       lines<-which(eventos_int[,3] == i-3)
       mean1<- mean(eventos_int$abnormal[lines])
    table[i,2]<-mean1
        }
  
       
   colnames(table)<-c("window", firm)
  table<-as.data.frame(table)
  return(table)
}



SCAR<-ic_CAR( c("petrobras", "ogx", "queiroz galvao"), eventos_panel)

######################### eventos juntos##############
datas_iguais <- eventos_panel$Exchange.Date[duplicated(eventos_panel$Exchange.Date)]


################### temos agora todos os retornos abnormais, agora façamos a parte com economia ou interza politica
table<-table_events("petrobras", eventos_panel)

table<-left_join(table, table_events("eletrobras", eventos_panel), by= "window" )
table<-left_join(table, table_events("ogx", eventos_panel), by= "window" )
table<-left_join(table, table_events("queiroz galvao", eventos_panel), by= "window" )


############ fazer com dummys
dados<-left_join(petrobras_window[,], ogx_window[,-3],  by = "date"  )
dados<-left_join(dados, qg_window[,-3] , by = "date")
dados<-left_join(dados, eletrobras_window[,-3] , by = "date")
dados<-left_join(dados, odbe_window[,-3] , by = "date")
a<- which(rowSums(is.na(dados)) >0)
dados<-dados[-a,]
colnames(dados)<-c("date","petrobras","ibov", "ogx", "queiroz galvao", "eletrobras", "odebrecht")

eve<-list()

retirar<-which(eventos_panel[,1] == " mendes junior")
eventos_panel<-eventos_panel[-retirar,]
retirar<-which(eventos_panel[,2] == 62 | eventos_panel[,2] == 63)
eventos_panel<-eventos_panel[-retirar,]
datas_iguais <- eventos_panel$Exchange.Date[duplicated(eventos_panel$Exchange.Date)]
eve<-list()
for(k in 1:length(datas_iguais)){
  eventos_ig<-which(eventos_panel$Exchange.Date %in% datas_iguais[k])# some evento 38 
  firms<-eventos_panel[eventos_ig,1]
lista<-eventos_panel[eventos_ig,2]
 eve[k]<-list(lista)
 
 }


eve<-unique(eve)

eve<-as.data.frame(eve)



eventos_std<-which(eventos_panel[,3] == 0)
eventos_std<- eventos_panel[eventos_std,]
ev<-unlist(eve)
ev<-unique(ev)
event<-c()
reg_dummie<-function(firma, firm_window, event){
 #firma<-"petrobras"
  eve_firm<- which(eventos_std[,1] == firma)
unicos <-setdiff(eve_firm , ev)
#firm_window<-petrobras_window
for(i in 1:length(unicos)){
  
  pos<-which(eventos_panel[,2] == unicos[i] & eventos_panel[,3]==0)
  if(length(pos)>0){
    line<-which(as.Date(firm_window[,1],format = "%d/%m/%Y") == as.Date(eventos_panel[pos,5], format = "%d/%m/%Y"))
    df1<-firm_window[((line-3):(line +4)),]
    df2<-firm_window[((line+90):(line +360)),]
    df<-rbind(df1, df2)
    d<-matrix(0,nrow = nrow(df), ncol =8)
    df<-cbind(df,d)
    d<-matrix(0,nrow = nrow(df), ncol =8)
    df<-cbind(df,d)
    names<-c("d3", "d2", "d1", "d0", "d_1", "d_2", "d_3", "d_4")
    
    colnames(df)<-c(colnames(df[,1:3]), names   )
    for(j in 1:8){
      
      df[j,3+j]<-1
      
    }
    
    reg<-lm(firm ~ ibov + d3 + d2 + d1 + d0 +  d_1 + d_2 +  d_3 + d_4 , data = df)
    sumar<-summary(reg)
    
    coef<-list(sumar$coefficients)
    res<-list(reg$residuals)
    reg_til<-lm(firm~ ibov, data = df1)
    res_til<-list(reg_til$residuals)
    lista<- list( coef, res, res_til)
    names(lista)<-paste0(c("coef", "res", "res_til"), paste0(firma,i))
    event<-c(event,lista )
    
    
  } 
  }
  
return(event)
}

event<-reg_dummie("petrobras", petrobras_window, event)
event<-reg_dummie("eletrobras", eletrobras_window, event)
event<-reg_dummie("ogx", ogx_window, event)
event<-reg_dummie("odebrecht", odbe_window, event)
event<-reg_dummie("queiroz galvao", qg_window, event)

event_cluster<-c()

for( position in 1:length(eve)){
  line<-which(eventos_panel[,2] == eve[[position]][1] &  eventos_panel[,3] ==0)
  date<-eventos_panel[line,5]
  firms<-which(eventos_panel[,2] %in%  eve[[position]] &  eventos_panel[,3] ==0) 
  firms<-  eventos_panel[firms,1]
  
  cluster<- which(colnames(dados)%in% firms)
  if(length(cluster) == 1){
    cluster<- dados[,cluster[1]] 
  }else if(length(cluster) == 2){
    cluster<- (dados[,cluster[1]] + dados[,cluster[2]])/2
  }else  if(length(cluster) == 3){
    cluster<- (dados[,cluster[1]] + dados[,cluster[2]] +dados[,cluster[3]] )/3 
  }else {
    cluster<- (dados[,cluster[1]] + dados[,cluster[2]] +dados[,cluster[3]] + dados[,cluster[4]] )/4
  }
  
  
  cluster<-cbind(dados[,c(1,3)], cluster)
 firm_window<-cluster
 
 line<-which(firm_window[,1] == date)
 if(length(line) == 0){
   line<-which(firm_window[,1] == as.Date(date, format = "%d/%m/%Y")-1 )
 }
   df1<-firm_window[((line-3):(line +4)),]
  df2<-firm_window[((line+90):(line +360)),]
  df<-rbind(df1, df2)
  d<-matrix(0,nrow = nrow(df), ncol =8)
  df<-cbind(df,d)
  d<-matrix(0,nrow = nrow(df), ncol =8)
  df<-cbind(df,d)
  names<-c("d3", "d2", "d1", "d0", "d_1", "d_2", "d_3", "d_4")
  
  colnames(df)<-c(colnames(df[,1:3]), names   )
  for(j in 1:8){
    
    df[j,3+j]<-1
    
  }
  
  reg<-lm(cluster ~ ibov + d3 + d2 + d1 + d0 +  d_1 + d_2 +  d_3 + d_4 , data = df)
  sumar<-summary(reg)
  
  coef<-list(sumar$coefficients)
  res<-list(reg$residuals)
  reg_til<-lm(cluster~ ibov, data = df1)
  res_til<-list(reg_til$residuals)
  lista<- list( coef, res, res_til)
  names(lista)<-paste0(c("coef", "res", "res_til"), 1)
  event_cluster<-c(event_cluster,lista )
  
  
  
}
events<-c(event, event_cluster)
table<-matrix(0, ncol = 4, nrow = 8)
table<-as.data.frame(table)
tabela<-c()
for(i in 1:(length(events)/3 )){
  a<-events[[3*i-2]]
  a<-as.data.frame(a)
tabela<-rbind(tabela,a)
  
  }
  
for(i in 1:8){
  sequ<- seq(i+2, nrow(tabela), 10)
  table[i,1]<-mean(tabela[sequ,1])
  
  table[i,2]<-sqrt(sum(tabela[sequ,2]^2) )/length(sequ)
}
 
table[,3]<-table[,1]/table[,2]

table[,4]<-seq(-4,3,1)

colnames(table)<-c("mean", "std", "t-value", "window ")

SCAR<- matrix(0, ncol = 4, nrow = 8)
SCAR<-as.data.frame(SCAR)
tabela_SCAR<-tabela
sequ<-c()
for(i in 1:8 ){
  
  a<-seq(i+2, nrow(tabela), 10)
  sequ<- c(sequ, a)
  SCAR[i,1]<-sum(tabela[sequ,1])/length(sequ)

  SCAR[i,2]<-sum(( tabela[sequ,1]- SCAR[i,1]/i)^2)/(length(a)-1)
  }
SCAR[,3]<-SCAR[,1]/SCAR[,2]

SCAR[,4]<-seq(-4,3,1)

colnames(SCAR)<-c("mean", "std", "t-value", "window ")


stargazer::stargazer(table, type = "latex")
library(xtable)

xtable(table)
#####################fazer car e ic#######

#################### botar só uma regressão por empresa###########



#################### risk premium #####################

#organização inicial
ibov$X.Chg  <- gsub(",", ".", gsub("%", "",ibov$X.Chg  )) 

ibov$X.Chg<-as.numeric( ibov$X.Chg)/100

summary(reg_Walter)
R_t<-cbind(ibov[,c(1,4)], c( ibov$X.Chg[-1], NA))
colnames(R_t)<-c("date", "r_t","r_t_1" )
R_t<-R_t[1:(nrow(R_t)-2 ),]

R_t$dummy_ind<-0

R_t$dummy_den<-0

tabela_cnn<-read.csv2("tabela cnn.csv")[,-1]

timeline<-c()
padrao_data <- "\\d{2}/\\d{2}/\\d{4}"  # Expressão regular para datas no formato dd/mm/yyyy

for (i in 1:nrow(tabela_cnn)) {
  date <- str_extract_all(tabela_cnn[i,2], padrao_data) 
  timeline<-c(timeline, as.vector(date))
}
timeline<-unlist(timeline)
tabela_cnn<-tabela_cnn[1:78,]
tabela_cnn$Date<-as.Date(timeline, format ="%d/%m/%Y" )
tabela_cnn$fase<- c(1:78)

estat_fases<-read.csv("estat_fases.csv")
tabela_cnn$estats<-0
tabela_cnn$estats[-78]<-estat_fases[,2]



pos_den<-which(R_t$date %in% eventos_panel$Exchange.Date)

R_t$dummy_den[pos_den]<-1

pos_ind<-which(R_t$date %in% eventos_panel_ind$Exchange.Date)

R_t$dummy_ind[pos_ind]<-1

#encaixar estatistica# preciso concertar por estar usando lgarch
R_t$stats<-NA
j<-1
while(j < nrow(tabela_cnn)){
  int<-  which(as.Date(R_t$date, format= "%d/%m/%Y") >= tabela_cnn$Date[j] & as.Date(R_t$date, format= "%d/%m/%Y") < tabela_cnn$Date[j+1]) 
R_t$stats[int]<-tabela_cnn$estats[j]
j<-j+1
}

na<-which(is.na(R_t$stats))
R_t<-R_t[-na,]
R_t[is.na(R_t)] <- 0
reg_std<-lm(r_t ~ r_t_1, data = R_t)

res_std<-reg_std$residuals
library(lgarch)

garch_std<- lgarch(res_std,  arch = 1, garch = 1 )


reg_den<-lm(r_t ~ r_t_1 + dummy_den + stats , data = R_t)

res_den<-reg_den$residuals

summary(reg_den)
garch_den<- lgarch(res_den, xreg = R_t[, c(5,6)]  , arch = 1, garch = 1 )


reg_ind<-lm(r_t ~ r_t_1 + dummy_ind + stats , data = R_t)
summary(reg_ind)
res_ind<-reg_ind$residuals


garch_ind<- lgarch(res_ind, xreg = R_t[, c(4,6)]  , arch = 1, garch = 1 )

############### com bond ##########

rf<-read.csv2("bonds daily.csv")
rf<-rf[-(1:26),-(1:2)]
colnames(rf)<-rf[1,]

rf<-rf[-1,]
rf<-rf[,c(1,4)]

rf[,2]  <- gsub(",", ".", rf[,2]  ) 

R_t$r_f<-0

pos_rf<-which(rf[,1] %in% R_t$date)
pos_rt<-which( R_t$date %in%  rf[,1])
R_t$r_f<-0

R_t$r_f[pos_rt]<-rf[pos_rf,2]

R_t$r_f<- as.numeric(R_t$r_f)/100

R_t$r_f<- (1 + as.numeric(R_t$r_f))^(1/252) - 1 # tirando do anualizado


R_t$risk_premium<-0

R_t$risk_premium<-R_t$r_t - R_t$r_f

# economic conditions

dummy_rec<-read.csv("BRAREC.csv")

focu<-read.csv("focus gdp.csv")

date<-as.Date(dummy_rec[,1], format = "%Y-%m-%d")
dummy_rec[,1] <- format(as.Date(date), "%d/%m/%Y")



mes_ano_Rt<-cbind(month(R_t$date), year(R_t$date))
R_t$recession<-0

for(i in 1:nrow(dummy_rec) ){
  
  if(dummy_rec[i,2] == 1){
    print(i)
   pos<- which(month(as.Date(R_t$date, format= "%d/%m/%Y" )) == month(as.Date(dummy_rec[i,1], format = "%d/%m/%Y"))  & year(as.Date(R_t$date, format= "%d/%m/%Y" )) == year(as.Date(dummy_rec[i,1], format = "%d/%m/%Y")) )
    R_t$recession[ pos]<-1
   }
}

R_t$rec_est<- R_t$stats * R_t$recession
R_t[is.na(R_t)] <- 0
reg_PV<-lm( risk_premium ~ rec_est + stats + recession, data = R_t)
summary(reg_PV)
library(garchx)
res_PV<-reg_PV$residuals
garch_PV<-garchx(res_PV, xreg = R_t$stats )

########## carteira de indiciadas########
#junta aqui e faz a regressão

carteira<-rowMeans(dados[,-1], na.rm = TRUE)
carteira<-data.frame(dados[,1], carteira)
colnames(carteira)<- c("date", "carteira")


R_t<-left_join(R_t, carteira, by = "date")
R_t[is.na(R_t)] <- 0
reg_Walter<-  lm( risk_premium ~ rec_est + stats + recession + carteira + dummy_den, data = R_t)
res_Walter<-reg_Walter$residuals
garch_Walter<-garchx(res_Walter, xreg = R_t[,c(5,6,9)] )
summary(reg_Walter)
summary(reg_PV)

library(xtable)
summary(reg_den)
xtable(reg_den)
library(stargazer)
stargazer(reg_den, type = "latex")
stargazer(garch_std, type = "latex")
summary(reg_std)
(garch_std)
garch_PV
ttest0(garch_PV)
print(garch_std)
garch_den
garch_ind
