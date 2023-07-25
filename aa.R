setwd("/home/nathan/Downloads/disertacao")
library(tidyr)
library(stringr)
library(dplyr)
library(lubridate)
denunciados<-read.csv("denunciados3.csv")
denunciados<-denunciados[,2:5]

# botar o numero de empresas envolvidos
num_virgulas <- str_count(denunciados$X.1, ",")
num_and<- str_count(denunciados$X.1, " e ")
num_env<-1 + num_virgulas + num_and
denunciados$num_emp<-num_env
den_separado = separate(denunciados, X.1, into = c(paste(1:max(denunciados$num_emp))), sep = ", ")
den_separado[] <- lapply(den_separado, tolower)
den_separado[] <- lapply(den_separado, trimws)

#companies<-read.csv("companies.csv")

eventos<-denunciados[,1:3]

d<-matrix(0,nrow(eventos),13)
comp<-c("petrobras", "ebx", "mendes junior","eletronuclear", "queiroz galvao", "banco do brasil *", "jurong",
        "saipem", "maersk", "econorte", "tenaris", "andrade gutierrez", "odebrecht", "oas",
        "delta", "dersa", "carioca engenharia", "politico", "engevix", "correios", "utc", "camargo correa", 
        "tucumann engenharian", "galvao engenharia")
d<-matrix(0,nrow(eventos),length(comp))
#achar as linhas de cada
for(j in 1:length(comp)){
  a<-c()
  for( i in 1:23){
    linhas <- which(den_separado[i+3] == comp[j])
   a<-c(a, linhas)
  }
  d[1:length(a),j]<-a
  print(j)
  }

colnames(d)<-comp



eventos<-cbind(eventos, d)
eventos[,4:length(d)]<-0

for(i in 1:ncol(d)){
  for( j in 1:160){
   if( d[j,i] != 0){
     k<-d[j,i] 
     eventos[as.numeric(k),i+3]<-1
   }
  }
  
}

write.csv(eventos, file = "eventos.csv")

eventos<-read.csv("eventos.csv")
ibov<-read.csv2("ibov completo.csv")
petrobras<-read.csv("petrobras.csv")
eletrobras<-read.csv("petrobras.csv")
ogx<-read.csv("ogx.csv")
mendes<-read.csv("mendes.csv")
qg<-read.csv("enauta.csv")
ecorod<-read.csv("ecorod.csv")
jurong<-read.csv('jurong.csv')
caixa<-read.csv('caixa.csv')
maersk<-read.csv('maersk.csv')
tenaris<-read.csv('tenaris.csv')
saipem<-read.csv('saipem.csv')
bb<-read.csv('bb.csv')
#############
qg<-qg[,-1]
colnames(maersk)<-colnames(petrobras)
#panel<-cbind("petrobras", petrobras[linha,])
eventos_panel<-as.data.frame(matrix(0,1,14))
colnames(eventos_panel)<-colnames(petrobras)

######################
dados<-list()
dados<-list(petrobras = petrobras , ogx = ogx, mendes = mendes , eletrobras = eletrobras, qg = qg, bb = bb, 
            jurong = jurong, saipem=saipem, maersk=maersk, ecorod = ecorod,   tenaris = tenaris)
#comp<-c("petrobras", "ebx", "mendes junior","eletronuclear", "queiroz galvao", "caixa", "banco do brasil *", "jurong",
 #       "saipem", "maersk", "econorte", "tenaris", "andrade gutierrez", "odebrecht", "oas",
  #      "delta", "dersa", "carioca engenharia", "politico", "engevix", "correios", "utc", "camargo correa", 
   #     "tucumann engenharian", "galvao engenharia")
teste<-c("petrobras", "ogx", " mendes junior", "eletrobras", "queiroz galvao", " banco do brasil","jurong",
         "saipem", "maersk ",  "ecorodovias", "tenaris"  )
k<-1
for(j in 1:length(dados)){
tamanho<-ncol(dados[[j]])

  for ( i in 1:nrow(eventos)){
    if(eventos[i, 4+j] == 1){
      print(j)
      linha <- which(dados[[j]]$Exchange.Date == eventos$datas[i])
      if (length(linha) == 0){
        print(i)
        print(j)
        linha <- which(as.Date(dados[[j]]$Exchange.Date,format = "%d/%m/%Y") == as.Date(eventos$datas[i], format = "%d/%m/%Y") + 1)
      }
      linha<-seq(linha-4, linha+3,1)
      janela<-seq(-4,3,1)
      #janela do evento
      dados[[j]]<-cbind(dados[[j]], 0)
      dados[[j]][linha,ncol(dados[[j]])]<-1

      panel<-cbind(teste[j],k,janela, dados[[j]][linha,1:tamanho])
      
      colnames(panel)<-colnames(eventos_panel)
       eventos_panel<-rbind(eventos_panel,panel)
       
       
    k<-k+1
       }
  }
}


#### foi, agora é basicamente montar a base

write.csv(eventos_panel, file = "eventos_panel2.csv")

petrobras_eventos<-dados$petrobras

write.csv(petrobras_eventos, file = "petrobras_eventos.csv")

eletrobras_eventos<-dados$eletrobras
write.csv(eletrobras_eventos, file = "eletrobras_eventos.csv")
ogx_eventos<-dados$ogx
write.csv(ogx_eventos, file = "ogx_eventos.csv")
mendes_eventos<-dados$mendes
write.csv(mendes_eventos, file = "mendes_eventos.csv")
qg_eventos<-dados$qg
write.csv(qg_eventos, file = "qg_eventos.csv")
