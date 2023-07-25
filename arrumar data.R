setwd("/home/nathan/Downloads/disertacao")
jurong<-read.csv('jurong.csv')
jurong<-jurong[-(1:26),(1:10)]
colnames(jurong)<-jurong[1,]
date<-as.Date(jurong[,1],format = "%d-%b-%Y")
date_form<- format(date,format = "%d/%m/%Y")
jurong[,1]<-date_form

write.csv(jurong, file = "jurong.csv")

setwd("/home/nathan/Downloads/disertacao")
caixa<-read.csv('caixa daily.csv')
caixa<-caixa[-(1:27),(1:10)]
colnames(caixa)<-caixa[1,]
date<-as.Date(caixa[,1],format = "%d-%b-%Y")
date_form<- format(date,format = "%d/%m/%Y")
caixa[,1]<-date_form

write.csv(caixa, file = "caixa.csv")

setwd("/home/nathan/Downloads/disertacao")
maersk<-read.csv('maersk.csv')
maersk<-maersk[-(1:8),(1:10)]
colnames(maersk)<-maersk[1,]
date<-as.Date(maersk[,1],format = "%d-%b-%Y")
date_form<- format(date,format = "%d/%m/%Y")
maersk[,1]<-date_form

write.csv(maersk, file = "maersk.csv")

setwd("/home/nathan/Downloads/disertacao")
ecorod<-read.csv('ecorodovias daily.csv')
ecorod<-ecorod[-(1:28),(1:10)]
colnames(ecorod)<-ecorod[1,]
date<-as.Date(ecorod[,1],format = "%d-%b-%Y")
date_form<- format(date,format = "%d/%m/%Y")
ecorod[,1]<-date_form

write.csv(ecorod, file = "ecorod.csv")

setwd("/home/nathan/Downloads/disertacao")
tenaris<-read.csv('tenaris.csv')
tenaris<-tenaris[-(1:27),(1:10)]
colnames(tenaris)<-tenaris[1,]
date<-as.Date(tenaris[,1],format = "%d-%b-%Y")
date_form<- format(date,format = "%d/%m/%Y")
tenaris[,1]<-date_form

write.csv(tenaris, file = "tenaris.csv")

setwd("/home/nathan/Downloads/disertacao")
saipem<-read.csv('saipem.csv')
saipem<-saipem[-(1:26),(1:10)]
colnames(saipem)<-saipem[1,]
date<-as.Date(saipem[,1],format = "%d-%b-%Y")
date_form<- format(date,format = "%d/%m/%Y")
saipem[,1]<-date_form

write.csv(saipem, file = "saipem.csv")
