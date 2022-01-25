#IMPORT LIBRERIE#
library(readxl)
library(readr)
library(lubridate)
library(stringr)
library(MASS)
library(stringr)
library(dplyr)
library(car)
library(olsrr)
library(het.test)
library(psych)
library(skedastic)
library(DataCombine)
library(tseries)
library(xts)
library(forecast)
library(urca)
library(MLmetrics)
library(Rcpp)
library(rlang)
library(RcppEigen)
library(StanHeaders)
library(prophet)
library(tidyselect)
library(writexl)
library(ggplot2)

#PREPROCESSING#


#dati u_1_2018
##aggregazione rilevazioni mensili 
gen_18_u1 <- read_excel("Gennaio2018_9685_20191029083905_PDO_12883450152.xltx")
gen_18_u1$...9 <- NULL
gen_18_u1$...10 <- NULL
View(gen_18_u1)

feb_18_u1 <- read_excel("Febbraio2018_9705_20191029083905_PDO_12883450152.xltx")
feb_18_u1$...9 <- NULL
feb_18_u1$...10 <- NULL
View(feb_18_u1)

mar_18_u1 <- read_excel("Marzo2018_9706_20191029083905_PDO_12883450152.xltx")
mar_18_u1$...9 <- NULL
mar_18_u1$...10 <- NULL
mar_18_u1$...11 <- NULL
View(mar_18_u1)

apr_18_u1 <- read_excel("Aprile2018_9707_20191029083905_PDO_12883450152.xltx")
apr_18_u1$...9 <- NULL
apr_18_u1$...10 <- NULL

mag_18_u1 <- read_excel("Maggio2018_9708_20191029083905_PDO_12883450152.xltx")
mag_18_u1$...9 <- NULL
mag_18_u1$...10 <- NULL
mag_18_u1$...11 <- NULL

giu_18_u1 <- read_excel("Giugno2018_9709_20191029083905_PDO_12883450152.xltx")
giu_18_u1$...9 <- NULL
giu_18_u1$...10 <- NULL
giu_18_u1$...11 <- NULL

lug_18_u1 <- read_excel("Luglio2018_9710_20191029083905_PDO_12883450152.xltx")
lug_18_u1$...9 <- NULL
lug_18_u1$...10 <- NULL
lug_18_u1$...11 <- NULL
lug_18_u1$...12 <- NULL

ago_18_u1 <- read_excel("Agosto2018_10210_20191111163505_PDO_12883450152.xlsx")
ago_18_u1$...9 <- NULL
ago_18_u1$...10 <- NULL
ago_18_u1$...11 <- NULL

set_18_u1 <- read_excel("Settembre2018_10211_20191111163505_PDO_12883450152.xlsx")
set_18_u1$...9 <- NULL
set_18_u1$...10 <- NULL
set_18_u1$...11 <- NULL

ott_18_u1 <- read_excel("Ottobre2018_10212_20191111163505_PDO_12883450152.xlsx")
ott_18_u1$...9 <- NULL
ott_18_u1$...10 <- NULL
ott_18_u1$...11 <- NULL

nov_18_u1 <- read_excel("Novembre2018_10208_20191111163505_PDO_12883450152.xlsx")
nov_18_u1$...9 <- NULL
nov_18_u1$...10 <- NULL
nov_18_u1$...11 <- NULL

dic_18_u1 <- read_excel("Dicembre2018_10209_20191111163505_PDO_12883450152.xlsx")
dic_18_u1$...9 <- NULL
dic_18_u1$...10 <- NULL
dic_18_u1$...11 <- NULL

u1_18 <- rbind(gen_18_u1, feb_18_u1, mar_18_u1, apr_18_u1, mag_18_u1, giu_18_u1, lug_18_u1, ago_18_u1, set_18_u1, ott_18_u1, nov_18_u1, dic_18_u1)
View(u1_18)

write_xlsx(u1_18, "D:/Bicocca/DS_lab\\u1_18.xlsx")

#dati u_1_2019
##unificazione rilevazioni mensili 
gen_19_u1 <- read_excel("Gennaio2019_9811_20191104093543_PDO_12883450152.xltx")
gen_19_u1$...9 <- NULL
View(gen_19_u1)

feb_19_u1 <- read_excel("Febbraio2019_9812_20191104093544_PDO_12883450152.xltx")
feb_19_u1$...9 <- NULL
feb_19_u1$...10 <- NULL
View(feb_19_u1)

mar_19_u1 <- read_excel("Marzo2019_9813_20191104093542_PDO_12883450152.xltx")
mar_19_u1$...9 <- NULL
mar_19_u1$...10 <- NULL
View(mar_19_u1)

apr_19_u1_pp <- read_excel("Aprile2019(1-29)_9825_20191104093542_PDO_12883450152.xltx")
apr_19_u1_pp$...9 <- NULL
apr_19_u1_pp$...10 <- NULL
View(apr_19_u1_pp)
apr_19_u1_sp <- read_excel("Aprile2019(30)_9805_20191104093538_PDO_12883450152.xltx")
View(apr_19_u1_sp)

apr_19_u1 <- rbind(apr_19_u1_pp,apr_19_u1_sp )

mag_19_u1 <- read_excel("Maggio2019_9806_20191104093538_PDO_12883450152.xltx")
mag_19_u1$...9 <- NULL
mag_19_u1$...10 <- NULL

giu_19_u1 <- read_excel("Giugno2019_9807_20191104093538_PDO_12883450152.xltx")
giu_19_u1$...9 <- NULL
giu_19_u1$...10 <- NULL

lug_19_u1 <- read_excel("Luglio2019_9808_20191104093538_PDO_12883450152.xltx")
lug_19_u1$...9 <- NULL
lug_19_u1$...10 <- NULL

ago_19_u1 <- read_excel("Agosto2019_9809_20191104093539_PDO_12883450152.xltx")
ago_19_u1$...9 <- NULL
ago_19_u1$...10 <- NULL
ago_19_u1$...11 <- NULL

set_19_u1 <- read_excel("Settembre2019_10245_20191112143021_PDO_12883450152.xlsx")
set_19_u1$...9 <- NULL
set_19_u1$...10 <- NULL
set_19_u1$...11 <- NULL
set_19_u1$...12 <- NULL

ott_19_u1 <- read_excel("Ottobre2019_9988_20191105154921_PDO_12883450152.xltx")
ott_19_u1$...9 <- NULL
ott_19_u1$...10 <- NULL

nov_19_u1 <- read_excel("Novembre2019_11307_20191204161908_PDO_12883450152.xlsx")
nov_19_u1$...9 <- NULL
nov_19_u1$...10 <- NULL

dic_19_u1 <- read.csv("U1_dicembre2019_14085_20200218142629_PDO_12883450152.csv", sep=";")

u1_19 <- rbind(gen_19_u1,feb_19_u1,mar_19_u1,apr_19_u1,mag_19_u1,giu_19_u1,lug_19_u1,ago_19_u1,set_19_u1,ott_19_u1,nov_19_u1,dic_19_u1)

write_xlsx(u1_19,"D:/Bicocca/DS_lab\\u1_19.xlsx")

#dati u_1_2020
##unificazione rilevazioni mensili 
gen_20_u1<-read.csv("Copia di U1_gennaio2020_14066_20200218142630_PDO_12883450152.csv", sep=";")

feb_20_u1<-read.csv("U1_febbraio2020_14848_20200318165340_PDO_12883450152.csv", sep=";")

mar_20_u1<-read.csv("U1_marzo2020_15115_20200406105853_PDO_12883450152.csv", sep=";")

apr_20_u1<-read.csv("U1_Aprile_2020_16325_20200513131423_PDO_12883450152.csv", sep=";")

mag_20_u1<-read.csv("U1_maggio_2020_16607_20200605122615_PDO_12883450152.csv", sep=";")

giu_20_u1<-read.csv("U1_giugno_17604_20200720130219_PDO_12883450152.csv", sep=";")

lug_20_u1<-read.csv("U1_Luglio_20200831112713541_IT012E00491869_20200701_20200731.csv", sep=";")

ago_20_u1<-read.csv("U1_ago_2020_20201202101351030_IT012E00491869_20200801_20200831.csv", sep=";")

set_20_u1<-read.csv("U1_sett_2020_20201202101338266_IT012E00491869_20200901_20200930.csv", sep=";")

ott_20_u1<-read.csv("U1_ott_2020_IT012E00491869_20201001_20201031.csv", sep=";")

nov_20_u1<-read.csv("U1_nov_2020_20201202101319092_IT012E00491869_20201101_20201130.csv", sep=";")

dic_20_u1<-read.csv("U1_DIC_2020_IT012E00491869.csv", sep=";")

u1_20 <- rbind(gen_20_u1, feb_20_u1, mar_20_u1, apr_20_u1, mag_20_u1, giu_20_u1, lug_20_u1, ago_20_u1, set_20_u1, ott_20_u1, nov_20_u1, dic_20_u1)

write_xlsx(u1_20, "D:/Bicocca/DS_lab\\u1_20.xlsx")

#dati u_6_2018
##unificazione rilevazioni mensili
gen_18_u6 <- read_excel("Gennaio2018_11075_20191127154705_PDO_12883450152.xlsx")

feb_18_u6 <- read_excel("Febbraio2018_11024_20191127154705_PDO_12883450152.xlsx")

mar_18_u6 <- read_excel("Marzo2018_11076_20191127154705_PDO_12883450152.xlsx")

apr_18_u6 <- read_excel("Aprile2018_11077_20191127154706_PDO_12883450152.xlsx")

mag_18_u6 <- read_excel("Maggio2018_11078_20191127154706_PDO_12883450152.xlsx")

giu_18_u6 <- read_excel("Giugno2018_11085_20191127154706_PDO_12883450152.xlsx")

lug_18_u6 <- read_excel("Luglio2018_11082_20191127155105_PDO_12883450152.xlsx")

ago_18_u6_pp <- read_excel("Agosto2018(1-8)_11088_20191127155104_PDO_12883450152.xlsx")
ago_18_u6_sp <- read_excel("Agosto2018(9-31)_11086_20191127155104_PDO_12883450152.xlsx")
ago_18_u6 <- rbind(ago_18_u6_pp,ago_18_u6_sp )

set_18_u6 <- read_excel("Settembre2018_11079_20191127155103_PDO_12883450152.xlsx")

ott_18_u6 <- read_excel("Ottobre2018_11087_20191127155104_PDO_12883450152.xlsx")

nov_18_u6 <- read_excel("Novembre2018_11080_20191127155104_PDO_12883450152.xlsx")

dic_18_u6 <- read_excel("Dicembre2018_11081_20191127155104_PDO_12883450152.xlsx")

u6_18 <- rbind(gen_18_u6,feb_18_u6,mar_18_u6,apr_18_u6,mag_18_u6,giu_18_u6,lug_18_u6,ago_18_u6,set_18_u6,ott_18_u6,nov_18_u6,dic_18_u6)

write_xlsx(u6_18,"D:/Bicocca/DS_lab\\u6_18.xlsx")

#dati u_6_2019
##unificazione rilevazioni mensili
gen_19_u6 <- read_excel("Gennaio2019_11092_20191127155557_PDO_12883450152.xlsx")

feb_19_u6 <- read_excel("Febbraio2019_11093_20191127155557_PDO_12883450152.xlsx")

mar_19_u6 <- read_excel("Marzo2019_11107_20191127155557_PDO_12883450152.xlsx")

apr1_19_u6 <- read_excel("Aprile2019(1-29)_11094_20191127155558_PDO_12883450152.xlsx")
apr2_19_u6 <- read_excel("Aprile2019(30)_11089_20191127155555_PDO_12883450152.xlsx")
apr_19_u6 <- rbind(apr1_19_u6, apr2_19_u6)

mag_19_u6 <- read_excel("Maggio2019_11083_20191127155555_PDO_12883450152.xlsx")

giu_19_u6 <- read_excel("Giugno2019_11084_20191127155556_PDO_12883450152.xlsx")

lug_19_u6 <- read_excel("Luglio2019_11090_20191127155556_PDO_12883450152_ELABORATO.xlsx")
lug_19_u6$...2 <- NULL
lug_19_u6$`POTENZA ATTIVA KW` <- NULL
lug_19_u6$ORA...5 <- NULL
colnames(lug_19_u6)[colnames(lug_19_u6)=="ORA...4"] <- "ORA"

ago_19_u6 <- read_excel("Agosto2019_11091_20191127155556_PDO_12883450152.xlsx")

set_19_u6 <- read_excel("Settembre2019_11105_20191127155556_PDO_12883450152.xlsx")

ott_19_u6 <- read_excel("Ottobre2019_11106_20191127155556_PDO_12883450152.xlsx")

nov_19_u6 <- read.csv("U6_Novembre_2019_20201109120532475_IT012E00491824_20191101_20191130.csv" , sep=";")

dic_19_u6 <- read.csv("U6_dicembre2019_14073_20200218144136_PDO_12883450152.csv" , sep=";")

u6_19 <- rbind(gen_19_u6, feb_19_u6, mar_19_u6, apr_19_u6, mag_19_u6, giu_19_u6, lug_19_u6, ago_19_u6, set_19_u6, ott_19_u6, nov_19_u6, dic_19_u6)

write_xlsx(u6_19, "D:/Bicocca/DS_lab\\u6_19.xlsx")

#dati u_6_2020
##unificazione rilevazioni mensili
gen_20_u6 <- read_excel("U6_gennaio2020_14074_20200218144136_PDO_12883450152.xlsx")

feb_20_u6 <- read_excel("U6_febbraio2020_15629_20200417144935_PDO_12883450152.xlsx")

mar_20_u6 <- read_excel("U6_marxo_2020_15628_20200417144434_PDO_12883450152.xlsx")

apr_20_u6 <- read.csv("U6_Apr_2020_16230_20200511101230_PDO_12883450152.csv", sep=";")

mag_20_u6 <- read.csv("U6_maggio_2020_16608_20200605122654_PDO_12883450152.csv", sep=";")

giu_20_u6 <- read_excel("U6_giugno_17604_20200720130219_PDO_12883450152.xlsx")
giu_20_u6$anno <- NULL
giu_20_u6$mese <- NULL
giu_20_u6$giorno <- NULL
giu_20_u6$`giorno/sett` <- NULL
giu_20_u6$ora <- NULL
giu_20_u6$TIPO_DATO <- "E"

lug_20_u6 <- read_excel("luglio corretto.xlsx")

ago_20_u6 <- read.csv("U6_Agosto_20201109_114712332_IT012E00491824_20200801_20200831.csv", sep=";")

set_20_u6 <- read.csv("U6_sett_2020_IT012E00491824_20200901_20200930.csv", sep=";")

ott_20_u6 <- read.csv("U6_Ottobre_20_modificato.csv", sep=";")

nov_20_u6 <- read.csv("U6_NOV_2020_IT012E00491824_20201101_20201130.csv", sep=";")

dic_20_u6 <- read.csv("U6_Dic_20210_IT012E00491824.csv", sep=";")

u6_20 <- rbind(gen_20_u6,feb_20_u6,mar_20_u6,apr_20_u6,mag_20_u6,giu_20_u6,lug_20_u6,ago_20_u6,set_20_u6,ott_20_u6,nov_20_u6,dic_20_u6)

write_xlsx(u6_20,"D:/Bicocca/DS_lab\\u6_20.xlsx")

#aggregazione rilevazioni annuali
u1_18 <- read_excel("u1_18.xlsx")
u1_19 <- read_excel("u1_19.xlsx")
u1_20 <- read_excel("u1_20.xlsx")

u6_18 <- read_excel("u6_18.xlsx")
u6_19 <- read_excel("u6_19.xlsx")
u6_20 <- read_excel("u6_20.xlsx")

u1 <- rbind(u1_18, u1_19, u1_20)
u6 <- rbind(u6_18, u6_19, u6_20)

write_xlsx(u1, "D:/Bicocca/DS_lab\\u1.xlsx")
write_xlsx(u6, "D:/Bicocca/DS_lab\\u6.xlsx")


##preprocessing dati U_1##
#import dati U_1
u1_corretto <- read_xlsx("U1.xlsx")

#uniformare la colonna data_ora
options(scipen=999)   #esclusione notazione scientifica
u1_corretto$ORA <- str_pad(u1_corretto$ORA, 6, pad = "0") #aggiunta di 0 per poter avere sei cifre nella colonna

fun_insert <- function(x, pos, insert) {        
  gsub(paste0("^(.{", pos, "})(.*)$"), 
       paste0("\\1", insert, "\\2"), 
       x) 
} #funzione per aggiungere caratteri


u1_corretto$ora <- fun_insert(x = u1_corretto$ORA,    #applico ad ora 
                              pos = 2,  
                              insert = ":") 
u1_corretto$ora

u1_corretto$ora<- fun_insert(x = u1_corretto$ora,    #applico ad ora 
                             pos = 5,  
                             insert = ":") 
u1_corretto$ora

u1_corretto$data <- fun_insert(x = u1_corretto$DATA,    #applico a data
                               pos = 4,  
                               insert = "-") 
u1_corretto$data

u1_corretto$data <- fun_insert(x = u1_corretto$data,    #applico a data
                               pos = 7,  
                               insert = "-") 
u1_corretto$data


u1_corretto <- within(u1_corretto, data_ora <- paste(data, ora, sep=' ')) #unione colonne data e ora

u1_corretto$data_ora <- strptime(u1_corretto$data_ora, format="%Y-%m-%d  %H:%M:%S") #formato datetime

head(u1_corretto$data_ora, n=50)

#selezione solo colonne data_ora e consumo attiva prelevata
u1_s <- select(u1_corretto, data_ora, CONSUMO_ATTIVA_PRELEVATA)


#formato colonna CONSUMO_ATTIVA_PRELEVATA e la rinominazione colonna  in 'consumo'
u1_s$CONSUMO_ATTIVA_PRELEVATA <-str_replace(u1_s$CONSUMO_ATTIVA_PRELEVATA , ",", ".")
u1_s$CONSUMO_ATTIVA_PRELEVATA <- as.numeric(u1_s$CONSUMO_ATTIVA_PRELEVATA )
u1_s$CONSUMO_ATTIVA_PRELEVATA <- round(u1_s$CONSUMO_ATTIVA_PRELEVATA, digits = 1)
colnames(u1_s)[which(names (u1_s) == "CONSUMO_ATTIVA_PRELEVATA")] <- "consumo"
summary(u1_s)  

# la summary evidenzia la presenza di valori 0.00 per la colonna consumo => sostituzione con la media dei valori più vicini
u1_0 <- subset(u1_s, consumo == 0.0)
unique(u1_0$data_ora)

#trasformazione consumo in chr per poi trasormare gli 0.0 in NA
u1_s$consumo<- as.character(u1_s$consumo)
u1_s$consumo <- replace(u1_s$consumo, u1_s$consumo == 0.0, NA)
u1_null <- u1_s[is.na(u1_s$consumo),]

#applicazione funzione 'as.numeric' su consumo 
u1_s$consumo <- as.numeric(u1_s$consumo )

#rimozione eventuali duplicati data_ora
u1_s <- u1_s[order(-u1_s$consumo), ]
u1_s <- u1_s[!duplicated(u1_s$data_ora), ]
u1_s <- u1_s[order(u1_s$data_ora), ]

summary(u1_s)

#sostituisco i valori NA con la media dei valori più vicini
a <- max(diff(which(!is.na(u1_s$consumo)))-1)

repeat{
  for(i in 1:a){
    u1_s$consumo[which(u1_s$consumo%in%NA)] <- ((lag(u1_s$consumo, 1)+lead(u1_s$consumo, i))/2)[which(u1_s$consumo%in%NA)]
  }
  if(any(is.na(u1_s$consumo)) ==FALSE) { break }
}

summary(u1_s)

#aggrgazione valori consumo su base giornaliera
u1_s$data_ora <- strptime(u1_s$data_ora, format="%Y-%m-%d")
u1_s$data_ora <- as.Date(u1_s$data_ora)
u1_s <- aggregate(u1_s["consumo"], by=u1_s["data_ora"], mean)
u1_s$consumo <- u1_s$consumo * 24
head(u1_s)
tail(u1_s)

#eliminazione rilevazione del 29/02/2020 
u1_s[u1_s[["data_ora"]] == "2020-02-29", ]
u1_s <- u1_s[-c(790), ]

##preprocessing dati U_6##
#import dati U_6
u6_corretto <- read_xlsx("u6.xlsx")

#uniformare la colonna data_ora
options(scipen=999)   #escludo notazione scientifica
u6_corretto$ORA <- str_pad(u6_corretto$ORA, 6, pad = "0") #aggiunta di 0 per poter avere sei cifre nella colonna


fun_insert <- function(x, pos, insert) {       
  gsub(paste0("^(.{", pos, "})(.*)$"), 
       paste0("\\1", insert, "\\2"), 
       x) 
} #funzione per aggiungere caratteri


u6_corretto$ora <- fun_insert(x = u6_corretto$ORA,    #applico ad ora 
                              pos = 2,  
                              insert = ":") 
u6_corretto$ora

u6_corretto$ora<- fun_insert(x = u6_corretto$ora,    #applico ad ora 
                             pos = 5,  
                             insert = ":") 
u6_corretto$ora

u6_corretto$data <- fun_insert(x = u6_corretto$DATA,    #applico a data
                               pos = 4,  
                               insert = "-") 
u6_corretto$data

u6_corretto$data <- fun_insert(x = u6_corretto$data,    #applico a data
                               pos = 7,  
                               insert = "-") 
u6_corretto$data


u6_corretto <- within(u6_corretto, data_ora <- paste(data, ora, sep=' ')) #unione colonne data e ora

u6_corretto$data_ora <- strptime(u6_corretto$data_ora, format="%Y-%m-%d  %H:%M:%S") #formato datetime

head(u6_corretto$data_ora, n=50)

#selezione solo colonne data_ora e consumo attiva prelevata
u6_s <- select(u6_corretto, data_ora, CONSUMO_ATTIVA_PRELEVATA)

#formato colonna CONSUMO_ATTIVA_PRELEVATA e la rinominazione colonna  in 'consumo'
u6_s$CONSUMO_ATTIVA_PRELEVATA <-str_replace(u6_s$CONSUMO_ATTIVA_PRELEVATA , ",", ".")
u6_s$CONSUMO_ATTIVA_PRELEVATA <- as.numeric(u6_s$CONSUMO_ATTIVA_PRELEVATA )
u6_s$CONSUMO_ATTIVA_PRELEVATA <- round(u6_s$CONSUMO_ATTIVA_PRELEVATA, digits = 1)
colnames(u6_s)[which(names (u6_s) == "CONSUMO_ATTIVA_PRELEVATA")] <- "consumo"
summary(u6_s)  

# la summary evidenzia la presenza di valori 0.00 per la colonna consumo => sostituzione con la media dei valori più vicini
u6_0 <- subset(u6_s, consumo == 0.0)
unique(u6_0$data_ora)

#trasformazione consumo in chr per poi trasormare gli 0.0 in NA
u6_s$consumo<- as.character(u6_s$consumo)
u6_s$consumo <- replace(u6_s$consumo, u6_s$consumo == 0.0, NA)
u6_null <- u6_s[is.na(u6_s$consumo),]

#applicazione funzione 'as.numeric' su consumo 
u6_s$consumo <- as.numeric(u6_s$consumo )

#rimozione eventuali duplicati data_ora
u6_s <- u6_s[order(-u6_s$consumo), ]
u6_s <- u6_s[!duplicated(u6_s$data_ora), ]
u6_s <- u6_s[order(u6_s$data_ora), ]

summary(u6_s)


#sostituzione valori NA con la media dei valori più vicini
b <- max(diff(which(!is.na(u6_s$consumo)))-1)

repeat{
  for(i in 1:b){
    u6_s$consumo[which(u6_s$consumo%in%NA)] <- ((lag(u6_s$consumo, 1)+lead(u6_s$consumo, i))/2)[which(u6_s$consumo%in%NA)]
  }
  if(any(is.na(u6_s$consumo)) ==FALSE) { break }
}

summary(u6_s)

#aggrgazione valori consumo su base giornaliera
u6_s$data_ora <- strptime(u6_s$data_ora, format="%Y-%m-%d")
u6_s$data_ora <- as.Date(u6_s$data_ora)
u6_s <- aggregate(u6_s["consumo"], by=u6_s["data_ora"], mean)
u6_s$consumo <- u6_s$consumo * 24
head(u6_s)
tail(u6_s)

#eliminazione rilevazione del 29/02/2020
u6_s[u6_s[["data_ora"]] == "2020-02-29", ]
u6_s <- u6_s[-c(790), ]

#previsione valori di U_6 del mese giugno 2020
u6_p <- subset(u6_s, data_ora >= "2018-01-01" & data_ora <= "2020-05-31")
head(u6_p)
tail(u6_p)
names(u6_p)[names(u6_p) == 'data_ora'] <- 'ds'
names(u6_p)[names(u6_p) == 'consumo'] <- 'y'
model1 <- prophet(u6_p)
future1 <- make_future_dataframe(model1, period = 30) 
tail (future1)
forecast1 <- predict(model1, future1)
tail(forecast1[c('ds','yhat','yhat_lower','yhat_upper')])

#creazione di un  subset per fare una merge con u6_s
u6_m <- select(forecast1, ds, yhat)
u6_m <- subset(u6_m, ds >= "2020-06-01" & ds <= "2020-06-30")
colnames(u6_m)[which(names (u6_m) == "ds")] <- "data_ora"
u6_m$data_ora<- as.Date(u6_m$data_ora)
u6_u <- merge(u6_s,u6_m, all.x = T,all.y = T, by.x = "data_ora", by.y = "data_ora")
u6_u$consumo <- ifelse(is.na(u6_u$yhat), u6_u$consumo, u6_u$yhat)
u6_u$yhat <- NULL
u6_u[880:890,]
plot(u6_s, type = "l")
lines(u6_u, type = "l", col= 4)

##FINE PREPROCESSING##
###dataset definitivi: u1_s e u6_u###

###Aggregazione dati su base settimanale, analisi stagionalità e previsione###

#creazione training set (periodo da 07/01/18 a 04/10/20) e aggregazione settimanale
u1_s_sub <- (u1_s[0:1007,])
u1_z_sub <- read.zoo(u1_s_sub, format = "%Y-%m-%d")
u1_z_sub <- as.xts(u1_z_sub)
u1_week_sub <- apply.weekly(u1_z_sub,sum)

#aggregazione dati su base settimanale
u1_z <- read.zoo(u1_s, format = "%Y-%m-%d")
u1_z <- as.xts(u1_z)
u1_week <- apply.weekly(u1_z,sum)

#controllo acf e pacf
par(mfrow=c(1,2),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
acf(u1_z,lag.max = 50)
pacf(u1_z, lag.max = 50)

#trasformzione dataset e training set in oggetto time series
U_1_ts <-ts(u1_week, start=c(2018,01,07), frequency = 52)
u_1_train_ts <- ts(u1_week_sub, start=c(2018,01,07), frequency = 52)

#test per verifica stazionarietà
adf.test(U_1_ts)
ur.kpss(U_1_ts)
ndiffs(U_1_ts)
nsdiffs(U_1_ts)
#stagionalità
u1_d <- decompose(U_1_ts)
autoplot(u1_d)

#previsione con modello arima
fit1 <- auto.arima(u_1_train_ts)
fit1
par(mfrow=c(1,1),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
plot(fit1 %>% forecast(h=11)) #plot previsoine
lines(U_1_ts)
checkresiduals(fit1)
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
res1 <- residuals(fit1)
plot(res1, type="o", pch=20, col="black", main="res1", cex.main=1.4)
hist(res1, col="black", main="Histogram", cex.main=1.4)
qqnorm(res1, cex.main=1.4)
qqline(res1)
Acf(res1, ci.col="white", main="ACF", cex.main=1.4, lag.max = 30)
a1 <- fit1 %>% forecast(h=11) %>% accuracy(U_1_ts)
a1[,c("RMSE","MAE","MAPE","MASE")]

#previsione con modello tbats
fit2 <- tbats(u_1_train_ts)
fit2
par(mfrow=c(1,1),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
plot(fit2 %>% forecast(h=11)) #plot previsoine
lines(U_1_ts)
checkresiduals(fit2)
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
res2 <- residuals(fit2)
plot(res2, type="o", pch=20, col="black", main="res1", cex.main=1.4)
hist(res2, col="black", main="Histogram", cex.main=1.4)
qqnorm(res2, cex.main=1.4)
qqline(res2)
Acf(res2, ci.col="white", main="ACF", cex.main=1.4, lag.max = 30)
a2 <- fit2 %>% forecast(h=11) %>% accuracy(U_1_ts)
a2[,c("RMSE","MAE","MAPE","MASE")]

#creazione training set (periodo da 01/01/18 a 04/10/20) con aggregazione giornaliera
U_1_ts_day <-ts(u1_z, start=c(2018,01,01), frequency = 365)
U_1_ts_day_sub <-ts(u1_z_sub, start=c(2018,01,01), frequency = 365)

#test verifica stazionarietà
adf.test(U_1_ts_day)
ur.kpss(U_1_ts_day)
ndiffs(U_1_ts)
nsdiffs(U_1_ts)
#stagionalità
u1_d1 <- decompose(U_1_ts_day)
autoplot(u1_d1)

#previsione con modello arima
fit3 <- auto.arima(U_1_ts_day_sub)
fit3
par(mfrow=c(1,1),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
plot(fit3 %>% forecast(h=77))
lines(U_1_ts_day)
checkresiduals(fit3)
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
res3 <- residuals(fit3)
plot(res3, type="o", pch=20, col="black", main="residuals", cex.main=1.4)
hist(res3, col="black", main="Histogram", cex.main=1.4)
qqnorm(res3, cex.main=1.4)
qqline(res3)
Acf(res3, ci.col='blue', main="ACF", cex.main=1.4, lag.max = 30)
a3 <- fit3 %>% forecast(h=77) %>% accuracy(U_1_ts_day)
a3[,c("RMSE","MAE","MAPE","MASE")]


#previsione con modello tbats
fit4 <- tbats(U_1_ts_day_sub)
fit4
par(mfrow=c(1,1),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
plot(fit4 %>% forecast(h=77))
lines(U_1_ts_day)
checkresiduals(fit4)
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
res4 <- residuals(fit4)
plot(res4, type="o", pch=20, col="black", main="residuals", cex.main=1.4)
hist(res4, col="black", main="Histogram", cex.main=1.4)
qqnorm(res4, cex.main=1.4)
qqline(res4)
Acf(res4, ci.col="blue", main="ACF", cex.main=1.4, lag.max = 30)
a4 <- fit4 %>% forecast(h=77) %>% accuracy(U_1_ts_day)
a4[,c("RMSE","MAE","MAPE","MASE")]

#previsione con modello prophet
k<-subset(u1_s, data_ora >= "2018-01-01" & data_ora <= "2020-10-04")
colnames(k)[which(names(k) == "consumo")] <- "y"
colnames(k)[which(names(k) == "data_ora")] <- "ds"
m <- prophet(k)
future <- make_future_dataframe(m, periods = 77)
forecast <- predict(m, future)
pf<-plot(m,forecast)
pf
prophet_plot_components(m, forecast)
sub_fore <- subset(forecast, ds >= "2020-10-05" & ds <= "2020-12-20")
sub_test <- subset(u1_s, data_ora >= "2020-10-05" & data_ora <= "2020-12-20")
MAPE(sub_fore$yhat, sub_test$consumo)

#creazione training set (periodo da 07/01/18 a 04/10/20) e aggregazione settimanale
u6_u_sub <- (u6_u[0:1007,])
u6_z_sub <- read.zoo(u6_u_sub, format = "%Y-%m-%d")
u6_z_sub <- as.xts(u6_z_sub)
u6_week_sub <- apply.weekly(u6_z_sub,sum)

#aggregazione dati su base settimanale
u6_z <- read.zoo(u6_u, format = "%Y-%m-%d")
u6_z <- as.xts(u6_z)
u6_week <- apply.weekly(u6_z,sum)

#controllo acf e pacf
par(mfrow=c(1,2),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
acf(u6_z,lag.max = 50)
pacf(u6_z, lag.max = 50)

#trasformzione dataset e training set in oggetto time series
U_6_ts <-ts(u6_week, start=c(2018,01,07), frequency = 52)
U_6_train_ts <- ts(u6_week_sub, start=c(2018,01,07), frequency = 52)

#test per verifica stazionarietà
adf.test(U_6_ts)
ur.kpss(U_6_ts) 
ndiffs(U_6_ts)
nsdiffs(U_6_ts)
#stagionalità
u6_d <- decompose(U_6_ts)
autoplot(u6_d)

#previsione con modello arima
fit5 <- auto.arima(U_6_train_ts)
fit5
par(mfrow=c(1,1),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
plot(fit5 %>% forecast(h=11)) #plot previsoine
lines(U_6_ts)
checkresiduals(fit5)
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
res5 <- residuals(fit5)
plot(res5, type="o", pch=20, col="black", main="res5", cex.main=1.4)
hist(res5, col="black", main="Histogram", cex.main=1.4)
qqnorm(res5, cex.main=1.4)
qqline(res5)
Acf(res5, ci.col="white", main="ACF", cex.main=1.4, lag.max = 30)
a5 <- fit5 %>% forecast(h=11) %>% accuracy(U_6_ts)
a5[,c("RMSE","MAE","MAPE","MASE")]

#previsione con modello tbats
fit6 <- tbats(U_6_train_ts)
fit6
par(mfrow=c(1,1),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
plot(fit6 %>% forecast(h=11)) #plot previsoine
lines(U_6_ts)
checkresiduals(fit6)
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
res6 <- residuals(fit6)
plot(res6, type="o", pch=20, col="black", main="res6", cex.main=1.4)
hist(res6, col="black", main="Histogram", cex.main=1.4)
qqnorm(res6, cex.main=1.4)
qqline(res6)
Acf(res6, ci.col="white", main="ACF", cex.main=1.4, lag.max = 30)
a6 <- fit6 %>% forecast(h=11) %>% accuracy(U_6_ts)
a6[,c("RMSE","MAE","MAPE","MASE")]

#creazione training set (periodo da 01/01/18 a 04/10/20) con aggregazione giornaliera
U_6_ts_day <-ts(u6_z, start=c(2018,01,01), frequency = 365)
U_6_ts_day_sub <-ts(u6_z_sub, start=c(2018,01,01), frequency = 365)

#test verifica stazionarietà
adf.test(U_6_ts_day)
ur.kpss(U_6_ts_day)
ndiffs(U_6_ts_day)
nsdiffs(U_6_ts_day)
#stagionalità
u6_d1 <- decompose(U_6_ts_day)
autoplot(u6_d1)

#previsione con modello arima
fit7 <- auto.arima(U_6_ts_day_sub)
fit7
par(mfrow=c(1,1),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
plot(fit7 %>% forecast(h=77))
lines(U_6_ts_day)
checkresiduals(fit7)
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
res7 <- residuals(fit7)
plot(res7, type="o", pch=20, col="black", main="residuals", cex.main=1.4)
hist(res7, col="black", main="Histogram", cex.main=1.4)
qqnorm(res7, cex.main=1.4)
qqline(res7)
Acf(res7, ci.col="blue", main="ACF", cex.main=1.4, lag.max = 30)
a7 <- fit7 %>% forecast(h=77) %>% accuracy(U_6_ts_day)
a7[,c("RMSE","MAE","MAPE","MASE")]

#previsione con modello tbats
fit8 <- tbats(U_6_ts_day_sub, D=1)
fit8
par(mfrow=c(1,1),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
plot(fit8 %>% forecast(h=77))
lines(U_6_ts_day)
checkresiduals(fit8)
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
res8 <- residuals(fit8)
plot(res8, type="o", pch=20, col="black", main="residuals", cex.main=1.4)
hist(res8, col="black", main="Histogram", cex.main=1.4)
qqnorm(res8, cex.main=1.4)
qqline(res8)
Acf(res8, ci.col="blue", main="ACF", cex.main=1.4, lag.max = 30)
a8 <- fit8 %>% forecast(h=77) %>% accuracy(U_6_ts_day)
a8[,c("RMSE","MAE","MAPE","MASE")]

#previsione con modello prophet
h<-subset(u6_u, data_ora >= "2018-01-01" & data_ora <= "2020-10-04")
colnames(h)[which(names(h) == "consumo")] <- "y"
colnames(h)[which(names(h) == "data_ora")] <- "ds"
n <- prophet(h)
future2 <- make_future_dataframe(n, periods = 77)
forecast2 <- predict(n, future2)
pf2<-plot(n,forecast2)
pf2
prophet_plot_components(n, forecast2)
sub_fore2 <- subset(forecast2, ds >= "2020-10-05" & ds <= "2020-12-20")
sub_test2 <- subset(u6_u, data_ora >= "2020-10-05" & data_ora <= "2020-12-20")
MAPE(sub_fore2$yhat, sub_test2$consumo)




###PREZZI ENERGIA ELETTRICA

#inport xlsx prezzi

prezzi_18 <- read_excel("prezzi_2018.xlsx")
prezzi_19 <- read_excel("prezzi_2019.xlsx")
prezzi_20 <- read_excel("prezzi_2020.xlsx")

#PREPROCESSING

#rimozione colonne inutili, unificazione in un unico df e rinomino colonna data

positions_18 <- c(1,3)
prezzi_18 <- prezzi_18 %>% select(positions_18)

positions_19 <- c(1,3)
prezzi_19 <- prezzi_19 %>% select(positions_19)

positions_20 <- c(1,3)
prezzi_20 <- prezzi_20 %>% select(positions_20)

p_e <- rbind(prezzi_18, prezzi_19, prezzi_20)
View(p_e)

colnames(p_e)[1] <- c("data")

#trasformo data in type:date time e aggrego il prezzo su base giornaliera

fun_insert <- function(x, pos, insert) {       
  gsub(paste0("^(.{", pos, "})(.*)$"), 
       paste0("\\1", insert, "\\2"), 
       x) 
}

p_e$data <- fun_insert(x = p_e$data, pos = 4, insert = "-") 
p_e$data <- fun_insert(x = p_e$data, pos = 7, insert = "-") 

p_e$data <- strptime(p_e$data, format="%Y-%m-%d")
p_e$data <- as.Date(p_e$data)
p_e <- aggregate(p_e["PUN"], by=p_e["data"], mean)

#eliminazione rilevazione del 29/02/2020
p_e[p_e[["data"]] == "2020-02-29", ]
p_e <- p_e[-c(790), ]
plot(p_e, type='l')
summary(p_e)

#creazione training set (periodo da 07/01/18 a 04/10/20) e aggregazione settimanale
p_e_sub <- (p_e[0:1007,])
p_e_z_sub <- read.zoo(p_e_sub, format = "%Y-%m-%d")
p_e_z_sub <- as.xts(p_e_z_sub)
p_e_week_sub <- apply.weekly(p_e_z_sub,mean)

#aggregazione dati su base settimanale
p_e_z <- read.zoo(p_e, format = "%Y-%m-%d")
P_e_z <- as.xts(p_e_z)
p_e_week <- apply.weekly(p_e_z,mean)

#controllo acf e pacf
par(mfrow=c(1,2),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
acf(p_e_z,lag.max = 50, na.action = na.pass)
pacf(p_e_z, lag.max = 50, na.action = na.pass)

#trasformzione dataset e training set in oggetto time series
p_e_ts <-ts(p_e_week, start=c(2018,01,07), frequency = 52)
p_e_train_ts <- ts(p_e_week_sub, start=c(2018,01,07), frequency = 52)

#test per verifica stazionarietà
adf.test(p_e_ts)
ur.kpss(p_e_ts) 

fit_e3 <- auto.arima(p_e_train_ts)
fit_e3
par(mfrow=c(1,1),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
plot(fit_e3 %>% forecast(h=11)) #plot previsoine
lines(p_e_ts)
checkresiduals(fit_e3)
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
res_e3 <- residuals(fit_e3)
plot(res_e3, type="o", pch=20, col="black", main="res5", cex.main=1.4)
hist(res_e3, col="black", main="Histogram", cex.main=1.4)
qqnorm(res_e3, cex.main=1.4)
qqline(res_e3)
Acf(res_e3, ci.col="blue", main="ACF", cex.main=1.4, lag.max = 30)
e3 <- fit_e3 %>% forecast(h=11) %>% accuracy(p_e_ts)
e3[,c("RMSE","MAE","MAPE","MASE")]

#previsione con modello tbats
fit_e4 <- tbats(p_e_train_ts)
fit_e4
par(mfrow=c(1,1),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
plot(fit_e4 %>% forecast(h=11)) #plot previsoine
lines(p_e_ts)
checkresiduals(fit_e4)
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
res_e4 <- residuals(fit_e4)
plot(res_e4, type="o", pch=20, col="black", main="res6", cex.main=1.4)
hist(res_e4, col="black", main="Histogram", cex.main=1.4)
qqnorm(res_e4, cex.main=1.4)
qqline(res_e4)
Acf(res_e4, ci.col="blue", main="ACF", cex.main=1.4, lag.max = 30)
e4 <- fit_e4 %>% forecast(h=11) %>% accuracy(p_e_ts)
e4[,c("RMSE","MAE","MAPE","MASE")]


#creazione training set (periodo da 01/01/18 a 04/10/20) con aggregazione giornaliera
p_e_ts_day <-ts(p_e_z, start=c(2018,01,01), frequency = 365)
p_e_ts_day_sub <-ts(p_e_z_sub, start=c(2018,01,01), frequency = 365)
frequency(p_e_ts_day_sub)

#test verifica stazionarietà
adf.test(p_e_ts_day)
ur.kpss(p_e_ts_day)

#previsione con modello arima
fit_e1 <- auto.arima(p_e_ts_day_sub)
fit_e1
par(mfrow=c(1,1),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")

plot(fit_e1 %>% forecast(h=77))
lines(p_e_ts_day)

checkresiduals(fit_e1)
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
res_e1 <- residuals(fit_e1)
plot(res_e1, type="o", pch=20, col="black", main="residuals", cex.main=1.4)
hist(res_e1, col="black", main="Histogram", cex.main=1.4)
qqnorm(res_e1, cex.main=1.4)
qqline(res_e1)
Acf(res_e1, ci.col="blue", main="ACF", cex.main=1.4, lag.max = 30)
e1 <- fit_e1 %>% forecast(h=77) %>% accuracy(p_e_ts_day)
e1[,c("RMSE","MAE","MAPE","MASE")]


#previsione con modello tbats
fit_e2 <- tbats(p_e_ts_day_sub)
fit_e2
par(mfrow=c(1,1),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
plot(fit_e2 %>% forecast(h=77))
lines(p_e_ts_day)
checkresiduals(fit_e2)
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
res_e2 <- residuals(fit_e2)
plot(res_e2, type="o", pch=20, col="black", main="residuals", cex.main=1.4)
hist(res_e2, col="black", main="Histogram", cex.main=1.4)
qqnorm(res_e2, cex.main=1.4)
qqline(res_e2)
Acf(res_e2, ci.col="blue", main="ACF", cex.main=1.4, lag.max = 30)
e2 <- fit_e2 %>% forecast(h=77) %>% accuracy(p_e_ts_day)
e2[,c("RMSE","MAE","MAPE","MASE")]

#previsione con modello prophet
k_e<-subset(p_e, data >= "2018-01-01" & data <= "2020-10-04")
colnames(k_e)[which(names(k_e) == "PUN")] <- "y"
colnames(k_e)[which(names(k_e) == "data")] <- "ds"
m_e <- prophet(k_e)
future_e <- make_future_dataframe(m_e, periods = 77)
forecast_e <- predict(m_e, future_e)
pf_e<-plot(m_e,forecast_e)
pf_e
prophet_plot_components(m_e, forecast_e)
sub_fore_e <- subset(forecast_e, ds >= "2020-10-05" & ds <= "2020-12-20")
sub_test_e <- subset(p_e, data >= "2020-10-05" & data <= "2020-12-20")
MAPE(sub_fore_e$yhat, sub_test_e$PUN)



### dashboard prezzi

#subsetting

u1_test <- (u1_s[1008:1084,])
u6_test <- (u6_u[1008:1084,])
p_e_test <- (p_e[1008:1084,])
f_test <- (forecast[1008:1084,])
f2_test <- (forecast2[1008:1084,])
fe_test <- (forecast_e[1008:1084,])

#selezione colonne

previsioni_u1 <- select(f_test, ds, yhat)
previsioni_u6 <- select(f2_test, ds, yhat)
previsioni_p_e <- select(fe_test, ds, yhat) 

#formato colonne

previsioni_u1$ds<- as.Date(previsioni_u1$ds)
previsioni_u6$ds<- as.Date(previsioni_u6$ds)
previsioni_p_e$ds<- as.Date(previsioni_p_e$ds)

#rinomino colonne selezionate

colnames(u1_test)[which(names (u1_test) == "data_ora")] <- "data"
colnames(u6_test)[which(names (u6_test) == "data_ora")] <- "data"
colnames(p_e_test)[which(names (p_e_test) == "data")] <- "data"
colnames(previsioni_u1)[which(names (previsioni_u1) == "ds")] <- "data"
colnames(previsioni_u6)[which(names (previsioni_u6) == "ds")] <- "data"
colnames(previsioni_p_e)[which(names (previsioni_p_e) == "ds")] <- "data"

p_e_test$PUN <- p_e_test$PUN / 1000
previsioni_p_e$yhat <- previsioni_p_e$yhat /1000

#merge e rename

dashboard_1 <- merge(u1_test,u6_test, all.x = T,all.y = T, by.x = "data", by.y = "data")
dashboard_2 <- merge(dashboard_1,p_e_test, all.x = T,all.y = T, by.x = "data", by.y = "data")
dashboard_3 <- merge(dashboard_2,previsioni_u1, all.x = T,all.y = T, by.x = "data", by.y = "data")
dashboard_4 <- merge(dashboard_3,previsioni_u6, all.x = T,all.y = T, by.x = "data", by.y = "data")
dashboard_5 <- merge(dashboard_4,previsioni_p_e, all.x = T,all.y = T, by.x = "data", by.y = "data")
head(dashboard_5)

colnames(dashboard_5)[which(names (dashboard_5) == "consumo.x")] <- "consumo_U1"
colnames(dashboard_5)[which(names (dashboard_5) == "consumo.y")] <- "consumo_U6"
colnames(dashboard_5)[which(names (dashboard_5) == "PUN")] <- "prezzo_energia"
colnames(dashboard_5)[which(names (dashboard_5) == "yhat.x")] <- "previsione_consumo_U1"
colnames(dashboard_5)[which(names (dashboard_5) == "yhat.y")] <- "previsione_consumo_U6"
colnames(dashboard_5)[which(names (dashboard_5) == "yhat")] <- "previsione_prezzo_energia"


dashboard_5$costo_real_U1 <- dashboard_5$consumo_U1*dashboard_5$prezzo_energia
dashboard_5$costo_real_U6 <- dashboard_5$consumo_U6*dashboard_5$prezzo_energia
dashboard_5$costo_prev_U1 <- dashboard_5$previsione_consumo_U1*dashboard_5$previsione_prezzo_energia
dashboard_5$costo_prev_U6 <- dashboard_5$previsione_consumo_U6*dashboard_5$previsione_prezzo_energia


View(dashboard_5)
summary(dashboard_5)
write_xlsx(dashboard_5,"D:/Bicocca/DS_lab\\dashboard_5.xlsx")


somma_reale_u1 <- sum(dashboard_5$costo_real_U1)
somma_prev_u1 <- sum(dashboard_5$costo_prev_U1)

somma_reale_u6 <- sum(dashboard_5$costo_real_U6)
somma_prev_u6 <- sum(dashboard_5$costo_prev_U6)