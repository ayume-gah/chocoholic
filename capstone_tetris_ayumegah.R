if (!require(googlesheets4)) install.packages("googlesheets4")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(pastecs)) install.packages("pastecs")

library(stringr)
library(gsheet)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(funModeling) 
library(plotly)

## Import From Google Sheets
ka20 <- as_tibble(gsheet2tbl('https://docs.google.com/spreadsheets/d/1cT9fhf2c7myXc_bkmmHUJHCcAS7GiI6A'))
ka21 <- as_tibble(gsheet2tbl('https://docs.google.com/spreadsheets/d/1mwh1TP3iTz8wQ3L2IXkEmzETf0AfJEBj'))
ka22 <- as_tibble(gsheet2tbl('https://docs.google.com/spreadsheets/d/1o8iMIYAqtc2IsqEy49E4wiRoZtYlA0yA'))
pl20 <- as_tibble(gsheet2tbl('https://docs.google.com/spreadsheets/d/1JuTDT7V8gNhaZVqSFATpHfX282-sJCDL'))
pl21 <- as_tibble(gsheet2tbl('https://docs.google.com/spreadsheets/d/1PKkU-ywxwagOI67zkEFAZ-lBvGjPfAx2'))
pl22 <- as_tibble(gsheet2tbl('https://docs.google.com/spreadsheets/d/1fZaIe2hs6eLXANCiafY5ECBtm_2dtA7w'))
kp20 <- as_tibble(gsheet2tbl('https://docs.google.com/spreadsheets/d/1HrgOGPS0q0kOFP5xWdDVYUrHRZXoHay8'))
kp21 <- as_tibble(gsheet2tbl('https://docs.google.com/spreadsheets/d/1CIh30RriIQGTeQXWF5bNYywms3rJUugh'))
kp22 <- as_tibble(gsheet2tbl('https://docs.google.com/spreadsheets/d/1s1MymBUQQchTbGHWFA9Y4ZGUvXRc7n52'))
co <- as_tibble(gsheet2tbl('https://docs.google.com/spreadsheets/d/1AeOIDK4kzdl2dt1LX8d-YQYUnGn2Dl1V'))

# Inspect data
periksa <- function(a){
  str(a)
  head(a)
  tail(a)
  summary(a)
  return(a)
}

periksa(ka20)
periksa(ka21)
periksa(ka22)
periksa(pl20)
periksa(pl21)
periksa(pl22)
periksa(kp20)
periksa(kp21)
periksa(kp22)
periksa(co)

names(ka20) <- make.names(names(ka20))
names(ka21) <- make.names(names(ka21))
names(ka22) <- make.names(names(ka22))
names(pl20) <- make.names(names(pl20))
names(pl21) <- make.names(names(pl21))
names(pl22) <- make.names(names(pl22))
names(kp20) <- make.names(names(kp20))
names(kp21) <- make.names(names(kp21))
names(kp22) <- make.names(names(kp22))
names(co) <- make.names(names(co))

##kereta20#
ka20<- na.omit(ka20)
#fungsi bersih untuk data ka
bersihka <- function(a){
  return <- a %>%
    filter(a$Wilayah.Kereta.Api == "Total") %>%
    select(-1,-14) %>%
    pivot_longer(cols=c(1:12), names_to="nn", values_to="Jumlah") %>%
    select(-1)
  return(return)
}

nka20 <- bersihka(ka20) 
#Making date
#fungsi pembuat data
tanggal <- function(x){
  return <- lapply(1:12, function (a) { 
    if(a<10){
      b <-paste("0",a,sep="")
    }else {
      b <- a}
    paste(x,b,sep="-")
  })
  return(return)
}
#making data
sq <- tanggal(2020)
nka20 <- nka20 %>%
  mutate(Bulan = sq) %>%
  mutate(Kereta = as.numeric(Jumlah) * 1000) %>%
  select(-1) 

##kereta21#
ka21<- na.omit(ka21)
nka21 <- bersihka(ka21)
#making data
sq <- tanggal(2021)
nka21 <- nka21 %>%
  mutate(Bulan = sq) %>%
  mutate(Kereta = as.numeric(Jumlah) * 1000) %>%
  select(-1) 

##kereta22#
ka22<- na.omit(ka22)
nka22 <- bersihka(ka22)
#making data
nka22 <- ka22 %>% 
  filter(Wilayah.Kereta.Api == "Total") %>% 
  select(1:2) %>%
  pivot_longer(cols=c(2), names_to="nn", values_to="Jumlah") %>%
  mutate(Bulan = paste("2022","01",sep="-")) %>%
  mutate(Kereta=as.numeric(Jumlah)*1000) %>%
  select(4,5)

#Merge ka
dataka <- rbind(nka20,nka21)
dataka <- rbind(dataka,nka22)

##pesawat20#
pl20<- na.omit(pl20)
#fungsi bersih data 
bersih2 <- function(a){
  return <- a %>% 
    select(1:13) %>%
    pivot_longer(cols=c(2:13), names_to="nn", values_to="Jumlah") %>%
    select(-2) 
  return(return)
}

npl20 <- bersih2(pl20)
#data per kolom
#fungsi data bandara
bandara <- function(a,b){
  return <- a %>%
    filter(a$Bandara.Utama == b) %>%
    select(-1)
  return(return)
}

pol <- bandara(npl20,"Polonia") %>%
  transmute(Polonia = as.numeric(Jumlah)) 
soeta <- bandara(npl20,"Soekarno Hatta") %>%
  transmute(Soeta = as.numeric(Jumlah))
juanda <- bandara(npl20,"Juanda") %>%
  transmute(Juanda = as.numeric(Jumlah))
rai <- bandara(npl20,"Ngurah Rai") %>%
  transmute(Ngurahrai = as.numeric(Jumlah))
hasan <- bandara(npl20,"Hasanudin") %>%
  transmute(Hasanudin = as.numeric(Jumlah))
#making data
sq <- tanggal(2020)
npl20 <- data.frame(pol,soeta,juanda,rai,hasan)
npl20 <- npl20 %>%
  mutate(Bulan = sq) %>%
  mutate(Pesawat = Polonia+Soeta+Juanda+Ngurahrai+Hasanudin) %>%
  select(-1:-5) 

##pesawat21#
pl21<- na.omit(pl21)
npl21 <- bersih2(pl21)
#data per kolom
pol <- bandara(npl21,"Polonia") %>%
  transmute(Polonia = as.numeric(Jumlah)) 
soeta <- bandara(npl21,"Soekarno Hatta") %>%
  transmute(Soeta = as.numeric(Jumlah))
juanda <- bandara(npl21,"Juanda") %>%
  transmute(Juanda = as.numeric(Jumlah))
rai <- bandara(npl21,"Ngurah Rai") %>%
  transmute(Ngurahrai = as.numeric(Jumlah))
hasan <- bandara(npl21,"Hasanudin") %>%
  transmute(Hasanudin = as.numeric(Jumlah))
#making data
sq <- tanggal(2021)
npl21 <- data.frame(pol,soeta,juanda,rai,hasan)
npl21 <- npl21 %>%
  mutate(Bulan = sq) %>%
  mutate(Pesawat = Polonia+Soeta+Juanda+Ngurahrai+Hasanudin) %>%
  select(-1:-5) 

##pesawat22#
pl22<- na.omit(pl22)
npl22 <- pl22 %>% 
  select(1:2) %>%
  pivot_longer(cols=c(2), names_to="nn", values_to="Jumlah") %>%
  select(-2)
#data per kolom
pol <- bandara(npl22,"Polonia") %>%
  transmute(Polonia = as.numeric(Jumlah)) 
soeta <- bandara(npl22,"Soekarno Hatta") %>%
  transmute(Soeta = as.numeric(Jumlah))
juanda <- bandara(npl22,"Juanda") %>%
  transmute(Juanda = as.numeric(Jumlah))
rai <- bandara(npl22,"Ngurah Rai") %>%
  transmute(Ngurahrai = as.numeric(Jumlah))
hasan <- bandara(npl22,"Hasanudin") %>%
  transmute(Hasanudin = as.numeric(Jumlah))
#Making data
npl22 <- data.frame(pol,soeta,juanda,rai,hasan)
npl22 <- npl22 %>%
  mutate(Bulan = paste("2022","01",sep="-")) %>%
  mutate(Pesawat = Polonia+Soeta+Juanda+Ngurahrai+Hasanudin) %>%
  select(-1:-5) 

#Merge pl
datapl <- rbind(npl20,npl21)
datapl <- rbind(datapl,npl22)

##kapal20#
kp20<- na.omit(kp20)
nkp20 <- bersih2(kp20)
#data per kolom
#fungsi data pelabuhan
pelabuhan <- function(a,b){
  return <- a %>% 
    filter(a$Pelabuhan.Utama == b) %>%
    select(-1)
  return(return)
}

bel <- pelabuhan(nkp20,"Belawan") %>%
  transmute(Belawan = as.numeric(Jumlah)) 
tpriok <- pelabuhan(nkp20,"Tanjung Priok") %>%
  transmute(Tanjungpriok = as.numeric(Jumlah)) 
tperak <- pelabuhan(nkp20,"Tanjung Perak") %>%
  transmute(Tanjungperak = as.numeric(Jumlah)) 
bp <- pelabuhan(nkp20,"Balikpapan") %>%
  transmute(Balikpapan = as.numeric(Jumlah)) 
mks <- pelabuhan(nkp20,"Makassar") %>%
  transmute(Makassar = as.numeric(Jumlah)) 
#making data
sq <- tanggal(2020)
nkp20 <- data.frame(bel,tpriok,tperak,bp,mks)
nkp20 <- nkp20 %>%
  mutate(Bulan = sq) %>%
  mutate(Kapal = Belawan+Tanjungpriok+Tanjungperak+Balikpapan+Makassar) %>%
  select(-1:-5) 

##kapal21#
kp21<- na.omit(kp21)
nkp21 <- bersih2(kp21)
#data per kolom
bel <- pelabuhan(nkp21,"Belawan") %>%
  transmute(Belawan = as.numeric(Jumlah)) 
tpriok <- pelabuhan(nkp21,"Tanjung Priok") %>%
  transmute(Tanjungpriok = as.numeric(Jumlah)) 
tperak <- pelabuhan(nkp21,"Tanjung Perak") %>%
  transmute(Tanjungperak = as.numeric(Jumlah)) 
bp <- pelabuhan(nkp21,"Balikpapan") %>%
  transmute(Balikpapan = as.numeric(Jumlah)) 
mks <- pelabuhan(nkp21,"Makassar") %>%
  transmute(Makassar = as.numeric(Jumlah)) 
#making data
sq <- tanggal(2021)
nkp21 <- data.frame(bel,tpriok,tperak,bp,mks)
nkp21 <- nkp21 %>%
  mutate(Bulan = sq) %>%
  mutate(Kapal = Belawan+Tanjungpriok+Tanjungperak+Balikpapan+Makassar) %>%
  select(-1:-5) 

##kapal22#
kp22<- na.omit(kp22)
nkp22 <- kp22 %>% 
  select(1:2) %>%
  pivot_longer(cols=c(2), names_to="nn", values_to="Jumlah") %>%
  select(-2) 
#data per kolom
bel <- pelabuhan(nkp22,"Belawan") %>%
  transmute(Belawan = as.numeric(Jumlah)) 
tpriok <- pelabuhan(nkp22,"Tanjung Priok") %>%
  transmute(Tanjungpriok = as.numeric(Jumlah)) 
tperak <- pelabuhan(nkp22,"Tanjung Perak") %>%
  transmute(Tanjungperak = as.numeric(Jumlah)) 
bp <- pelabuhan(nkp22,"Balikpapan") %>%
  transmute(Balikpapan = as.numeric(Jumlah)) 
mks <- pelabuhan(nkp22,"Makassar") %>%
  transmute(Makassar = as.numeric(Jumlah)) 
#making data
nkp22 <- data.frame(bel,tpriok,tperak,bp,mks)
nkp22 <- nkp22 %>%
  mutate(Bulan = paste("2022","01",sep="-")) %>%
  mutate(Kapal = Belawan+Tanjungpriok+Tanjungperak+Balikpapan+Makassar) %>%
  select(-1:-5) 

#Merge kp
datakp <- rbind(nkp20,nkp21)
datakp <- rbind(datakp,nkp22)

##union semua tabel
as_tibble(dataka)
as_tibble(datapl)
as_tibble(datakp)
dataka$Bulan <- as.character(dataka$Bulan)
datapl$Bulan <- as.character(datapl$Bulan)
datakp$Bulan <- as.character(datakp$Bulan)
databaru <- left_join(dataka, datapl, by="Bulan")
databaru <- left_join(databaru, datakp, by="Bulan")

datafix <- databaru %>%
  pivot_longer(cols=c(Kereta:Kapal), names_to="Moda", values_to="Penumpang")

str(datafix)
is_tibble(datafix) #make sure


#grafik 3 moda
ggplot(datafix, aes(Bulan,Penumpang, color = Moda,group=1)) + 
  geom_line()+
  facet_wrap(~Moda, scales = "free_y", ncol = 1)+
  theme(plot.caption = element_text(size = 8))+
  labs(x = "Bulan - Tahun", y = "Jumlah Penumpang")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # rotate x axis text
        plot.title = element_text(hjust = 0.5, vjust = -1),
        legend.position = "none")
#####rata2 total penumpang/bulan
ndatabaru <- databaru %>% 
  mutate(Penumpang = (Kereta+Pesawat+Kapal)/30) %>%
  select(-2:-4) 
#grafik rata2 total 3 moda
ggplot(ndatabaru, aes(x=Bulan, y=Penumpang,group=1)) +
  geom_area(fill="light blue") +
  labs(x = "Tahun-Bulan", y = "Jumlah Total Rata2 Penumpang Moda Transportasi") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = "none")
  
  
#tabel dan grafik covid
dataco <- co %>%
  mutate(Kasus.covid = jumlah.rata2.kasus.covid) %>%
  select(-2)
dataco$Bulan <- as.character(dataco$Bulan)

ggplot(dataco, aes(Bulan, Kasus.covid, group=1)) +
  geom_area(fill="orange") +
  labs(x = "Tahun-Bulan", y = "Jumlah Rata2 Kasus Positif Covid-19") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = "none")

#grafik 3 moda & grafik covid
ggplot(NULL) +
  geom_area(data=dataco, mapping=aes(x=Bulan,y=Kasus.covid,group=1),fill="orange",
            alpha=0.9) + #opacity colour
  geom_area(data=ndatabaru, mapping=aes(x=Bulan,y=(Penumpang/10),group=1),fill="light blue",
            alpha=0.4) +
  labs(x = "Tahun-Bulan",y="") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 65, hjust = 1),axis.text.y=element_blank(),
        legend.position = "none")
 
#grafik linear
ggplot(dataset,aes(x=dataco$Kasus.covid,y=ndatabaru$Penumpang))+
  geom_point()+
  geom_smooth(method="lm",se=T)+
  labs(x = "Kasus Positif Covid-19 (rata2 per bulan)", y = "Penumpang Moda Transportasi (rata2 per bulan)", subtitle="Regresi Linear") +
  theme_classic()

#grafik penumpang kereta
turunka <- function(a,b){
  return1 <- dataka %>% filter(Bulan == a) %>% select(Kereta)
  return2 <- dataka %>% filter(Bulan == b) %>% select(Kereta)
  return(((return2 - return1)/return1)*100)
}

titik1 <- round(turunka("2020-04","2020-03"),1)
titik2 <- round(turunka("2020-06","2020-05"),1)*-1
titik3 <- round(turunka("2020-09","2020-08"),1)
titik4 <- round(turunka("2020-10","2020-09"),1)*-1
titik5 <- round(turunka("2021-01","2020-12"),1)
titik6 <- round(turunka("2021-02","2021-01"),1)
titik7 <- round(turunka("2021-07","2021-06"),1)

ggplot(dataka, aes(Bulan, Kereta, color = Kereta,group=1)) +
  geom_line() +
  labs(x = "Tahun-Bulan", y = "Jumlah Penumpang", title="Jumlah Penumpang Kereta", subtitle="pada Periode 2020-2022") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = "none")+
  annotate(geom = "text",label="- PSBB -",x=5,y=7000000,color="red",size=4.5,angle=15)+
  annotate(geom = "text",label="PSBB transisi",x=7.5,y=9000000,color="orange",size=5)+
  annotate(geom = "text",label="PSBB ketat",x=9.5,y=8000000,color="red",size=4.7,angle=80)+
  annotate(geom = "text",label="PSBB transisi 2",x=11.5,y=12000000,color="black",size=4.7)+
  annotate(geom = "text",label="PPKM Jawa-Bali",x=13.5,y=9000000,color="brown",size=3,angle=60)+
  annotate(geom = "text",label="- PPKM Mikro -",x=16,y=10000000,color="blue",size=6.5)+
  annotate(geom = "text",label="PPKM Darurat",x=19,y=11000000,color="purple",size=4.5,angle=90)+
  annotate(geom = "text",label="-- PPKM Level --",x=22,y=5000000,color="green",size=7)+
  annotate(geom = "text",label=paste("-",titik1,"%"),x=4.5,y=9000000,color="red",size=5)+
  annotate(geom = "text",label=paste("+",titik2,"%"),x=6.5,y=12000000,color="orange",size=5)+
  annotate(geom = "text",label=paste("-",titik3,"%"),x=9.5,y=13000000,color="red",size=5)+
  annotate(geom = "text",label=paste("+",titik4,"%"),x=10.5,y=14000000,color="black",size=5)+
  annotate(geom = "text",label=paste("-",titik5,"%"),x=13.5,y=13000000,color="brown",size=5)+
  annotate(geom = "text",label=paste("-",titik6,"%"),x=14.5,y=14000000,color="blue",size=5)+
  annotate(geom = "text",label=paste("-",titik7,"%"),x=19.5,y=17000000,color="purple",size=5)

#grafik penumpang pesawat
turunpl <- function(a,b){
  return1 <- datapl %>% filter(Bulan == a) %>% select(Pesawat)
  return2 <- datapl %>% filter(Bulan == b) %>% select(Pesawat)
  return(((return2 - return1)/return1)*100)
}

titik1 <- round(turunpl("2020-04","2020-03"),1)
titik2 <- round(turunpl("2020-06","2020-05"),1)*-1
titik3 <- round(turunpl("2020-09","2020-08"),1)
titik4 <- round(turunpl("2020-10","2020-09"),1)*-1
titik5 <- round(turunpl("2021-01","2020-12"),1)
titik6 <- round(turunpl("2021-02","2021-01"),1)
titik7 <- round(turunpl("2021-07","2021-06"),1)

ggplot(datapl, aes(Bulan, Pesawat, color = Pesawat, group=1)) +
  geom_line() +
  labs(x = "Tahun-Bulan", y = "Jumlah Penumpang", title="Jumlah Penumpang Pesawat", subtitle="pada Periode 2020-2022") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = "none")+
  annotate(geom = "text",label="- PSBB -",x=5,y=300000,color="red",size=4.5,angle=15)+
  annotate(geom = "text",label="PSBB transisi",x=7.5,y=300000,color="orange",size=5)+
  annotate(geom = "text",label="PSBB ketat",x=9.5,y=500000,color="red",size=4.7,angle=80)+
  annotate(geom = "text",label="PSBB transisi 2",x=11.5,y=1100000,color="black",size=4.7)+
  annotate(geom = "text",label="PPKM Jawa-Bali",x=13.5,y=1500000,color="brown",size=3,angle=60)+
  annotate(geom = "text",label="- PPKM Mikro -",x=16,y=1000000,color="blue",size=6.5)+
  annotate(geom = "text",label="PPKM Darurat",x=19,y=1400000,color="purple",size=4.5,angle=90)+
  annotate(geom = "text",label="-- PPKM Level --",x=22,y=400000,color="green",size=7)+
  annotate(geom = "text",label=paste("-",titik1,"%"),x=4.5,y=500000,color="red",size=5)+
  annotate(geom = "text",label=paste("+",titik2,"%"),x=6.5,y=200000,color="orange",size=5)+
  annotate(geom = "text",label=paste("-",titik3,"%"),x=9.5,y=900000,color="red",size=5)+
  annotate(geom = "text",label=paste("+",titik4,"%"),x=10.5,y=1000000,color="black",size=5)+
  annotate(geom = "text",label=paste("-",titik5,"%"),x=13.5,y=900000,color="brown",size=5)+
  annotate(geom = "text",label=paste("-",titik6,"%"),x=14.5,y=800000,color="blue",size=5)+
  annotate(geom = "text",label=paste("-",titik7,"%"),x=19.5,y=400000,color="purple",size=5)

#grafik penumpang kapal
turunkp <- function(a,b){
  return1 <- datakp %>% filter(Bulan == a) %>% select(Kapal)
  return2 <- datakp %>% filter(Bulan == b) %>% select(Kapal)
  return(((return2 - return1)/return1)*100)
}

titik1 <- round(turunkp("2020-04","2020-03"),1)
titik2 <- round(turunkp("2020-06","2020-05"),1)*-1
titik3 <- round(turunkp("2020-09","2020-08"),1)*-1
titik4 <- round(turunkp("2020-10","2020-09"),1)
titik5 <- round(turunkp("2021-01","2020-12"),1)
titik6 <- round(turunkp("2021-02","2021-01"),1)
titik7 <- round(turunkp("2021-07","2021-06"),1)

ggplot(datakp, aes(Bulan, Kapal, color = Kapal, group=1)) +
  geom_line() +
  labs(x = "Tahun-Bulan", y = "Jumlah Penumpang", title="Jumlah Penumpang Kapal", subtitle="pada Periode 2020-2022") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = "none")+
  annotate(geom = "text",label="- PSBB -",x=5,y=20000,color="red",size=4.5,angle=15)+
  annotate(geom = "text",label="PSBB transisi",x=7.5,y=10000,color="orange",size=5)+
  annotate(geom = "text",label="PSBB ketat",x=9.5,y=20000,color="red",size=4.7,angle=80)+
  annotate(geom = "text",label="PSBB transisi 2",x=11.5,y=30000,color="black",size=4.7)+
  annotate(geom = "text",label="PPKM Jawa-Bali",x=13.5,y=65000,color="brown",size=3,angle=60)+
  annotate(geom = "text",label="- PPKM Mikro -",x=16,y=35000,color="blue",size=6.5)+
  annotate(geom = "text",label="PPKM Darurat",x=19,y=27000,color="purple",size=4.5,angle=90)+
  annotate(geom = "text",label="-- PPKM Level --",x=22,y=7000,color="green",size=7)+
  annotate(geom = "text",label=paste("-",titik1,"%"),x=4.5,y=25000,color="red",size=5)+
  annotate(geom = "text",label=paste("+",titik2,"%"),x=6.5,y=20000,color="orange",size=5)+
  annotate(geom = "text",label=paste("-",titik3,"%"),x=9.5,y=40000,color="red",size=5)+
  annotate(geom = "text",label=paste("+",titik4,"%"),x=10.5,y=25000,color="black",size=5)+
  annotate(geom = "text",label=paste("-",titik5,"%"),x=13.5,y=55000,color="brown",size=5)+
  annotate(geom = "text",label=paste("-",titik6,"%"),x=14.5,y=30000,color="blue",size=5)+
  annotate(geom = "text",label=paste("-",titik7,"%"),x=19.5,y=47000,color="purple",size=5)
