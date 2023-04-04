library(shiny)
library(dplyr)
library(maps)
library(ggplot2)
library(leaflet)
library(tidyverse)
library(plotly)
library(lubridate)
library(shinydashboardPlus)
library(readxl)
library(openair)
library(forecast) #ggplotly
library(imputeTS) #do zamiany wartosci NA 
library(shinydashboard)


# DELHI PUNKT POMIAROWY NR 1

#dane z czujnika Delhi Institute of Tool Engineering, Wazirpur, Delhi, Delhi, India
Delhi <- read_excel("C:/Users/Gaba/OneDrive/Dokumenty/praca inżynierska/inżynierka/praca_inzynierska/delhi-institute of tool engineering, wazirpur, delhi, delhi, india-air-quality.xlsx")

#przycinanie danych do zanieczyszczen pylami pm2.5
Delhi_pm25 <- Delhi %>%
  select(date, pm25)


#2019:
Delhi_2019 <- selectByDate( 
  Delhi_pm25,
  start = "2019-01-01",
  end = "2019-12-31",
)

#2020:
Delhi_2020 <- selectByDate( 
  Delhi_pm25,
  start = "2020-01-01",
  end = "2020-12-31",
)

#2021:
Delhi_2021 <- selectByDate( 
  Delhi_pm25,
  start = "2021-01-01",
  end = "2021-12-31",
)


# DELHI PKT1 WSZYSTKIE LATA 
#2019-2021
Delhi_3lata <- selectByDate( 
  Delhi_pm25,
  start = "2019-01-01",
  end = "2021-12-31",
)

#DELHI PKT1 ŚREDNIA DLA KAŻDEGO ROKU
sredniaDelhi_2019<-as.data.frame(apply(Delhi_2019[1:364,2], 2, mean, na.rm=TRUE))
sredniaDelhi_2020<-as.data.frame(apply(Delhi_2020[1:363,2], 2, mean, na.rm=TRUE))
sredniaDelhi_2021<-as.data.frame(apply(Delhi_2021[1:362,2], 2, mean, na.rm=TRUE))

#DELHI PKT 1 ŚREDNIA Z 3 LAT 
srednia3lata_Delhi<-as.data.frame((sredniaDelhi_2019+sredniaDelhi_2020+sredniaDelhi_2021)/3)

#wykres sezonowości dla pkt 1 Delhi
Delhi_3lata_ts <- ts(Delhi_3lata["pm25"], start = c(2019, 1), end = c(2021, 12), frequency = 12)

season_Delhi1 <- ggseasonplot(Delhi_3lata_ts, 
                              year.labels=FALSE, 
                              season.labels = c("st", "lut", "mrz", "kw", "maj", "cz", "lip", "sier", "wrz", "paź", "lis", "gr"))+
  geom_line(size = 0.3) +
  geom_point(size = 1.7, alpha=0.5) +
  #ggtitle("Delhi") + 
  xlab("miesiąc") +
  scale_color_brewer(name= "Rok", palette = "Set1")

#Delhi1_gg <- ggplotly(season_Delhi1)  

#obliczanie średniej miesięcznej do wyswietlania w boxach wg miesiąca i roku 
Delhi_2019lata_ts <- ts(Delhi_2019["pm25"], start = c(2019, 1), end = c(2019, 12), frequency = 12)
Delhi1_2019mean <- tapply(Delhi_2019lata_ts,cycle(Delhi_2019lata_ts),mean,na.rm=TRUE)

Delhi_2020lata_ts <- ts(Delhi_2020["pm25"], start = c(2020, 1), end = c(2020, 12), frequency = 12)
Delhi1_2020mean <- tapply(Delhi_2020lata_ts,cycle(Delhi_2020lata_ts),mean,na.rm=TRUE)

Delhi_2021lata_ts <- ts(Delhi_2021["pm25"], start = c(2021, 1), end = c(2021, 12), frequency = 12)
Delhi1_2021mean <- tapply(Delhi_2021lata_ts,cycle(Delhi_2021lata_ts),mean,na.rm=TRUE)





#DELHI PUNKT POMIAROWY NR 2

#dane z czujnika punjabi-bagh, delhi, delhi, india-air-quality
Delhi2 <- read_excel("C:/Users/Gaba/OneDrive/Dokumenty/praca inżynierska/inżynierka/praca_inzynierska/punjabi-bagh, delhi, delhi, india-air-quality.xlsx")

#przycinanie danych do zanieczyszczen pylami pm2.5
Delhi2_pm25 <- Delhi2 %>%
  select(date, pm25)

#2019:
Delhi2_2019 <- selectByDate( 
  Delhi2_pm25,
  start = "2019-01-01",
  end = "2019-12-31",
)

#2020:
Delhi2_2020 <- selectByDate( 
  Delhi2_pm25,
  start = "2020-01-01",
  end = "2020-12-31",
)

#2021:
Delhi2_2021 <- selectByDate( 
  Delhi2_pm25,
  start = "2021-01-01",
  end = "2021-12-31",
)

# DELHI PKT 2 WSZYSTKIE LATA 
#2019-2021
Delhi2_3lata <- selectByDate( 
  Delhi2_pm25,
  start = "2019-01-01",
  end = "2021-12-31",
)

Delhi2_3lata <- na.omit(Delhi2_3lata)

#DELHI PKT 2 ŚREDNIA DLA KAŻDEGO ROKU
sredniaDelhi2_2019<-as.data.frame(apply(Delhi2_2019[1:364,2], 2, mean, na.rm = TRUE))
sredniaDelhi2_2020<-as.data.frame(apply(Delhi2_2020[1:365,2], 2, mean, na.rm = TRUE))
sredniaDelhi2_2021<-as.data.frame(apply(Delhi2_2021[1:365,2], 2, mean, na.rm = TRUE))

#DELHI PKT 2 ŚREDNIA Z 3 LAT 
srednia3lata2_Delhi<-as.data.frame((sredniaDelhi2_2019+sredniaDelhi2_2020+sredniaDelhi2_2021)/3)

#wykres sezonowości dla pkt 2 Delhi
Delhi2_3lata_ts <- ts(Delhi2_3lata["pm25"], start = c(2019, 1), end = c(2021, 12), frequency = 12)

season_Delhi2 <- ggseasonplot(Delhi2_3lata_ts,
                              year.labels=FALSE, 
                              season.labels = c("st", "lut", "mrz", "kw", "maj", "cz", "lip", "sier", "wrz", "paź", "lis", "gr"))+
  geom_line(size = 0.3) +
  geom_point(size = 1.7, alpha=0.5) +
  #ggtitle("Punjabi Bagh, Delhi") + 
  xlab("miesiąc") +
  scale_color_brewer(name= "Rok", palette = "Set1")

#Delhi2_gg <- ggplotly(season_Delhi2) 



#obliczanie średniej miesięcznej do wyswietlania w boxach wg miesiąca i roku 
Delhi2_2019lata_ts <- ts(Delhi2_2019["pm25"], start = c(2019, 1), end = c(2019, 12), frequency = 12)
Delhi2_2019mean <- tapply(Delhi2_2019lata_ts,cycle(Delhi2_2019lata_ts),mean,na.rm=TRUE)

Delhi2_2020lata_ts <- ts(Delhi2_2020["pm25"], start = c(2020, 1), end = c(2020, 12), frequency = 12)
Delhi2_2020mean <- tapply(Delhi2_2020lata_ts,cycle(Delhi2_2020lata_ts),mean,na.rm=TRUE)

Delhi2_2021lata_ts <- ts(Delhi2_2021["pm25"], start = c(2021, 1), end = c(2021, 12), frequency = 12)
Delhi2_2021mean <- tapply(Delhi2_2021lata_ts,cycle(Delhi2_2021lata_ts),mean,na.rm=TRUE)










#DELHI PUNKT POMIAROWY NR 3

#dane z czujnika punjabi-bagh, delhi, delhi, india-air-quality
Delhi3 <- read_excel("C:/Users/Gaba/OneDrive/Dokumenty/praca inżynierska/inżynierka/praca_inzynierska/satyawati-college, delhi, delhi, india-air-quality.xlsx")

#przycinanie danych do zanieczyszczen pylami pm2.5
Delhi3_pm25 <- Delhi3 %>%
  select(date, pm25)

#2019:
Delhi3_2019 <- selectByDate( 
  Delhi3_pm25,
  start = "2019-01-01",
  end = "2019-12-31",
)

#2020:
Delhi3_2020 <- selectByDate( 
  Delhi3_pm25,
  start = "2020-01-01",
  end = "2020-12-31",
)

#2021:
Delhi3_2021 <- selectByDate( 
  Delhi3_pm25,
  start = "2021-01-01",
  end = "2021-12-31",
)
Delhi3_2021 <- na.omit(Delhi3_2021)

# DELHI PKT 3 WSZYSTKIE LATA 
#2019-2021
Delhi3_3lata <- selectByDate( 
  Delhi3_pm25,
  start = "2019-01-01",
  end = "2021-12-31",
)

#Delhi3_3lata <- na.omit(Delhi3_3lata)

#obliczanie sredniego zanieczyszczenia dla Delhi2_2019,2020,2021
sredniaDelhi3_2019<-as.data.frame(apply(Delhi3_2019[1:364,2], 2, mean, na.rm = TRUE))
sredniaDelhi3_2020<-as.data.frame(apply(Delhi3_2020[1:363,2], 2, mean, na.rm = TRUE))
sredniaDelhi3_2021<-as.data.frame(apply(Delhi3_2021[1:362,2], 2, mean, na.rm = TRUE))

#SREDNIA DELHI PKT3 DLA 3 PUNKTOW
srednia3lata3_Delhi<-as.data.frame((sredniaDelhi3_2019+sredniaDelhi3_2020+sredniaDelhi3_2021)/3)


#wykres sezonowości dla pkt 3 Delhi
Delhi3_3lata_ts <- ts(Delhi3_3lata["pm25"], start = c(2019, 1), end = c(2021, 12), frequency = 12)

season_Delhi3 <- ggseasonplot(Delhi3_3lata_ts, 
                              year.labels=FALSE, 
                              season.labels = c("st", "lut", "mrz", "kw", "maj", "cz", "lip", "sier", "wrz", "paź", "lis", "gr"))+
  geom_line(size = 0.3) +
  geom_point(size = 1.7, alpha=0.5) +
  ggtitle("Wykresy sezonowości, Delhi") + 
  xlab("miesiąc") +
  scale_color_brewer(name= "Rok", palette = "Set1")

#Delhi3_gg <- ggplotly(season_Delhi3) 


#obliczanie średniej miesięcznej do wyswietlania w boxach wg miesiąca i roku 
Delhi3_2019lata_ts <- ts(Delhi3_2019["pm25"], start = c(2019, 1), end = c(2019, 12), frequency = 12)
Delhi3_2019mean <- tapply(Delhi3_2019lata_ts,cycle(Delhi3_2019lata_ts),mean,na.rm=TRUE)

Delhi3_2020lata_ts <- ts(Delhi3_2020["pm25"], start = c(2020, 1), end = c(2020, 12), frequency = 12)
Delhi3_2020mean <- tapply(Delhi3_2020lata_ts,cycle(Delhi3_2020lata_ts),mean,na.rm=TRUE)

Delhi3_2021lata_ts <- ts(Delhi3_2021["pm25"], start = c(2021, 1), end = c(2021, 12), frequency = 12)
Delhi3_2021mean <- tapply(Delhi3_2021lata_ts,cycle(Delhi3_2021lata_ts),mean,na.rm=TRUE)



#srednia dla 3 czujnikow z wyszczegolnieniem roku i miesiaca
#trzeba usunąc NA z 2021, czujnik nr 3

DELHI_2019_SR <- as.data.frame((Delhi1_2019mean+Delhi2_2019mean+Delhi3_2019mean)/3)
DELHI_2020_SR <- as.data.frame((Delhi1_2020mean+Delhi2_2020mean+Delhi3_2020mean)/3)
DELHI_2021_SR <- as.data.frame((Delhi1_2021mean+Delhi2_2021mean+Delhi3_2021mean)/3)


colnames(DELHI_2019_SR) <- c('2019')
colnames(DELHI_2020_SR) <- c('2020')
colnames(DELHI_2021_SR) <- c('2021')

DELHI_SR <- data.frame(DELHI_2019_SR,DELHI_2020_SR,DELHI_2021_SR)

#nadawanie nazw kolumn co przyda sie przy polaczeniu ich w jedna data frame ORAZ PRZY interaktywnosci
# do wyboru miesiaca i roku 
rownames(DELHI_SR) <- c("styczeń","luty","marzec","kwiecień","maj","czerwiec","lipiec","sierpień","wrzesień","październik","listopad","grudzień")
colnames(DELHI_SR) <- c("2019","2020","2021")


#PEKIN PUNKT POMIAROWY NR 1

#dane z czujnika beijing-south , huai'an-air-quality
Pekin <- read_excel("C:/Users/Gaba/OneDrive/Dokumenty/praca inżynierska/inżynierka/praca_inzynierska/yongledianzhen,-tongzhou, beijing-air-quality.xlsx")

#przycinanie danych do zanieczyszczen pylami pm2.5
Pekin_pm25 <- Pekin %>%
  select(date, pm25)

#2019:
Pekin1_2019 <- selectByDate( 
  Pekin_pm25,
  start = "2019-01-01",
  end = "2019-12-31",
)

#2020:
Pekin1_2020 <- selectByDate( 
  Pekin_pm25,
  start = "2020-01-01",
  end = "2020-12-31",
)

#2021:
Pekin1_2021 <- selectByDate( 
  Pekin_pm25,
  start = "2021-01-01",
  end = "2021-12-31",
)

#2019-2021
Pekin_3lata <- selectByDate( 
  Pekin_pm25,
  start = "2019-01-01",
  end = "2021-12-31",
)


#obliczanie sredniego zanieczyszczenia dla Pekin_2019,2020,2021
sredniaPekin_2019<-as.data.frame(apply(Pekin1_2019[1:364,2], 2, mean, na.rm = TRUE))
sredniaPekin_2020<-as.data.frame(apply(Pekin1_2020[1:359,2], 2, mean, na.rm = TRUE))
sredniaPekin_2021<-as.data.frame(apply(Pekin1_2021[1:362,2], 2, mean, na.rm = TRUE))

srednia3lata_Pekin<-as.data.frame((sredniaPekin_2019+sredniaPekin_2020+sredniaPekin_2021)/3)

#wykres sezonowości dla pkt 1 Pekin
Pekin_3lata_ts <- ts(Pekin_3lata["pm25"], start = c(2019, 1), end = c(2021, 12), frequency = 12)

season_Pekin1 <- ggseasonplot(Pekin_3lata_ts, 
                              year.labels=FALSE, 
                              season.labels = c("st", "lut", "mrz", "kw", "maj", "cz", "lip", "sier", "wrz", "paź", "lis", "gr"))+
  geom_line(size = 0.3) +
  geom_point(size = 1.7, alpha=0.5) +
  #ggtitle("Pekin") + 
  xlab("miesiąc") +
  scale_color_brewer(name= "Rok", palette = "Set1")

#Pekin1_gg <- ggplotly(season_Pekin1)


#obliczanie średniej miesięcznej do wyswietlania w boxach wg miesiąca i roku 
Pekin1_2019lata_ts <- ts(Pekin1_2019["pm25"], start = c(2019, 1), end = c(2019, 12), frequency = 12)
Pekin1_2019mean <- tapply(Pekin1_2019lata_ts,cycle(Pekin1_2019lata_ts),mean,na.rm=TRUE)

Pekin1_2020lata_ts <- ts(Pekin1_2020["pm25"], start = c(2020, 1), end = c(2020, 12), frequency = 12)
Pekin1_2020mean <- tapply(Pekin1_2020lata_ts,cycle(Pekin1_2020lata_ts),mean,na.rm=TRUE)

Pekin1_2021lata_ts <- ts(Pekin1_2021["pm25"], start = c(2021, 1), end = c(2021, 12), frequency = 12)
Pekin1_2021mean <- tapply(Pekin1_2021lata_ts,cycle(Pekin1_2021lata_ts),mean,na.rm=TRUE)







#PEKIN PUNKT POMIAROWY NR 2

#dane z czujnika pinggu-town, beijing-air-quality
Pekin2 <- read_excel("C:/Users/Gaba/OneDrive/Dokumenty/praca inżynierska/inżynierka/praca_inzynierska/pinggu-town, beijing-air-quality.xlsx")

#przycinanie danych do zanieczyszczen pylami pm2.5
Pekin2_pm25 <- Pekin2 %>%
  select(date, pm25)

#2019:
Pekin2_2019 <- selectByDate( 
  Pekin2_pm25,
  start = "2019-01-01",
  end = "2019-12-31",
)

#2020:
Pekin2_2020 <- selectByDate( 
  Pekin2_pm25,
  start = "2020-01-01",
  end = "2020-12-31",
)

#2021:
Pekin2_2021 <- selectByDate( 
  Pekin2_pm25,
  start = "2021-01-01",
  end = "2021-12-31",
)

Pekin2_3lata <- selectByDate( 
  Pekin2_pm25,
  start = "2019-01-01",
  end = "2021-12-31"
)

#obliczanie sredniego zanieczyszczenia dla Pekin2_2019,2020,2021
sredniaPekin2_2019<-as.data.frame(apply(Pekin2_2019[1:364,2], 2, mean,na.rm = TRUE))
sredniaPekin2_2020<-as.data.frame(apply(Pekin2_2020[1:363,2], 2, mean,na.rm = TRUE))
sredniaPekin2_2021<-as.data.frame(apply(Pekin2_2021[1:362,2], 2, mean,na.rm = TRUE))

srednia3lata2_Pekin<-as.data.frame((sredniaPekin2_2019+sredniaPekin2_2020+sredniaPekin2_2021)/3)

#wykres sezonowości dla pkt 2 Pekin
Pekin2_3lata_ts <- ts(Pekin2_3lata["pm25"], start = c(2019, 1), end = c(2021, 12), frequency = 12)

season_Pekin2 <- ggseasonplot(Pekin2_3lata_ts, 
                              year.labels=FALSE, 
                              season.labels = c("st", "lut", "mrz", "kw", "maj", "cz", "lip", "sier", "wrz", "paź", "lis", "gr"))+
  geom_line(size = 0.3) +
  geom_point(size = 1.7, alpha=0.5) +
  #ggtitle("Pinggu New Town, Pekin") + 
  xlab("miesiąc") +
  scale_color_brewer(name= "Rok", palette = "Set1")

#Pekin2_gg <- ggplotly(season_Pekin2) 


#obliczanie średniej miesięcznej do wyswietlania w boxach wg miesiąca i roku 
Pekin2_2019lata_ts <- ts(Pekin2_2019["pm25"], start = c(2019, 1), end = c(2019, 12), frequency = 12)
Pekin2_2019mean <- tapply(Pekin2_2019lata_ts,cycle(Pekin2_2019lata_ts),mean,na.rm=TRUE)

Pekin2_2020lata_ts <- ts(Pekin2_2020["pm25"], start = c(2020, 1), end = c(2020, 12), frequency = 12)
Pekin2_2020mean <- tapply(Pekin2_2020lata_ts,cycle(Pekin2_2020lata_ts),mean,na.rm=TRUE)

Pekin2_2021lata_ts <- ts(Pekin2_2021["pm25"], start = c(2021, 1), end = c(2021, 12), frequency = 12)
Pekin2_2021mean <- tapply(Pekin2_2021lata_ts,cycle(Pekin2_2021lata_ts),mean,na.rm=TRUE)








#PEKIN PUNKT POMIAROWY NR 3

#dane z czujnika yanqing-town, beijing-air-quality
Pekin3 <- read_excel("C:/Users/Gaba/OneDrive/Dokumenty/praca inżynierska/inżynierka/praca_inzynierska/yanqing-town, beijing-air-quality.xlsx")

#przycinanie danych do zanieczyszczen pylami pm2.5
Pekin3_pm25 <- Pekin3 %>%
  select(date, pm25)

#2019:
Pekin3_2019 <- selectByDate( 
  Pekin3_pm25,
  start = "2019-01-01",
  end = "2019-12-31",
)

#2020:
Pekin3_2020 <- selectByDate( 
  Pekin3_pm25,
  start = "2020-01-01",
  end = "2020-12-31",
)

#2021:
Pekin3_2021 <- selectByDate( 
  Pekin3_pm25,
  start = "2021-01-01",
  end = "2021-12-31",
)

Pekin3_3lata <- selectByDate( 
  Pekin3_pm25,
  start = "2019-01-01",
  end = "2021-12-31"
)

#obliczanie sredniego zanieczyszczenia dla Pekin2_2019,2020,2021
sredniaPekin3_2019<-as.data.frame(apply(Pekin3_2019[1:364,2], 2, mean,na.rm=TRUE))
sredniaPekin3_2020<-as.data.frame(apply(Pekin3_2020[1:359,2], 2, mean,na.rm=TRUE))
sredniaPekin3_2021<-as.data.frame(apply(Pekin3_2021[1:362,2], 2, mean,na.rm=TRUE))

srednia3lata3_Pekin<-as.data.frame((sredniaPekin3_2019+sredniaPekin3_2020+sredniaPekin3_2021)/3)

#wykres sezonowości dla pkt 3 Pekin
Pekin3_3lata_ts <- ts(Pekin3_3lata["pm25"], start = c(2019, 1), end = c(2021, 12), frequency = 12)

season_Pekin3 <- ggseasonplot(Pekin3_3lata_ts, 
                              year.labels=FALSE, 
                              season.labels = c("st", "lut", "mrz", "kw", "maj", "cz", "lip", "sier", "wrz", "paź", "lis", "gr"))+
  geom_line(size = 0.3) +
  geom_point(size = 1.7, alpha=0.5) +
  ggtitle("Wykresy sezonowości, Pekin") + 
  xlab("miesiąc") +
  scale_color_brewer(name= "Rok", palette = "Set1")

#Pekin3_gg <- ggplotly(season_Pekin3) 



#obliczanie średniej miesięcznej do wyswietlania w boxach wg miesiąca i roku 
Pekin3_2019lata_ts <- ts(Pekin3_2019["pm25"], start = c(2019, 1), end = c(2019, 12), frequency = 12)
Pekin3_2019mean <- tapply(Pekin3_2019lata_ts,cycle(Pekin3_2019lata_ts),mean,na.rm=TRUE)

Pekin3_2020lata_ts <- ts(Pekin3_2020["pm25"], start = c(2020, 1), end = c(2020, 12), frequency = 12)
Pekin3_2020mean <- tapply(Pekin3_2020lata_ts,cycle(Pekin3_2020lata_ts),mean,na.rm=TRUE)

Pekin3_2021lata_ts <- ts(Pekin3_2021["pm25"], start = c(2021, 1), end = c(2021, 12), frequency = 12)
Pekin3_2021mean <- tapply(Pekin3_2021lata_ts,cycle(Pekin3_2021lata_ts),mean,na.rm=TRUE)



#srednia dla 3 czujnikow z wyszczegolnieniem roku i miesiaca
#trzeba usunąc NA z 2021, czujnik nr 3

Pekin_2019_SR <- as.data.frame((Pekin1_2019mean+Pekin2_2019mean+Pekin1_2019mean)/3)
Pekin_2020_SR <- as.data.frame((Pekin1_2020mean+Pekin2_2020mean+Pekin1_2020mean)/3)
Pekin_2021_SR <- as.data.frame((Pekin1_2021mean+Pekin2_2021mean+Pekin1_2021mean)/3)


colnames(Pekin_2019_SR) <- c('2019')
colnames(Pekin_2020_SR) <- c('2020')
colnames(Pekin_2021_SR) <- c('2021')

PEKIN_SR <- data.frame(Pekin_2019_SR,Pekin_2020_SR,Pekin_2021_SR)

#nadawanie nazw kolumn co przyda sie przy polaczeniu ich w jedna data frame ORAZ PRZY interaktywnosci
# do wyboru miesiaca i roku 
rownames(PEKIN_SR) <- c("styczeń","luty","marzec","kwiecień","maj","czerwiec","lipiec","sierpień","wrzesień","październik","listopad","grudzień")
colnames(PEKIN_SR) <- c("2019","2020","2021")







#Helsinki PUNKT POMIAROWY NR 1

#dane z czujnika helsinki centrum
Helsinki <- read_excel("C:/Users/Gaba/OneDrive/Dokumenty/praca inżynierska/inżynierka/praca_inzynierska/helsinki-air-quality.xlsx")

#przycinanie danych do zanieczyszczen pylami pm2.5
Helsinki_pm25 <- Helsinki %>%
  select(date, pm25)


#2019:
Helsinki_2019 <- selectByDate( 
  Helsinki_pm25,
  start = "2019-01-01",
  end = "2019-12-31",
)

#2020:
Helsinki_2020 <- selectByDate( 
  Helsinki_pm25,
  start = "2020-01-01",
  end = "2020-12-31",
)

#2021:
Helsinki_2021 <- selectByDate( 
  Helsinki_pm25,
  start = "2021-01-01",
  end = "2021-12-31",
)

#2019-2021
Helsinki_3lata <- selectByDate( 
  Helsinki_pm25,
  start = "2019-01-01",
  end = "2021-12-31",
)


#obliczanie sredniego zanieczyszczenia dla Helsinki_2019,2020,2021
sredniaHelsinki_2019<-as.data.frame(apply(Helsinki_2019[1:364,2], 2, mean, na.rm=TRUE)) 
sredniaHelsinki_2020<-as.data.frame(apply(Helsinki_2020[1:365,2], 2, mean, na.rm=TRUE))
sredniaHelsinki_2021<-as.data.frame(apply(Helsinki_2021[1:365,2], 2, mean, na.rm=TRUE))

srednia3lata_Helsinki <- as.data.frame((sredniaHelsinki_2019+sredniaHelsinki_2020+sredniaHelsinki_2021)/3)

#wykres sezonowości dla pkt 1 Helsinki
Helsinki_3lata_ts <- ts(Helsinki_3lata["pm25"], start = c(2019, 1), end = c(2021, 12), frequency = 12)

season_Helsinki1 <- ggseasonplot(Helsinki_3lata_ts, 
                                 year.labels=FALSE, 
                                 season.labels = c("st", "lut", "mrz", "kw", "maj", "cz", "lip", "sier", "wrz", "paź", "lis", "gr"))+
  geom_line(size = 0.3) +
  geom_point(size = 1.7, alpha=0.5) +
  #ggtitle("Helsinki") + 
  xlab("miesiąc") +
  scale_color_brewer(name= "Rok", palette = "Set1")

#Hlesinki1_gg <- ggplotly(season_Helsinki1) 



#obliczanie średniej miesięcznej do wyswietlania w boxach wg miesiąca i roku 
Helsinki1_2019lata_ts <- ts(Helsinki_2019["pm25"], start = c(2019, 1), end = c(2019, 12), frequency = 12)
Helsinki1_2019mean <- tapply(Helsinki1_2019lata_ts,cycle(Helsinki1_2019lata_ts),mean,na.rm=TRUE)

Helsinki1_2020lata_ts <- ts(Helsinki_2020["pm25"], start = c(2020, 1), end = c(2020, 12), frequency = 12)
Helsinki1_2020mean <- tapply(Helsinki1_2020lata_ts,cycle(Helsinki1_2020lata_ts),mean,na.rm=TRUE)

Helsinki1_2021lata_ts <- ts(Helsinki_2021["pm25"], start = c(2021, 1), end = c(2021, 12), frequency = 12)
Helsinki1_2021mean <- tapply(Helsinki1_2021lata_ts,cycle(Helsinki1_2021lata_ts),mean,na.rm=TRUE)






#Helsinki PUNKT POMIAROWY NR 2

#dane z czujnika kallio-2, helsinki 60.18002146137783, 24.949833908093428
Helsinki2 <- read_excel("C:/Users/Gaba/OneDrive/Dokumenty/praca inżynierska/inżynierka/praca_inzynierska/kallio-2, helsinki, finland-air-quality.xlsx")

#przycinanie danych do zanieczyszczen pylami pm2.5
Helsinki2_pm25 <- Helsinki2 %>%
  select(date, pm25)

#2019:
Helsinki2_2019 <- selectByDate( 
  Helsinki2_pm25,
  start = "2019-01-01",
  end = "2019-12-31",
)

#2020:
Helsinki2_2020 <- selectByDate( 
  Helsinki2_pm25,
  start = "2020-01-01",
  end = "2020-12-31",
)

#2021:
Helsinki2_2021 <- selectByDate( 
  Helsinki2_pm25,
  start = "2021-01-01",
  end = "2021-12-31",
)

#2019-2021
Helsinki2_3lata <- selectByDate( 
  Helsinki2_pm25,
  start = "2019-01-01",
  end = "2021-12-31",
)

#obliczanie sredniego zanieczyszczenia dla Helsinki2_2019,2020,2021
sredniaHelsinki2_2019<-as.data.frame(apply(Helsinki2_2019[1:364,2], 2, mean, na.rm=TRUE))
sredniaHelsinki2_2020<-as.data.frame(apply(Helsinki2_2020[1:363,2], 2, mean, na.rm=TRUE))
sredniaHelsinki2_2021<-as.data.frame(apply(Helsinki2_2021[1:362,2], 2, mean, na.rm=TRUE))

srednia3lata2_Helsinki <- as.data.frame((sredniaHelsinki2_2019+sredniaHelsinki2_2020+sredniaHelsinki2_2021)/3)
#wykres sezonowości dla pkt 2 Helsinki
Helsinki2_3lata_ts <- ts(Helsinki2_3lata["pm25"], start = c(2019, 1), end = c(2021, 12), frequency = 12)


#zmiana wartości NA z ts
Helsinki2_3lata_ts <- na_kalman(Helsinki2_3lata_ts)

season_Helsinki2 <- ggseasonplot(Helsinki2_3lata_ts, 
                                 year.labels=FALSE, 
                                 season.labels = c("st", "lut", "mrz", "kw", "maj", "cz", "lip", "sier", "wrz", "paź", "lis", "gr"))+
  geom_line(size = 0.3) +
  geom_point(size = 1.7, alpha=0.5) +
  #ggtitle("Kallio 2, Helsinki") + 
  xlab("miesiąc") +
  scale_color_brewer(name= "Rok", palette = "Set1")

Helsinki2_gg <- ggplotly(season_Helsinki2) 


#obliczanie średniej miesięcznej do wyswietlania w boxach wg miesiąca i roku 
Helsinki2_2019lata_ts <- ts(Helsinki2_2019["pm25"], start = c(2019, 1), end = c(2019, 12), frequency = 12)
Helsinki2_2019mean <- tapply(Helsinki2_2019lata_ts,cycle(Helsinki2_2019lata_ts),mean,na.rm=TRUE)

Helsinki2_2020lata_ts <- ts(Helsinki2_2020["pm25"], start = c(2020, 1), end = c(2020, 12), frequency = 12)
Helsinki2_2020mean <- tapply(Helsinki2_2020lata_ts,cycle(Helsinki2_2020lata_ts),mean,na.rm=TRUE)

Helsinki2_2021lata_ts <- ts(Helsinki2_2021["pm25"], start = c(2021, 1), end = c(2021, 12), frequency = 12)
Helsinki2_2021mean <- tapply(Helsinki2_2021lata_ts,cycle(Helsinki2_2021lata_ts),mean,na.rm=TRUE)





#Helsinki PUNKT POMIAROWY NR 3

#dane z czujnika yanqing-town, beijing-air-quality
Helsinki3 <- read_excel("C:/Users/Gaba/OneDrive/Dokumenty/praca inżynierska/inżynierka/praca_inzynierska/mäkelänkatu,-helsinki, finland-air-quality.xlsx")

#przycinanie danych do zanieczyszczen pylami pm2.5
Helsinki3_pm25 <- Helsinki3 %>%
  select(date, pm25)

#2019:
Helsinki3_2019 <- selectByDate( 
  Helsinki3_pm25,
  start = "2019-01-01",
  end = "2019-12-31",
)

#2020:
Helsinki3_2020 <- selectByDate( 
  Helsinki3_pm25,
  start = "2020-01-01",
  end = "2020-12-31",
)

#2021:
Helsinki3_2021 <- selectByDate( 
  Helsinki3_pm25,
  start = "2021-01-01",
  end = "2021-12-31",
)

#2019-2021
Helsinki3_3lata <- selectByDate( 
  Helsinki3_pm25,
  start = "2019-01-01",
  end = "2021-12-31",
)


#obliczanie sredniego zanieczyszczenia dla Helsinki2_2019,2020,2021
sredniaHelsinki3_2019<-as.data.frame(apply(Helsinki3_2019[1:364,2], 2, mean, na.rm=TRUE))
sredniaHelsinki3_2020<-as.data.frame(apply(Helsinki3_2020[1:363,2], 2, mean, na.rm=TRUE))
sredniaHelsinki3_2021<-as.data.frame(apply(Helsinki3_2021[1:362,2], 2, mean, na.rm=TRUE))

srednia3lata3_Helsinki <-as.data.frame((sredniaHelsinki3_2019+sredniaHelsinki3_2020+sredniaHelsinki3_2021)/3)
#wykres sezonowości dla pkt 3 Helsinki
Helsinki3_3lata_ts <- ts(Helsinki3_3lata["pm25"], start = c(2019, 1), end = c(2021, 12), frequency = 12)

season_Helsinki3 <- ggseasonplot(Helsinki3_3lata_ts, 
                                 year.labels=FALSE, 
                                 season.labels = c("st", "lut", "mrz", "kw", "maj", "cz", "lip", "sier", "wrz", "paź", "lis", "gr"))+
  geom_line(size = 0.3) +
  geom_point(size = 1.7, alpha=0.5) +
  ggtitle("Wykresy sezonowości, Helsinki") + 
  xlab("miesiąc") +
  scale_color_brewer(name= "Rok", palette = "Set1")

#Helsinki3_gg <- ggplotly(season_Helsinki3) 




#obliczanie średniej miesięcznej do wyswietlania w boxach wg miesiąca i roku 
Helsinki3_2019lata_ts <- ts(Helsinki3_2019["pm25"], start = c(2019, 1), end = c(2019, 12), frequency = 12)
Helsinki3_2019mean <- tapply(Helsinki3_2019lata_ts,cycle(Helsinki3_2019lata_ts),mean,na.rm=TRUE)

Helsinki3_2020lata_ts <- ts(Helsinki3_2020["pm25"], start = c(2020, 1), end = c(2020, 12), frequency = 12)
Helsinki3_2020mean <- tapply(Helsinki3_2020lata_ts,cycle(Helsinki3_2020lata_ts),mean,na.rm=TRUE)

Helsinki3_2021lata_ts <- ts(Helsinki3_2021["pm25"], start = c(2021, 1), end = c(2021, 12), frequency = 12)
Helsinki3_2021mean <- tapply(Helsinki3_2021lata_ts,cycle(Helsinki3_2021lata_ts),mean,na.rm=TRUE)



#srednia dla 3 czujnikow z wyszczegolnieniem roku i miesiaca
#trzeba usunąc NA z 2021, czujnik nr 3

Helsinki_2019_SR <- as.data.frame((Helsinki1_2019mean+Helsinki2_2019mean+Helsinki1_2019mean)/3)
Helsinki_2020_SR <- as.data.frame((Helsinki1_2020mean+Helsinki2_2020mean+Helsinki1_2020mean)/3)
Helsinki_2021_SR <- as.data.frame((Helsinki1_2021mean+Helsinki2_2021mean+Helsinki1_2021mean)/3)

colnames(Helsinki_2019_SR) <- c('2019')
colnames(Helsinki_2020_SR) <- c('2020')
colnames(Helsinki_2021_SR) <- c('2021')

HELSINKI_SR <- data.frame(Helsinki_2019_SR,Helsinki_2020_SR,Helsinki_2021_SR)

#nadawanie nazw kolumn co przyda sie przy polaczeniu ich w jedna data frame ORAZ PRZY interaktywnosci
# do wyboru miesiaca i roku 
rownames(HELSINKI_SR) <- c("styczeń","luty","marzec","kwiecień","maj","czerwiec","lipiec","sierpień","wrzesień","październik","listopad","grudzień")
colnames(HELSINKI_SR) <- c("2019","2020","2021")






#FUNKCJE






#MAPA ŚWIATA ZMIENNE

#do popup użyte elementy html do ladniejszej wizualizacji 
lat_ <- c(28.696022012351253,28.67090991328277,28.701421327989603,39.70910823530599,40.140470935483826,40.459452228921315,60.16996629091204,60.18002146137783,60.2018011333608)
lng_ <- c(77.18107596375631,77.12846544257657,77.16498887962415,116.79610210988997,117.10023830315099,115.98451704935258,24.93836911403921,24.949833908093428,24.945685436247874)
popup_ <- c(paste("<h4><b> Satyawati College, Delhi </b></h4>
                  <b>Min. PM2.5:</b>", min(Delhi3_3lata$pm25,na.rm=TRUE),"<br/> 
                  <b>Maks. PM2.5:</b>",max(Delhi3_3lata$pm25,na.rm=TRUE),"<br/> 
                  <b>Śr. zanieczyszczenie PM2.5:</b>", formatC(round(as.numeric(srednia3lata3_Delhi),2))),
            
            
            paste("<h4><b> Punjabi Bagh, Delhi </b></h4>
                  <b>Min. PM2.5:</b>", min(Delhi2_3lata$pm25,na.rm=TRUE),"<br/> 
                  <b>Maks. PM2.5:</b>",max(Delhi2_3lata$pm25,na.rm=TRUE),"<br/> 
                  <b>Śr. zanieczyszczenie PM2.5:</b>", formatC(round(as.numeric(srednia3lata2_Delhi),2))),
            
            
            paste("<h4><b> Delhi Institute Of Tool Engineering </b></h4>
                  <b>Min. PM2.5:</b>", min(Delhi_3lata$pm25,na.rm=TRUE),"<br/> 
                  <b>Maks. PM2.5:</b>",max(Delhi_3lata$pm25,na.rm=TRUE),"<br/> 
                  <b>Śr. zanieczyszczenie PM2.5:</b>", formatC(round(as.numeric(srednia3lata_Delhi),2))),
            
            paste("<h4><b>Yongledianzhen, Tongzhou, Pekin</b></h4>
                  <b>Min. PM2.5:</b>", min(Pekin_3lata$pm25,na.rm=TRUE),"<br/> 
                  <b>Maks. PM2.5:</b>",max(Pekin_3lata$pm25,na.rm=TRUE),"<br/> 
                  <b>Śr. zanieczyszczenie PM2.5:</b>", formatC(round(as.numeric(srednia3lata_Pekin),2))),
            
            paste("<h4><b>Pinggu New Town, Pekin</b></h4>
                  <b>Min. PM2.5:</b>", min(Pekin2_3lata$pm25,na.rm=TRUE),"<br/> 
                  <b>Maks. PM2.5:</b>",max(Pekin2_3lata$pm25,na.rm=TRUE),"<br/> 
                  <b>Śr. zanieczyszczenie PM2.5:</b>", formatC(round(as.numeric(srednia3lata2_Pekin),2))),
            
            paste("<h4><b>Yanqing District, Pekin</b></h4>
                  <b>Min. PM2.5:</b>", min(Pekin3_3lata$pm25,na.rm=TRUE),"<br/> 
                  <b>Maks. PM2.5:</b>",max(Pekin3_3lata$pm25,na.rm=TRUE),"<br/> 
                  <b>Śr. zanieczyszczenie PM2.5:</b>", formatC(round(as.numeric(srednia3lata3_Pekin),2))),
            
            paste("<h4><b>Center, Helsinki</b></h4>
                  <b>Min. PM2.5:</b>", min(Helsinki_3lata$pm25,na.rm=TRUE),"<br/> 
                  <b>Maks. PM2.5:</b>",max(Helsinki_3lata$pm25,na.rm=TRUE),"<br/> 
                  <b>Śr. zanieczyszczenie PM2.5:</b>", formatC(round(as.numeric(srednia3lata_Helsinki),2))),
            
            paste("<h4><b>Kallio 2, Helsinki</b></h4>
                  <b>Min. PM2.5:</b>", min(Helsinki2_3lata$pm25,na.rm=TRUE),"<br/> 
                  <b>Maks. PM2.5:</b>",max(Helsinki2_3lata$pm25,na.rm=TRUE),"<br/> 
                  <b>Śr. zanieczyszczenie PM2.5:</b>", formatC(round(as.numeric(srednia3lata2_Helsinki),2))),
            
            paste("<h4><b>Mäkelänkatu, Helsinki</b></h4>
                  <b>Min. PM2.5:</b>", min(Helsinki3_3lata$pm25,na.rm=TRUE),"<br/> 
                  <b>Maks. PM2.5:</b>",max(Helsinki3_3lata$pm25,na.rm=TRUE),"<br/> 
                  <b>Śr. zanieczyszczenie PM2.5:</b>", formatC(round(as.numeric(srednia3lata3_Helsinki),2))))

lok_ <- c("Delhi","Delhi","Delhi","Pekin","Pekin","Pekin","Helsinki","Helsinki","Helsinki")
pm25_ <-c("high","high","high","medium","medium","medium","low","low","low")
col_ <- c("red","red","red","orange","orange","orange","green","green","green")
lokalizacje_swiat<-as.data.frame(list(lat = lat_, lng = lng_, popup = popup_, lok = lok_, pm25 = pm25_, coll = col_))
lokalizacje_swiat











####################WYKRES SŁUPKOWY################################


#sumowanie wartosci w zaleznosci od stanu powietrza Delhi:

#2019:

d2019_3_bardzo_dobra <- sum(Delhi3_2019$pm25 < 50,na.rm=TRUE) 
d2019_3_dobra <- sum(Delhi3_2019$pm25 > 51 & Delhi3_2019$pm25 < 100, na.rm=TRUE)
d2019_3_umiarkowana <- sum(Delhi3_2019$pm25 > 101 & Delhi3_2019$pm25 < 150, na.rm=TRUE) 
d2019_3_dostateczna <- sum(Delhi3_2019$pm25 > 151 & Delhi3_2019$pm25 < 200, na.rm=TRUE)
d2019_3_zla <- sum(Delhi3_2019$pm25 > 201 & Delhi3_2019$pm25 < 300, na.rm=TRUE) 
d2019_3_bardzo_zla <- sum(Delhi3_2019$pm25 > 300, na.rm=TRUE)

d2019_2_bardzo_dobra <- sum(Delhi2_2019$pm25 < 50,na.rm=TRUE) 
d2019_2_dobra <- sum(Delhi2_2019$pm25 > 51 & Delhi2_2019$pm25 < 100, na.rm=TRUE)
d2019_2_umiarkowana <- sum(Delhi2_2019$pm25 > 101 & Delhi2_2019$pm25 < 150, na.rm=TRUE) 
d2019_2_dostateczna <- sum(Delhi2_2019$pm25 > 151 & Delhi2_2019$pm25 < 200, na.rm=TRUE) 
d2019_2_zla <- sum(Delhi2_2019$pm25 > 201 & Delhi2_2019$pm25 < 300, na.rm=TRUE) 
d2019_2_bardzo_zla <- sum(Delhi2_2019$pm25 > 300, na.rm=TRUE)

d2019_bardzo_dobra <- sum(Delhi_2019$pm25 < 50,na.rm=TRUE) 
d2019_dobra <- sum(Delhi_2019$pm25 > 51 & Delhi_2019$pm25 < 100, na.rm=TRUE)
d2019_umiarkowana <- sum(Delhi_2019$pm25 > 101 & Delhi_2019$pm25 < 150, na.rm=TRUE) 
d2019_dostateczna <- sum(Delhi_2019$pm25 > 151 & Delhi_2019$pm25 < 200, na.rm=TRUE) 
d2019_zla <- sum(Delhi_2019$pm25 > 201 & Delhi_2019$pm25 < 300, na.rm=TRUE) 
d2019_bardzo_zla <- sum(Delhi_2019$pm25 > 300, na.rm=TRUE)

Delhi_bardzo_dobra2019<-mean(sum(d2019_3_bardzo_dobra,d2019_2_bardzo_dobra,d2019_bardzo_dobra))
Delhi_dobra2019<-mean(sum(d2019_3_dobra,d2019_2_dobra,d2019_dobra))
Delhi_umiarkowana2019<-mean(sum(d2019_3_umiarkowana,d2019_2_umiarkowana,d2019_umiarkowana))
Delhi_dostateczna2019<-mean(sum(d2019_3_dostateczna,d2019_2_dostateczna,d2019_dostateczna))
Delhi_zla2019<-mean(sum(d2019_3_zla,d2019_2_zla,d2019_zla))
Delhi_bardzo_zla2019 <-mean(sum(d2019_3_bardzo_zla,d2019_2_bardzo_zla,d2019_bardzo_zla))

#2020:

d2020_3_bardzo_dobra <- sum(Delhi3_2020$pm25 < 50,na.rm=TRUE) 
d2020_3_dobra <- sum(Delhi3_2020$pm25 > 51 & Delhi3_2020$pm25 < 100, na.rm=TRUE)
d2020_3_umiarkowana <- sum(Delhi3_2020$pm25 > 101 & Delhi3_2020$pm25 < 150, na.rm=TRUE) 
d2020_3_dostateczna <- sum(Delhi3_2020$pm25 > 151 & Delhi3_2020$pm25 < 200, na.rm=TRUE)
d2020_3_zla <- sum(Delhi3_2020$pm25 > 201 & Delhi3_2020$pm25 < 300, na.rm=TRUE) 
d2020_3_bardzo_zla <- sum(Delhi3_2020$pm25 > 300, na.rm=TRUE)

d2020_2_bardzo_dobra <- sum(Delhi2_2020$pm25 < 50,na.rm=TRUE) 
d2020_2_dobra <- sum(Delhi2_2020$pm25 > 51 & Delhi2_2020$pm25 < 100, na.rm=TRUE)
d2020_2_umiarkowana <- sum(Delhi2_2020$pm25 > 101 & Delhi2_2020$pm25 < 150, na.rm=TRUE) 
d2020_2_dostateczna <- sum(Delhi2_2020$pm25 > 151 & Delhi2_2020$pm25 < 200, na.rm=TRUE) 
d2020_2_zla <- sum(Delhi2_2020$pm25 > 201 & Delhi2_2020$pm25 < 300, na.rm=TRUE) 
d2020_2_bardzo_zla <- sum(Delhi2_2020$pm25 > 300, na.rm=TRUE)

d2020_bardzo_dobra <- sum(Delhi_2020$pm25 < 50,na.rm=TRUE) 
d2020_dobra <- sum(Delhi_2020$pm25 > 51 & Delhi_2020$pm25 < 100, na.rm=TRUE)
d2020_umiarkowana <- sum(Delhi_2020$pm25 > 101 & Delhi_2020$pm25 < 150, na.rm=TRUE) 
d2020_dostateczna <- sum(Delhi_2020$pm25 > 151 & Delhi_2020$pm25 < 200, na.rm=TRUE) 
d2020_zla <- sum(Delhi_2020$pm25 > 201 & Delhi_2020$pm25 < 300, na.rm=TRUE) 
d2020_bardzo_zla <- sum(Delhi_2020$pm25 > 300, na.rm=TRUE)

Delhi_bardzo_dobra2020<-mean(sum(d2020_3_bardzo_dobra,d2020_2_bardzo_dobra,d2020_bardzo_dobra))
Delhi_dobra2020<-mean(sum(d2020_3_dobra,d2020_2_dobra,d2020_dobra))
Delhi_umiarkowana2020<-mean(sum(d2020_3_umiarkowana,d2020_2_umiarkowana,d2020_umiarkowana))
Delhi_dostateczna2020<-mean(sum(d2020_3_dostateczna,d2020_2_dostateczna,d2020_dostateczna))
Delhi_zla2020<-mean(sum(d2020_3_zla,d2020_2_zla,d2020_zla))
Delhi_bardzo_zla2020 <-mean(sum(d2020_3_bardzo_zla,d2020_2_bardzo_zla,d2020_bardzo_zla))

#2021:

#2020:
d2021_3_bardzo_dobra <- sum(Delhi3_2021$pm25 < 50,na.rm=TRUE) 
d2021_3_dobra <- sum(Delhi3_2021$pm25 > 51 & Delhi3_2021$pm25 < 100, na.rm=TRUE)
d2021_3_umiarkowana <- sum(Delhi3_2021$pm25 > 101 & Delhi3_2021$pm25 < 150, na.rm=TRUE) 
d2021_3_dostateczna <- sum(Delhi3_2021$pm25 > 151 & Delhi3_2021$pm25 < 200, na.rm=TRUE)
d2021_3_zla <- sum(Delhi3_2021$pm25 > 201 & Delhi3_2021$pm25 < 300, na.rm=TRUE) 
d2021_3_bardzo_zla <- sum(Delhi3_2021$pm25 > 300, na.rm=TRUE)

d2021_2_bardzo_dobra <- sum(Delhi2_2021$pm25 < 50,na.rm=TRUE) 
d2021_2_dobra <- sum(Delhi2_2021$pm25 > 51 & Delhi2_2021$pm25 < 100, na.rm=TRUE)
d2021_2_umiarkowana <- sum(Delhi2_2021$pm25 > 101 & Delhi2_2021$pm25 < 150, na.rm=TRUE) 
d2021_2_dostateczna <- sum(Delhi2_2021$pm25 > 151 & Delhi2_2021$pm25 < 200, na.rm=TRUE) 
d2021_2_zla <- sum(Delhi2_2021$pm25 > 201 & Delhi2_2021$pm25 < 300, na.rm=TRUE) 
d2021_2_bardzo_zla <- sum(Delhi2_2021$pm25 > 300, na.rm=TRUE)

d2021_bardzo_dobra <- sum(Delhi_2021$pm25 < 50,na.rm=TRUE) 
d2021_dobra <- sum(Delhi_2021$pm25 > 51 & Delhi_2021$pm25 < 100, na.rm=TRUE)
d2021_umiarkowana <- sum(Delhi_2021$pm25 > 101 & Delhi_2021$pm25 < 150, na.rm=TRUE) 
d2021_dostateczna <- sum(Delhi_2021$pm25 > 151 & Delhi_2021$pm25 < 200, na.rm=TRUE) 
d2021_zla <- sum(Delhi_2021$pm25 > 201 & Delhi_2021$pm25 < 300, na.rm=TRUE) 
d2021_bardzo_zla <- sum(Delhi_2021$pm25 > 300, na.rm=TRUE)

Delhi_bardzo_dobra2021<-mean(sum(d2021_3_bardzo_dobra,d2021_2_bardzo_dobra,d2021_bardzo_dobra))
Delhi_dobra2021<-mean(sum(d2021_3_dobra,d2021_2_dobra,d2021_dobra))
Delhi_umiarkowana2021<-mean(sum(d2021_3_umiarkowana,d2021_2_umiarkowana,d2021_umiarkowana))
Delhi_dostateczna2021<-mean(sum(d2021_3_dostateczna,d2021_2_dostateczna,d2021_dostateczna))
Delhi_zla2021<-mean(sum(d2021_3_zla,d2021_2_zla,d2021_zla))
Delhi_bardzo_zla2021 <-mean(sum(d2021_3_bardzo_zla,d2021_2_bardzo_zla,d2021_bardzo_zla))



#####################PEKIN##########################
#2019:

p3_2019_bardzo_dobra <- sum(Pekin3_2019$pm25 < 50,na.rm=TRUE) 
p3_2019_dobra <- sum(Pekin3_2019$pm25 > 51 & Pekin3_2019$pm25 < 100, na.rm=TRUE)
p3_2019_umiarkowana <- sum(Pekin3_2019$pm25 > 101 & Pekin3_2019$pm25 < 150, na.rm=TRUE)
p3_2019_dostateczna <- sum(Pekin3_2019$pm25 > 151 & Pekin3_2019$pm25 < 200, na.rm=TRUE) 
p3_2019_zla <- sum(Pekin3_2019$pm25 > 201 & Pekin3_2019$pm25 < 300, na.rm=TRUE) 
p3_2019_bardzo_zla <- sum(Pekin3_2019$pm25 > 300, na.rm=TRUE)

p2_2019_bardzo_dobra <- sum(Pekin2_2019$pm25 < 50,na.rm=TRUE) 
p2_2019_dobra <- sum(Pekin2_2019$pm25 > 51 & Pekin2_2019$pm25 < 100, na.rm=TRUE)
p2_2019_umiarkowana <- sum(Pekin2_2019$pm25 > 101 & Pekin2_2019$pm25 < 150, na.rm=TRUE) 
p2_2019_dostateczna <- sum(Pekin2_2019$pm25 > 151 & Pekin2_2019$pm25 < 200, na.rm=TRUE) 
p2_2019_zla <- sum(Pekin2_2019$pm25 > 201 & Pekin2_2019$pm25 < 300, na.rm=TRUE) 
p2_2019_bardzo_zla <- sum(Pekin2_2019$pm25 > 300, na.rm=TRUE)

p1_2019_bardzo_dobra <- sum(Pekin1_2019$pm25 < 50,na.rm=TRUE) 
p1_2019_dobra <- sum(Pekin1_2019$pm25 > 51 & Pekin1_2019$pm25 < 100, na.rm=TRUE)
p1_2019_umiarkowana <- sum(Pekin1_2019$pm25 > 101 & Pekin1_2019$pm25 < 150, na.rm=TRUE) 
p1_2019_dostateczna <- sum(Pekin1_2019$pm25 > 151 & Pekin1_2019$pm25 < 200, na.rm=TRUE) 
p1_2019_zla <- sum(Pekin1_2019$pm25 > 201 & Pekin1_2019$pm25 < 300, na.rm=TRUE)
p1_2019_bardzo_zla <- sum(Pekin1_2019$pm25 > 300, na.rm=TRUE)


Pekin_bardzo_dobra2019<-mean(sum(p1_2019_bardzo_dobra,p2_2019_bardzo_dobra,p3_2019_bardzo_dobra))
Pekin_dobra2019<-mean(sum(p1_2019_dobra,p2_2019_dobra,p3_2019_dobra))
Pekin_umiarkowana2019<-mean(sum(p1_2019_umiarkowana,p2_2019_umiarkowana,p3_2019_umiarkowana))
Pekin_dostateczna2019<-mean(sum(p1_2019_dostateczna,p2_2019_dostateczna,p3_2019_dostateczna))
Pekin_zla2019<-mean(sum(p1_2019_zla,p2_2019_zla,p3_2019_zla))
Pekin_bardzo_zla2019 <-mean(sum(p1_2019_bardzo_zla,p2_2019_bardzo_zla,p3_2019_bardzo_zla))

#2020:

p3_2020_bardzo_dobra <- sum(Pekin3_2020$pm25 < 50,na.rm=TRUE) 
p3_2020_dobra <- sum(Pekin3_2020$pm25 > 51 & Pekin3_2020$pm25 < 100, na.rm=TRUE)
p3_2020_umiarkowana <- sum(Pekin3_2020$pm25 > 101 & Pekin3_2020$pm25 < 150, na.rm=TRUE)
p3_2020_dostateczna <- sum(Pekin3_2020$pm25 > 151 & Pekin3_2020$pm25 < 200, na.rm=TRUE) 
p3_2020_zla <- sum(Pekin3_2020$pm25 > 201 & Pekin3_2020$pm25 < 300, na.rm=TRUE) 
p3_2020_bardzo_zla <- sum(Pekin3_2020$pm25 > 300, na.rm=TRUE)

p2_2020_bardzo_dobra <- sum(Pekin2_2020$pm25 < 50,na.rm=TRUE) 
p2_2020_dobra <- sum(Pekin2_2020$pm25 > 51 & Pekin2_2020$pm25 < 100, na.rm=TRUE)
p2_2020_umiarkowana <- sum(Pekin2_2020$pm25 > 101 & Pekin2_2020$pm25 < 150, na.rm=TRUE) 
p2_2020_dostateczna <- sum(Pekin2_2020$pm25 > 151 & Pekin2_2020$pm25 < 200, na.rm=TRUE) 
p2_2020_zla <- sum(Pekin2_2020$pm25 > 201 & Pekin2_2020$pm25 < 300, na.rm=TRUE) 
p2_2020_bardzo_zla <- sum(Pekin2_2020$pm25 > 300, na.rm=TRUE)

p1_2020_bardzo_dobra <- sum(Pekin1_2020$pm25 < 50,na.rm=TRUE) 
p1_2020_dobra <- sum(Pekin1_2020$pm25 > 51 & Pekin1_2020$pm25 < 100, na.rm=TRUE)
p1_2020_umiarkowana <- sum(Pekin1_2020$pm25 > 101 & Pekin1_2020$pm25 < 150, na.rm=TRUE) 
p1_2020_dostateczna <- sum(Pekin1_2020$pm25 > 151 & Pekin1_2020$pm25 < 200, na.rm=TRUE) 
p1_2020_zla <- sum(Pekin1_2020$pm25 > 201 & Pekin1_2020$pm25 < 300, na.rm=TRUE)
p1_2020_bardzo_zla <- sum(Pekin1_2020$pm25 > 300, na.rm=TRUE)


Pekin_bardzo_dobra2020<-mean(sum(p1_2020_bardzo_dobra,p2_2020_bardzo_dobra,p3_2020_bardzo_dobra))
Pekin_dobra2020<-mean(sum(p1_2020_dobra,p2_2020_dobra,p3_2020_dobra))
Pekin_umiarkowana2020<-mean(sum(p1_2020_umiarkowana,p2_2020_umiarkowana,p3_2020_umiarkowana))
Pekin_dostateczna2020<-mean(sum(p1_2020_dostateczna,p2_2020_dostateczna,p3_2020_dostateczna))
Pekin_zla2020<-mean(sum(p1_2020_zla,p2_2020_zla,p3_2020_zla))
Pekin_bardzo_zla2020 <-mean(sum(p1_2020_bardzo_zla,p2_2020_bardzo_zla,p3_2020_bardzo_zla))

#2021:

p3_2021_bardzo_dobra <- sum(Pekin3_2021$pm25 < 50,na.rm=TRUE) 
p3_2021_dobra <- sum(Pekin3_2021$pm25 > 51 & Pekin3_2021$pm25 < 100, na.rm=TRUE)
p3_2021_umiarkowana <- sum(Pekin3_2021$pm25 > 101 & Pekin3_2021$pm25 < 150, na.rm=TRUE)
p3_2021_dostateczna <- sum(Pekin3_2021$pm25 > 151 & Pekin3_2021$pm25 < 200, na.rm=TRUE) 
p3_2021_zla <- sum(Pekin3_2021$pm25 > 201 & Pekin3_2021$pm25 < 300, na.rm=TRUE) 
p3_2021_bardzo_zla <- sum(Pekin3_2021$pm25 > 300, na.rm=TRUE)

p2_2021_bardzo_dobra <- sum(Pekin2_2021$pm25 < 50,na.rm=TRUE) 
p2_2021_dobra <- sum(Pekin2_2021$pm25 > 51 & Pekin2_2021$pm25 < 100, na.rm=TRUE)
p2_2021_umiarkowana <- sum(Pekin2_2021$pm25 > 101 & Pekin2_2021$pm25 < 150, na.rm=TRUE) 
p2_2021_dostateczna <- sum(Pekin2_2021$pm25 > 151 & Pekin2_2021$pm25 < 200, na.rm=TRUE) 
p2_2021_zla <- sum(Pekin2_2021$pm25 > 201 & Pekin2_2021$pm25 < 300, na.rm=TRUE) 
p2_2021_bardzo_zla <- sum(Pekin2_2021$pm25 > 300, na.rm=TRUE)

p1_2021_bardzo_dobra <- sum(Pekin1_2021$pm25 < 50,na.rm=TRUE) 
p1_2021_dobra <- sum(Pekin1_2021$pm25 > 51 & Pekin1_2021$pm25 < 100, na.rm=TRUE)
p1_2021_umiarkowana <- sum(Pekin1_2021$pm25 > 101 & Pekin1_2021$pm25 < 150, na.rm=TRUE) 
p1_2021_dostateczna <- sum(Pekin1_2021$pm25 > 151 & Pekin1_2021$pm25 < 200, na.rm=TRUE) 
p1_2021_zla <- sum(Pekin1_2021$pm25 > 201 & Pekin1_2021$pm25 < 300, na.rm=TRUE)
p1_2021_bardzo_zla <- sum(Pekin1_2021$pm25 > 300, na.rm=TRUE)


Pekin_bardzo_dobra2021<-mean(sum(p1_2021_bardzo_dobra,p2_2021_bardzo_dobra,p3_2021_bardzo_dobra))
Pekin_dobra2021<-mean(sum(p1_2021_dobra,p2_2021_dobra,p3_2021_dobra))
Pekin_umiarkowana2021<-mean(sum(p1_2021_umiarkowana,p2_2021_umiarkowana,p3_2021_umiarkowana))
Pekin_dostateczna2021<-mean(sum(p1_2021_dostateczna,p2_2021_dostateczna,p3_2021_dostateczna))
Pekin_zla2021<-mean(sum(p1_2021_zla,p2_2021_zla,p3_2021_zla))
Pekin_bardzo_zla2021 <-mean(sum(p1_2021_bardzo_zla,p2_2021_bardzo_zla,p3_2021_bardzo_zla))

#####################HELSINKI##########################
#2019:
h3_2019_bardzo_dobra <- sum(Helsinki3_2019$pm25 < 50,na.rm=TRUE) 
h3_2019_dobra <- sum(Helsinki3_2019$pm25 > 51 & Helsinki3_2019$pm25 < 100, na.rm=TRUE)
h3_2019_umiarkowana <- sum(Helsinki3_2019$pm25 > 101 & Helsinki3_2019$pm25 < 150, na.rm=TRUE)
h3_2019_dostateczna <- sum(Helsinki3_2019$pm25 > 151 & Helsinki3_2019$pm25 < 200, na.rm=TRUE) 
h3_2019_zla <- sum(Helsinki3_2019$pm25 > 201 & Helsinki3_2019$pm25 < 300, na.rm=TRUE) 
h3_2019_bardzo_zla <- sum(Helsinki3_2019$pm25 > 300, na.rm=TRUE)

h2_2019_bardzo_dobra <- sum(Helsinki2_2019$pm25 < 50,na.rm=TRUE) 
h2_2019_dobra <- sum(Helsinki2_2019$pm25 > 51 & Helsinki2_2019$pm25 < 100, na.rm=TRUE)
h2_2019_umiarkowana <- sum(Helsinki2_2019$pm25 > 101 & Helsinki2_2019$pm25 < 150, na.rm=TRUE) 
h2_2019_dostateczna <- sum(Helsinki2_2019$pm25 > 151 & Helsinki2_2019$pm25 < 200, na.rm=TRUE) 
h2_2019_zla <- sum(Helsinki2_2019$pm25 > 201 & Helsinki2_2019$pm25 < 300, na.rm=TRUE) 
h2_2019_bardzo_zla <- sum(Helsinki2_2019$pm25 > 300, na.rm=TRUE)

h1_2019_bardzo_dobra <- sum(Helsinki_2019$pm25 < 50,na.rm=TRUE) 
h1_2019_dobra <- sum(Helsinki_2019$pm25 > 51 & Helsinki_2019$pm25 < 100, na.rm=TRUE)
h1_2019_umiarkowana <- sum(Helsinki_2019$pm25 > 101 & Helsinki_2019$pm25 < 150, na.rm=TRUE) 
h1_2019_dostateczna <- sum(Helsinki_2019$pm25 > 151 & Helsinki_2019$pm25 < 200, na.rm=TRUE) 
h1_2019_zla <- sum(Helsinki_2019$pm25 > 201 & Helsinki_2019$pm25 < 300, na.rm=TRUE)
h1_2019_bardzo_zla <- sum(Helsinki_2019$pm25 > 300, na.rm=TRUE)


Helsinki_bardzo_dobra2019<-mean(sum(h1_2019_bardzo_dobra,h2_2019_bardzo_dobra,h3_2019_bardzo_dobra))
Helsinki_dobra2019<-mean(sum(h1_2019_dobra,h2_2019_dobra,h3_2019_dobra))
Helsinki_umiarkowana2019<-mean(sum(h1_2019_umiarkowana,h2_2019_umiarkowana,h3_2019_umiarkowana))
Helsinki_dostateczna2019<-mean(sum(h1_2019_dostateczna,h2_2019_dostateczna,h3_2019_dostateczna))
Helsinki_zla2019<-mean(sum(h1_2019_zla,h2_2019_zla,h3_2019_zla))
Helsinki_bardzo_zla2019<-mean(sum(h1_2019_bardzo_zla,h2_2019_bardzo_zla,h3_2019_bardzo_zla))

#2020:

h3_2020_bardzo_dobra <- sum(Helsinki3_2020$pm25 < 50,na.rm=TRUE) 
h3_2020_dobra <- sum(Helsinki3_2020$pm25 > 51 & Helsinki3_2020$pm25 < 100, na.rm=TRUE)
h3_2020_umiarkowana <- sum(Helsinki3_2020$pm25 > 101 & Helsinki3_2020$pm25 < 150, na.rm=TRUE)
h3_2020_dostateczna <- sum(Helsinki3_2020$pm25 > 151 & Helsinki3_2020$pm25 < 200, na.rm=TRUE) 
h3_2020_zla <- sum(Helsinki3_2020$pm25 > 201 & Helsinki3_2020$pm25 < 300, na.rm=TRUE) 
h3_2020_bardzo_zla <- sum(Helsinki3_2020$pm25 > 300, na.rm=TRUE)

h2_2020_bardzo_dobra <- sum(Helsinki2_2020$pm25 < 50,na.rm=TRUE) 
h2_2020_dobra <- sum(Helsinki2_2020$pm25 > 51 & Helsinki2_2020$pm25 < 100, na.rm=TRUE)
h2_2020_umiarkowana <- sum(Helsinki2_2020$pm25 > 101 & Helsinki2_2020$pm25 < 150, na.rm=TRUE) 
h2_2020_dostateczna <- sum(Helsinki2_2020$pm25 > 151 & Helsinki2_2020$pm25 < 200, na.rm=TRUE) 
h2_2020_zla <- sum(Helsinki2_2020$pm25 > 201 & Helsinki2_2020$pm25 < 300, na.rm=TRUE) 
h2_2020_bardzo_zla <- sum(Helsinki2_2020$pm25 > 300, na.rm=TRUE)

h1_2020_bardzo_dobra <- sum(Helsinki_2020$pm25 < 50,na.rm=TRUE) 
h1_2020_dobra <- sum(Helsinki_2020$pm25 > 51 & Helsinki_2020$pm25 < 100, na.rm=TRUE)
h1_2020_umiarkowana <- sum(Helsinki_2020$pm25 > 101 & Helsinki_2020$pm25 < 150, na.rm=TRUE) 
h1_2020_dostateczna <- sum(Helsinki_2020$pm25 > 151 & Helsinki_2020$pm25 < 200, na.rm=TRUE) 
h1_2020_zla <- sum(Helsinki_2020$pm25 > 201 & Helsinki_2020$pm25 < 300, na.rm=TRUE)
h1_2020_bardzo_zla <- sum(Helsinki_2020$pm25 > 300, na.rm=TRUE)


Helsinki_bardzo_dobra2020<-mean(sum(h1_2020_bardzo_dobra,h2_2020_bardzo_dobra,h3_2020_bardzo_dobra))
Helsinki_dobra2020<-mean(sum(h1_2020_dobra,h2_2020_dobra,h3_2020_dobra))
Helsinki_umiarkowana2020<-mean(sum(h1_2020_umiarkowana,h2_2020_umiarkowana,h3_2020_umiarkowana))
Helsinki_dostateczna2020<-mean(sum(h1_2020_dostateczna,h2_2020_dostateczna,h3_2020_dostateczna))
Helsinki_zla2020<-mean(sum(h1_2020_zla,h2_2020_zla,h3_2020_zla))
Helsinki_bardzo_zla2020<-mean(sum(h1_2020_bardzo_zla,h2_2020_bardzo_zla,h3_2020_bardzo_zla))

#2021

h3_2021_bardzo_dobra <- sum(Helsinki3_2021$pm25 < 50,na.rm=TRUE) 
h3_2021_dobra <- sum(Helsinki3_2021$pm25 > 51 & Helsinki3_2021$pm25 < 100, na.rm=TRUE)
h3_2021_umiarkowana <- sum(Helsinki3_2021$pm25 > 101 & Helsinki3_2021$pm25 < 150, na.rm=TRUE)
h3_2021_dostateczna <- sum(Helsinki3_2021$pm25 > 151 & Helsinki3_2021$pm25 < 200, na.rm=TRUE) 
h3_2021_zla <- sum(Helsinki3_2021$pm25 > 201 & Helsinki3_2021$pm25 < 300, na.rm=TRUE) 
h3_2021_bardzo_zla <- sum(Helsinki3_2021$pm25 > 300, na.rm=TRUE)

h2_2021_bardzo_dobra <- sum(Helsinki2_2021$pm25 < 50,na.rm=TRUE) 
h2_2021_dobra <- sum(Helsinki2_2021$pm25 > 51 & Helsinki2_2021$pm25 < 100, na.rm=TRUE)
h2_2021_umiarkowana <- sum(Helsinki2_2021$pm25 > 101 & Helsinki2_2021$pm25 < 150, na.rm=TRUE) 
h2_2021_dostateczna <- sum(Helsinki2_2021$pm25 > 151 & Helsinki2_2021$pm25 < 200, na.rm=TRUE) 
h2_2021_zla <- sum(Helsinki2_2021$pm25 > 201 & Helsinki2_2021$pm25 < 300, na.rm=TRUE) 
h2_2021_bardzo_zla <- sum(Helsinki2_2021$pm25 > 300, na.rm=TRUE)

h1_2021_bardzo_dobra <- sum(Helsinki_2021$pm25 < 50,na.rm=TRUE) 
h1_2021_dobra <- sum(Helsinki_2021$pm25 > 51 & Helsinki_2021$pm25 < 100, na.rm=TRUE)
h1_2021_umiarkowana <- sum(Helsinki_2021$pm25 > 101 & Helsinki_2021$pm25 < 150, na.rm=TRUE) 
h1_2021_dostateczna <- sum(Helsinki_2021$pm25 > 151 & Helsinki_2021$pm25 < 200, na.rm=TRUE) 
h1_2021_zla <- sum(Helsinki_2021$pm25 > 201 & Helsinki_2021$pm25 < 300, na.rm=TRUE)
h1_2021_bardzo_zla <- sum(Helsinki_2021$pm25 > 300, na.rm=TRUE)


Helsinki_bardzo_dobra2021<-mean(sum(h1_2021_bardzo_dobra,h2_2021_bardzo_dobra,h3_2021_bardzo_dobra))
Helsinki_dobra2021<-mean(sum(h1_2021_dobra,h2_2021_dobra,h3_2021_dobra))
Helsinki_umiarkowana2021<-mean(sum(h1_2021_umiarkowana,h2_2021_umiarkowana,h3_2021_umiarkowana))
Helsinki_dostateczna2021<-mean(sum(h1_2021_dostateczna,h2_2021_dostateczna,h3_2021_dostateczna))
Helsinki_zla2021<-mean(sum(h1_2021_zla,h2_2021_zla,h3_2021_zla))
Helsinki_bardzo_zla2021<-mean(sum(h1_2021_bardzo_zla,h2_2021_bardzo_zla,h3_2021_bardzo_zla))

#WYKRES 2019

Helsinki_stan2019 <- c(Helsinki_bardzo_dobra2019,Helsinki_dobra2019,Helsinki_umiarkowana2019,Helsinki_dostateczna2019,Helsinki_zla2019,Helsinki_bardzo_zla2019)

Pekin_stan2019 <- c(Pekin_bardzo_dobra2019,Pekin_dobra2019,Pekin_umiarkowana2019,Pekin_dostateczna2019,Pekin_zla2019,Pekin_bardzo_zla2019)

Delhi_stan2019 <- c(Delhi_bardzo_dobra2019,Delhi_dobra2019,Delhi_umiarkowana2019,Delhi_dostateczna2019,Delhi_zla2019,Delhi_bardzo_zla2019)

df_stan2019 <- data.frame(Delhi_stan2019, Pekin_stan2019, Helsinki_stan2019)
df_stan2019$stan <- c("Bardzo dobry","Dobry","Umiarkowany","Dostateczny","Zły","Bardzo zły")
#df_stan$Pekin <- data.frame(Pekin_stan)
colnames(df_stan2019) <- c("Delhi","Pekin", "Helsinki", "stan")
#rownames(df_stan) <- c("Bardzo dobry","Dobry","Umiarkowany","Dostateczny","Zły","Bardzo zły")


#fct_inorder w kolejności
col_plot2019 <- df_stan2019 %>%
  pivot_longer(cols = c("Delhi","Pekin","Helsinki")) 


gg_stan2019 <- ggplot(col_plot2019,aes(fct_inorder(stan), value, 
                               text = paste("Miasto: ", name,
                                            "\nPoziom zanieczyszczenia: ",stan, 
                                            "\nLiczba dni: ",value))) + 
  geom_col(aes(fill = name),position = "dodge") +
  ggtitle("Zanieczyszczenie powietrza według norm, 2019") + 
  xlab("") +
  ylab("Liczba dni") +
  coord_flip()+
  scale_fill_discrete(name = "")

gg_stan2019_plotly <- ggplotly(gg_stan2019, tooltip = "text") 
  

#WYKRES 2020

Helsinki_stan2020 <- c(Helsinki_bardzo_dobra2020,Helsinki_dobra2020,Helsinki_umiarkowana2020,Helsinki_dostateczna2020,Helsinki_zla2020,Helsinki_bardzo_zla2020)

Pekin_stan2020 <- c(Pekin_bardzo_dobra2020,Pekin_dobra2020,Pekin_umiarkowana2020,Pekin_dostateczna2020,Pekin_zla2020,Pekin_bardzo_zla2020)

Delhi_stan2020 <- c(Delhi_bardzo_dobra2020,Delhi_dobra2020,Delhi_umiarkowana2020,Delhi_dostateczna2020,Delhi_zla2020,Delhi_bardzo_zla2020)

df_stan2020 <- data.frame(Delhi_stan2020, Pekin_stan2020, Helsinki_stan2020)
df_stan2020$stan <- c("Bardzo dobry","Dobry","Umiarkowany","Dostateczny","Zły","Bardzo zły")
#df_stan$Pekin <- data.frame(Pekin_stan)
colnames(df_stan2020) <- c("Delhi","Pekin", "Helsinki", "stan")
#rownames(df_stan) <- c("Bardzo dobry","Dobry","Umiarkowany","Dostateczny","Zły","Bardzo zły")


#fct_inorder w kolejności
col_plot2020 <- df_stan2020 %>%
  pivot_longer(cols = c("Delhi","Pekin","Helsinki")) 


gg_stan2020 <- ggplot(col_plot2020,aes(fct_inorder(stan), value, 
                                       text = paste("Miasto: ", name,
                                                    "\nPoziom zanieczyszczenia: ",stan, 
                                                    "\nLiczba dni: ",value))) + 
  geom_col(aes(fill = name),position = "dodge") +
  ggtitle("Zanieczyszczenie powietrza według norm, 2020") + 
  xlab("") +
  ylab("Liczba dni") +
  coord_flip()+
  scale_fill_discrete(name = "")

gg_stan2020_plotly <- ggplotly(gg_stan2020,originalData = FALSE, tooltip = "text") 

#WYKRES 2021

Helsinki_stan2021 <- c(Helsinki_bardzo_dobra2021,Helsinki_dobra2021,Helsinki_umiarkowana2021,Helsinki_dostateczna2021,Helsinki_zla2021,Helsinki_bardzo_zla2021)

Pekin_stan2021 <- c(Pekin_bardzo_dobra2021,Pekin_dobra2021,Pekin_umiarkowana2021,Pekin_dostateczna2021,Pekin_zla2021,Pekin_bardzo_zla2021)

Delhi_stan2021 <- c(Delhi_bardzo_dobra2021,Delhi_dobra2021,Delhi_umiarkowana2021,Delhi_dostateczna2021,Delhi_zla2021,Delhi_bardzo_zla2021)

df_stan2021 <- data.frame(Delhi_stan2021, Pekin_stan2021, Helsinki_stan2021)
df_stan2021$stan <- c("Bardzo dobry","Dobry","Umiarkowany","Dostateczny","Zły","Bardzo zły")
#df_stan$Pekin <- data.frame(Pekin_stan)
colnames(df_stan2021) <- c("Delhi","Pekin", "Helsinki", "stan")
#rownames(df_stan) <- c("Bardzo dobry","Dobry","Umiarkowany","Dostateczny","Zły","Bardzo zły")


#fct_inorder w kolejności
col_plot2021 <- df_stan2021 %>%
  pivot_longer(cols = c("Delhi","Pekin","Helsinki")) 


gg_stan2021 <- ggplot(col_plot2021,aes(fct_inorder(stan), value, 
                                       text = paste("Miasto: ", name,
                                                    "\nPoziom zanieczyszczenia: ",stan, 
                                                    "\nLiczba dni: ",value))) + 
  geom_col(aes(fill = name),position = "dodge") +
  ggtitle("Zanieczyszczenie powietrza według norm, 2021") + 
  xlab("") +
  ylab("Liczba dni") +
  coord_flip()+
  scale_fill_discrete(name = "")

gg_stan2021_plotly <- ggplotly(gg_stan2021,originalData = FALSE, tooltip = "text") 






