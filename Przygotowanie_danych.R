library(dplyr)
library(stringr)
library(lubridate)


#pamietaj o dobrze przeksztalconej dacie rrrr-mm-dd hh:mm:ss
Post_B <- read.csv("Pozyskane_dane/Posty_wybranych_spolek.csv", header = FALSE, encoding = "UTF-8")
colnames(Post_B) <- c("Indeks", "Tytul", "Autor", "Komentarze", "Data", "Spolka")
Post_B$Data <- as.POSIXct(Post_B$Data, "%Y-%m-%d %H:%M", tz = Sys.timezone())
Post_B <- Post_B[c(2:6)]


#Wczytanie wszystkich nazw plikow tekstowych
files <- list.files(path="Dodatkowe_dane/Kursy", pattern="*.txt", full.names=TRUE, recursive=FALSE)
vb5 <- read.csv(files[1])
vb5 <- vb5[FALSE,] #oproznienie ramki z danych


#Stworzenie jednej ramki danych z kursami spolek
for(i in files){
  info = file.info(i)
  if(info$size != 0){
    vb4 <- read.csv(i)
    vb5 <- rbind(vb5, vb4)
  }
}

#format daty
vb5$Data <- paste(str_sub(vb5$X.DATE., 1, 4), str_sub(vb5$X.DATE., 5, 6), str_sub(vb5$X.DATE., 7, 8), sep = "-")
vb5$Time <- ifelse(nchar(vb5$X.TIME.) == 5, 
                   paste(str_sub(vb5$X.TIME., 1, 1), str_sub(vb5$X.TIME., 2, 3), str_sub(vb5$X.TIME., 4, 5), sep = ":"),
                   paste(str_sub(vb5$X.TIME., 1, 2), str_sub(vb5$X.TIME., 3, 4), str_sub(vb5$X.TIME., 5, 6), sep = ":"))
vb5$Data2 <- as.POSIXct(paste(vb5$Data, vb5$Time), format="%Y-%m-%d %H:%M:%S")

del_col <- c("X.DATE.", "X.TIME.", "Data", "Time")
vb5 <- vb5[, !(names(vb5) %in% del_col)]
colnames(vb5)[1] <- "Skrot"

skr_sp <- read.csv("Dodatkowe_dane/Skroty_spolek.csv")
colnames(skr_sp) <- c("Skrot", "Nazwa_spolki")

join_vb5 <- left_join(vb5, skr_sp, by = "Skrot")
tes <- join_vb5 %>% filter(is.na(Nazwa_spolki)) #sprawdzenie NA
colnames(join_vb5)[10] <- "Nazwa_sp"


#zwalnianie miejsca
rm(info, vb4, files, i, del_col, tes, skr_sp, vb5)


#PETLA DO DOPASOWANIA POBLISKICH KURSOW WZGLEDEM CZASU WYSTAWIENIA CZASU#
#UWAGA!#
#PETLA NIE JEST DOBRZE ZOPTYMALIZOWANA I DOPASOWANIE TRWALO PONAD 5 GODZIN#

Post_B$MinDat <- NA
Post_B$MaxDat <- NA
Post_B$MinDat <- as.POSIXct(Post_B$MinDat, format="%Y-%m-%d %H:%M:%S", origin = "1970-01-01")
Post_B$MaxDat <- as.POSIXct(Post_B$MaxDat, format="%Y-%m-%d %H:%M:%S", origin = "1970-01-01")
Jaki_Czas <- 1800 #ustalenie roznicy od czasu wystawienia tematu na forum (w sekundach)
Po_czas <- 600 #wplyw na kurs po ilu sekundach

for(x in 1:500){ #nrow(Post_B)
  nz_sp <- Post_B[x, "Spolka"]
  dt_sp <- Post_B[x, "Data"]
  temp_det <- join_vb5 %>% filter(Nazwa_sp == nz_sp)
  dmax <- NA
  dmin <- NA
  
  if(dim(temp_det)[1] != 0){
    for(y in 1:nrow(temp_det)){
      df <- difftime(temp_det[y, "Data2"], dt_sp, units = "secs")
      #print(df)
      
      if(df > 0){
        if(is.na(dmax)){
          dmax <- df
        } else{
          if(dmax > df){dmax = df}
          if(df > Po_czas & dmax <= Jaki_Czas){dmax = df}
        }
      } else {
        if(is.na(dmin)){
          dmin <- df
        } else{
          if(dmin < df){dmin = df}
        }
      }
      
      if(!is.na(dmin) & !is.na(dmax)){
        if(dmax > Jaki_Czas & dmax > Po_czas){
          break
        }
      }
    }
  }
  
  print(x)
  Post_B[x, "MinDat"] <- dt_sp + dmin
  Post_B[x, "MaxDat"] <- dt_sp + dmax
}


Post_B$Mn2 <- difftime(Post_B$MinDat, Post_B$Data,units = "secs")
Post_B$Mx2 <- difftime(Post_B$MaxDat, Post_B$Data, units = "secs")


#zwalnianie miejsca
rm(df, dmax, dmin, dt_sp, Jaki_Czas, nz_sp, Po_czas, temp_det, x, y)


#DODANIE KURSU PRZED WYSTAWIENIEM POSTU
Tytuly_Z_Kursami <- left_join(Post_B, join_vb5, by = c("MinDat" = "Data2", "Spolka" = "Nazwa_sp"))
Tytuly_Z_Kursami <- Tytuly_Z_Kursami %>%
  select(Tytul, Autor, Komentarze, Data, Spolka, MinDat, MaxDat, X.LOW.)
colnames(Tytuly_Z_Kursami)[8] <- "Kurs_Przed"


#DODANIE KURSU PO WYSTAWIENIEM POSTU
Tytuly_Z_Kursami <- left_join(Tytuly_Z_Kursami, join_vb5, by = c("MaxDat" = "Data2", "Spolka" = "Nazwa_sp"))
Tytuly_Z_Kursami <- Tytuly_Z_Kursami %>%
  select(Tytul, Autor, Komentarze, Data, Spolka, MinDat, MaxDat, Kurs_Przed, X.LOW.)
colnames(Tytuly_Z_Kursami)[9] <- "Kurs_Po"


#GOTOWY PLIK DO ZALADOWANIA I DALSZEJ OBROBKI
Tytuly_Z_Kursami <- read.csv("Dodatkowe_dane/Tytuly_Z_Kursami_Po_Obrobce.csv")
#GOTOWY PLIK DO ZALADOWANIA I DALSZEJ OBROBKI


#USTALENIE ROZNICY PRZED I PO WYSTAWIENIU POSTU
Tytuly_Z_Kursami$Roznica <- Tytuly_Z_Kursami$Kurs_Po - Tytuly_Z_Kursami$Kurs_Przed
Tytuly_Z_Kursami$Zmiana <- ifelse(Tytuly_Z_Kursami$Roznica > 0, 2, ifelse(Tytuly_Z_Kursami$Roznica == 0, 1, 0))


#ZAPISANIE TYTULOW POSTOW ORAZ ZMIANY
Tytuly_Komenty <- Tytuly_Z_Kursami %>% filter(!is.na(Zmiana)) %>% select(Tytul, Zmiana)
colnames(Tytuly_Komenty) <- c("description", "rate")
write.csv(Tytuly_Komenty, "Dane_dla_modeli/Tytuly_Komentarze.csv", fileEncoding = "UTF-8")


#ZAPISANIE TYTULOW POSTOW - ZASTAPIENIE PUSTYCH MIEJSC SREDNIMI
SpolkiSrednia <- Tytuly_Z_Kursami %>%
  group_by(Spolka, Autor) %>%
  summarise(sred_przed = round(mean(Kurs_Przed, na.rm = TRUE), 2), 
            sred_po = round(mean(Kurs_Po, na.rm = TRUE), 2))

SpolkiSrednia2 <- Tytuly_Z_Kursami %>%
  group_by(Spolka) %>%
  summarise(sred_przed2 = round(mean(Kurs_Przed, na.rm = TRUE), 2), 
            sred_po2 = round(mean(Kurs_Po, na.rm = TRUE), 2))

SpolkiSrednia2$Roznica1 <- SpolkiSrednia2$sred_po2 - SpolkiSrednia2$sred_przed2
SpolkiSrednia <- left_join(SpolkiSrednia, SpolkiSrednia2, by = "Spolka")
SpolkiSrednia$sred_po <- ifelse(is.nan(SpolkiSrednia$sred_po), SpolkiSrednia$sred_po2, SpolkiSrednia$sred_po)
SpolkiSrednia$sred_przed <- ifelse(is.nan(SpolkiSrednia$sred_przed), SpolkiSrednia$sred_przed2, SpolkiSrednia$sred_przed)
SpolkiSrednia$RoznicaSPA <- SpolkiSrednia$sred_po - SpolkiSrednia$sred_przed

SpolkiSrednia$sred_przed[is.nan(SpolkiSrednia$sred_przed)] <- 0
SpolkiSrednia$sred_po[is.nan(SpolkiSrednia$sred_po)] <- 0

Tytuly_Z_Kursami <- left_join(Tytuly_Z_Kursami, SpolkiSrednia, by = c("Spolka", "Autor")) 
Tytuly_Z_Kursami <- Tytuly_Z_Kursami %>% filter(sred_przed > 0 & sred_po > 0)
Tytuly_Z_Kursami$Kurs_Przed <- ifelse(is.na(Tytuly_Z_Kursami$Kurs_Przed), Tytuly_Z_Kursami$sred_przed, Tytuly_Z_Kursami$Kurs_Przed)
Tytuly_Z_Kursami$Kurs_Po <- ifelse(is.na(Tytuly_Z_Kursami$Kurs_Po), Tytuly_Z_Kursami$sred_po, Tytuly_Z_Kursami$Kurs_Po)

#MOZE WZIAC DODATKOWE DWA PLIKI BAZUJACE TYLKO NA TEMATACH
#ORAZ OPISIE (SPOLKA+AUTOR+TEMAT)

Tytuly_Z_Kursami$Opis <- paste(Tytuly_Z_Kursami$Autor, Tytuly_Z_Kursami$Spolka, Tytuly_Z_Kursami$Tytul, sep = ' ')
Tytuly_Z_Kursami$Roznica <- Tytuly_Z_Kursami$Kurs_Po - Tytuly_Z_Kursami$Kurs_Przed
Tytuly_Z_Kursami$Zmiana <- ifelse(Tytuly_Z_Kursami$Roznica == 0, 1, ifelse(Tytuly_Z_Kursami$Roznica > 0, 2, 0))
Tytuly_Z_Kursami_SPA <- Tytuly_Z_Kursami %>% select(Opis, Zmiana)
Tytuly_Z_Kursami_BEZ <- Tytuly_Z_Kursami %>% select(Tytul, Zmiana)


colnames(Tytuly_Z_Kursami_SPA) <- c("description", "rate")
colnames(Tytuly_Z_Kursami_BEZ) <- c("description", "rate")
write.csv(Tytuly_Z_Kursami_SPA, "Dane_dla_modeli/Tytuly_Komentarze_SPA.csv", fileEncoding = "UTF-8")
write.csv(Tytuly_Z_Kursami_BEZ, "Dane_dla_modeli/Tytuly_Komentarze_BEZ.csv", fileEncoding = "UTF-8")