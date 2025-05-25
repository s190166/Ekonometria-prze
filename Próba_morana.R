# Bibioteki 
install.packages(c("sf", "spdep", "tmap", "spatialreg", "tidyverse"))
library(sf)
library(spdep)
library(tmap)
library(tidyverse)
library(dplyr)

# wczytanie danych z pliku shape 
eu27 <- st_read("danemapy/EU27Official.shp")

# upewnij się, że oba są typu character
eu27$CNTR_ID <- as.character(eu27$CNTR_ID)
dane_panelowe$ID <- as.character(dane_panelowe$skr)

# teraz dopiero łączymy
eu27_merged <- left_join(eu27, dane_panelowe, by = c("CNTR_ID" = "skr"))

# Tworzymy relacje sąsiedztwa
neigh <- poly2nb(eu27_merged)

# Lista wag z normalizacją i obsługą brakujących sąsiadów
weights_list <- nb2listw(neigh, style = "W", zero.policy = TRUE)

# Podgląd liczby sąsiadów
table(card(neigh))

# Test globalny Morana
moran_global <- moran.test(eu27_merged$PPP, weights_list, zero.policy = TRUE)
print(moran_global)

# Wykres rozproszenia Morana
moran.plot(eu27_merged$PPP, weights_list, zero.policy = TRUE)

# Oblicz lokalny Moran I
local_moran <- localmoran(eu27_merged$PPP, weights_list, zero.policy = TRUE)

# Dodaj wyniki do mapy
eu27_merged$Ii <- local_moran[, "Ii"]
eu27_merged$Pvalue <- local_moran[, "Pr(z > 0)"]
eu27_merged$significant <- eu27_merged$Pvalue < 0.05













