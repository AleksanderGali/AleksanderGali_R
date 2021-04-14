# zad_1

div <- function(x, y) {
  if (y == 0) {
    ret <- "zero division, not possible"
  }
  else if ((x %% y) == 0){
    ret <- "division possible"
  }
  else {
    ret <- "division with remainder"
  }
  ret
}

div(10, 5)

# -----------------------------------------------------------------------------
# zad_2

train_speed <- function(x=120, y=90) {
  if ((y <= 0) || (x <= 0)) {
    ret <- "bad data"
  }
  else {
    ret <- ((x + y) / 2)
  }
  ret
}

train_speed()

# -----------------------------------------------------------------------------
# zad_3

pea_corr <- function(x, y) {
  
  if (length(x) != length(y)) {
    ret <- "not matching vector size"
  }
  else {
    ret <- cor.test(x, y, method='pearson')
  }
  ret
}

csv_data <- read.csv2("dane.csv")
pea_corr(csv_data$wzrost, csv_data$waga)

# Info o korelacji
# 0 <- brak korelacji
# 1 <- to korelacja bardzo silna

# Opis wyniku
# Wystepuje bardzo silna korelacja miedzy waga, a wzrostem. >0.97.
# p-value jest bardzo male. p-value < 0.05.
# Oznacza to, ze wynik jest dokladny i jest mala szansa, 
# ze jest wynikiem losowego zdazenia.

# wykres pomocniczy
#plot(csv_data$wzrost, csv_data$waga)

# -----------------------------------------------------------------------------
# zad_4

stworzDataFrame <- function(ile=1) {

  col_list <- readline(prompt="Enter columns names: ")
  col_vector <- unlist(strsplit(col_list, " "))
  df <- setNames(data.frame(matrix(ncol = length(col_vector), nrow = ile)), col_vector)
  
  for (x in seq(1, length(col_vector))) {
    
    print(paste0("Number of values to insert (space separated): ", ile))
    print(paste0("Current column is: ", col_vector[x]))
    
    temp1 <- readline(prompt="Enter data: ")
    temp2 <- unlist(strsplit(temp1, " "))
    
    if (length(temp2) != ile) {
      df <- "incorrect length"
    }
    else {
      df[col_vector[x]] <- temp2  
    }

  }
  df
}


stworzDataFrame(2)



# -----------------------------------------------------------------------------
# zad_5

# I z tylu pliow ilu podalismy dla wybranej nazwy kolumny

liczZplikow <- function(sciezka, nazwaKolumny, jakaFunkcja="mean", DlaIluPlikow=1) {
  
  # pobranie odpowiedniej ilosci plikow
  list_files <- list.files(sciezka)
  list_files <- list_files[1:DlaIluPlikow]
  
  setwd(sciezka)
  myfiles <- lapply(list_files, read.csv)
  nazwaKolumny <- paste0("X", nazwaKolumny)
  
  vec <- vector()
 
  for (x in myfiles) {
    x[nazwaKolumny]
    vec <- rbind(vec, x[nazwaKolumny])
  }

  ret <- lapply(vec, jakaFunkcja, na.rm = TRUE)
  ret
}

liczZplikow("/home/ag/R/AleksanderGali_R/smogKrakow/", "142_humidity", "min", 2)
liczZplikow("/home/ag/R/AleksanderGali_R/smogKrakow/", "183_temperature", "mean", 3)
liczZplikow("/home/ag/R/AleksanderGali_R/smogKrakow/", "218_pressure", "max")
liczZplikow("/home/ag/R/AleksanderGali_R/smogKrakow/", "223_pm1", "median", 6)
liczZplikow("/home/ag/R/AleksanderGali_R/smogKrakow/", "142_temperature")

# -----------------------------------------------------------------------------










