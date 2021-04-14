install.packages("httr")
install.packages("jsonlite")

library("httr")
library("jsonlite")

print("hello")

endpoint <- "api.openweathermap.org/data/2.5/weather?q=Warszawa&appid=1765994b51ed366c506d5dc0d0b07b77"
getWeather <- GET(endpoint)
weatherText <- content(getWeather, "text")
weatherJson <- fromJSON(weatherText, flatten = TRUE)
weatherDF <- as.data.frame(weatherJson)
View(weatherDF)


x <- 123.5
x <- "string"
x <- 20:200

# Jak sprawdzic typ klasy
# class(x)
# typeof(x)

x <- c(1, 2, 3, 4, -1)
x <- as.integer(x)
x <- c(x, FALSE)
class(x)
y <- as.logical(x)
as.numeric(y)
 
x2 <- c(1.2, 2.2, 3.3, 4.4)
v <- c("1", "2", "3", "4")

vector(mode = "logical", length = 10)

v1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
v2 <- c(2)

wynik = v1 - v2
wynik = v1 + v2
wynik = v1 * v2
wynik = v1 / v2
v1%%v2
v1%/%v2

lista <- list(1, 2, 3, 4, 5)
lista <- list( c(1, 2, 3), c("jeden", "dwa", "trzy"), wynik)

wynik[1]
lista[[3]][1]
class(lista[[3]][1])

lista[[3]] [lista[[3]]>1]


# Wybieranie najwiekszej liczy z vektorow 
v1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
v2 <- c(2)
wynik <- v1*v2 
wynik[wynik>10]

# python range(1, 10, 1)
sekwencja <- seq(1, 10, 1)
sekwencja <- seq(10, 1, -1)

# unique
plec <- c("kobieta", "mezczyzna", "mezczyzna", "kobieta")
plec <- as.factor(plec)
plecf <- as.integer(plec)

factor(c("kobieta", "mezczyzna", "mezczyzna", "kobieta"))
unclass(plecf)

factor(c("kobieta", "mezczyzna", "mezczyzna", "kobieta"))

plecf2 <- factor(c("kobieta", "mezczyzna", "mezczyzna", "kobieta"), levels=c("mezczyzna", "kobieta"))
unclass(plecf2)

plecf[3:8] <- NA
brakujace <- is.na(plecf)

# Pobranie elementow ktore nie sa NA
wplec <- plecf[!brakujace]
plecf[complete.cases(plecf)]


mojaMacierz <- matrix(data = seq(1, 90, 1), nrow=10, ncol=9, byrow = TRUE)
mojaMacierz <- matrix(data = seq(1, 45, 1), nrow=10, ncol=9, byrow = TRUE)

# print whole column 
# mojaMacierz[1,]

cbind(v1, wynik)
rbind(v1, wynik)

df <- data.frame(index=1:3, 
                 imie=c("jan", "alina", "bartek"),
                 plec=c("mezczyzna", "kobieta", "mezczyzna"))

# read.csv
df2<-read.table(file="dane.csv")
View(df2)
df2<-read.csv("dane.csv")

df2<-read.csv2("dane.csv")

# ?read.csv


hello <- function(x) {
  print(paste0(x, " witaj R i R studio!!"))
}

dziel <- function(x, y) {
  if(y==0){
    wynik <- "nie dziel przez zero"
  }
  else {
    wynik <- z/y
  }
  wynik
}

dzielKlawiatura <- function() {
  komunikat <- "podaj 2 liczby oddzielone przecinkiem: "
  wektorOdp <- as.numeric(strsplit(readline(komunikat),",")[[1]])
  l1 <- wektorOdp[1]
  l2 <- wektorOdp[2]
  if(l2==0) {
    v <- "nie dziel przez zero"
  }
  else {
    v<-l1/l2
  }
  v
}






