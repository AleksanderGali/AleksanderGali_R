install.packages( c("RSelenium","seleniumPipes","dplyr","gtools","stringr", "xml2", "rvest") )

library(RSelenium)
library(seleniumPipes)
library(dplyr)
library(stringr)
library(gtools)
library(xml2)
library(rvest)

# java -jar selenium-server-standalone-3.0.1.jar -port 4444

# ------------------------------------------------------------------------------
# Wstepne testy

remDr <- remoteDr(remoteServerAddr = "http://localhost",
                  port=4444,
                  browserName = "chrome",
                  newSession = TRUE)

remDr %>% maximizeWindow
remDr %>% go("https://www.otomoto.pl/osobowe/ford/mondeo/?search%5Border%5D=created_at%3Adesc&page=1")

# ------------------------------------------------------------------------------
# Pobranie linkow

remDr <- remoteDr(remoteServerAddr = "http://localhost",
                  port=4444,
                  browserName = "chrome",
                  newSession = TRUE)

wektorLinkow<-c()
for( i in 1:50){
  newUrl<- paste0("https://www.otomoto.pl/osobowe/ford/mondeo/?search%5Border%5D=created_at%3Adesc&page=",i)
  remDr%>%go(newUrl)
  elems<- remDr %>% findElements(using="tag name", "h2")

  for( j in 1: length(elems)){
    e<-findElementsFromElement(elems[[j]],using="tag name", "a")
    if( length(e)>0){
      link<- e[[1]]%>%getElementAttribute("href")
      wektorLinkow<-c(wektorLinkow,link)
    }
  }
}
wektorLinkowU<- wektorLinkow%>%unique()

# ------------------------------------------------------------------------------
# scrapping w petli

remDr <- remoteDr(remoteServerAddr = "http://localhost",
                  port=4444,
                  browserName = "chrome",
                  newSession = TRUE)

for( w in 1:length(wektorLinkowU)){
  remDr%>%maximizeWindow
  remDr%>%go(wektorLinkowU[w])
  cena<-NA
  cena<-remDr%>%findElement("class name", "price-wrapper")%>%getElementText()
  cena<-str_split(cena, " P")
  cena<-cena[[1]][1]
  szczegoly<-remDr%>%findElements("class name","offer-params__list" )
  
  listaSzczegolowOpis<-c()
  listaSzczegolowWartosci<-c()
  
  for ( i in 1: length(szczegoly)){
    listaSzczegolowOpis<- c(listaSzczegolowOpis,szczegoly[[i]]%>%findElementsFromElement("class name","offer-params__label") )
    listaSzczegolowWartosci<- c(listaSzczegolowWartosci,szczegoly[[i]]%>%findElementsFromElement("class name","offer-params__value") )
  }
  nazwyKolumn<- lapply(listaSzczegolowOpis,getElementText)%>%unlist()
  wartosci<- lapply(listaSzczegolowWartosci,getElementText)%>%unlist()
  
  df1<- data.frame( matrix(wartosci,nrow=1,ncol=length(wartosci)))
  names(df1)<-nazwyKolumn
  df1<-cbind(cena,df1)
  View(df1)
}

# ------------------------------------------------------------------------------
# funkcja

zrobWiersz<-function(w,wektorLinkow,remDr){
  remDr%>%maximizeWindow
  remDr%>%go(wektorLinkowU[w])
  cena<-NA
  cena<-remDr%>%findElement("class name", "price-wrapper")%>%getElementText()
  cena<-str_split(cena, " P")
  cena<-cena[[1]][1]
  szczegoly<-remDr%>%findElements("class name","offer-params__list" )
  listaSzczegolowOpis<-c()
  listaSzczegolowWartosci<-c()
  
  for ( i in 1: length(szczegoly)){
    listaSzczegolowOpis<- c(listaSzczegolowOpis,szczegoly[[i]]%>%findElementsFromElement("class name","offer-params__label") )
    listaSzczegolowWartosci<- c(listaSzczegolowWartosci,szczegoly[[i]]%>%findElementsFromElement("class name","offer-params__value") )
  }
  
  nazwyKolumn<- lapply(listaSzczegolowOpis,getElementText)%>%unlist()
  wartosci<- lapply(listaSzczegolowWartosci,getElementText)%>%unlist()
  df1<- data.frame( matrix(wartosci,nrow=1,ncol=length(wartosci)))
  names(df1)<-nazwyKolumn
  df1<-cbind(cena,df1)
}

# -------------------------------------------------------------------------------
# Uzycie funkcji z zabezpieczeniem

remDr <- remoteDr(remoteServerAddr = "http://localhost",
                  port=4444,
                  browserName = "chrome",
                  newSession = TRUE)

auta<-NULL
for(w in 1: length(wektorLinkowU)){
  skip<-FALSE
  tryCatch(
    df1<-zrobWiersz(w,wektorLinkowU,remDr),error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  if(is.null(auta)){
    auta<-df1
  }else{
    auta<-smartbind(auta,df1)
    View(auta)
  }
}

# ------------------------------------------------------------------------------
# Do testow

remDr <- remoteDr(remoteServerAddr = "http://localhost",
                  port=4444,
                  browserName = "chrome",
                  newSession = TRUE)

remDr %>% maximizeWindow
remDr%>%go(wektorLinkowU[1])
cena<-NA
cena<-remDr%>%findElement("class name", "price-wrapper")%>%getElementText()
cena<-str_split(cena, " P")
cena<-cena[[1]][1]

szczegoly<-remDr%>%findElements("class name", "offer-params__list" )







