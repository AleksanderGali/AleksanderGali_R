install.packages("tidyverse")

library(DBI)
library(RSQLite)
library(RPostgres)
library(rstudioapi)
library(tidyverse)
library(ggplot2)
library(dplyr)

konta <- read_csv("konta.csv")
View(konta)

lengthOfFile<- function(filepath,systemLinuxUnix=FALSE){
  #if(.Platform$OS.type == "unix" )
  if ( systemLinuxUnix){
    l <- try(system(paste("wc -l",filepath),intern=TRUE))
    l<-strsplit(l,split=" ")
    l<-as.numeric(l[[1]])
    l
  }
  else{
    l<-length(count.fields(filepath))
    l
  }
}

lengthOfFile("konta.csv", TRUE)

start_time <- Sys.time()
lengthOfFile("konta.csv",TRUE)
end_time <- Sys.time()
wyn1<-end_time - start_time

start_time <- Sys.time()
lengthOfFile("konta.csv",FALSE)
end_time <- Sys.time()
wyn2<-end_time - start_time
print(wyn1)
print(wyn2)


srednia<- function(filepath,columnname,header=TRUE,size,sep=","){
  fileConnection<-file(description = filepath,open="r")
  suma<-0
  counter<-0
  data<-read.table(fileConnection,nrows=size,header = header,fill=TRUE,sep=sep)
  columnsNames<-names(data)
  repeat{
    if(nrow(data) == 0){
      break
    }
    data<-na.omit(data)
    suma<- suma + sum(data[[columnname]])
    counter <- counter + nrow(data)
    data<-read.table(fileConnection,nrows=size,col.names = columnsNames,fill=TRUE,sep=sep)
  }
  suma / counter
}

srednia("konta.csv","saldo", size=1000)
mean(konta[["saldo"]], na.rm=TRUE)


readToBase<- function(filepath,dbpath,tablename,header=TRUE,size,sep=",",deleteTable=TRUE){
  ap= !deleteTable
  ov= deleteTable
  fileConnection<- file(description = filepath,open = "r")
  dbConn<-dbConnect(SQLite(),dbpath)
  data<-read.table(fileConnection,nrows=size,header = header,fill=TRUE,sep=sep)
  columnsNames<-names(data)
  dbWriteTable(conn = dbConn,name=tablename,data,append=ap,overwrite=ov)
  repeat{
    if ( nrow(data)==0){
      close(fileConnection)
      dbDisconnect(dbConn)
      break
    }
    data<-read.table(fileConnection,nrows=size,col.names=columnsNames,fill=TRUE,sep=sep)
    dbWriteTable(conn = dbConn,name=tablename,data,append=TRUE,overwrite=FALSE)
  }
}

readToBase("konta.csv", "konta", "konta", size=1000)

dbp="konta"
con=dbConnect(SQLite(),dbp)
tablename="konta"
dbGetQuery(con, paste0("SELECT COUNT(*) FROM ",tablename,";" ) )

#-------------------------------------------------------------------------------
readToBase("konta.csv","bazaKonta.sqlite","konta", size=1000)

dbp="bazaKonta.sqlite"
con=dbConnect(SQLite(),dbp)
tablename="konta"
liczbaWierszyBaza<-dbGetQuery(con, paste0("SELECT COUNT(*) FROM ",tablename,";" ) )
liczbaWierszyBaza
liczbaWierszyPlik<-lengthOfFile("konta.csv", FALSE)
liczbaWierszyBaza

#-------------------------------------------------------------------------------
  
connectMe<-function(typ=Postgres(),dbname="kuwznwts",host="rogue.db.elephantsql.com",user="kuwznwts"){
  con<- dbConnect(typ,
                  dbname=dbname,
                  host=host,
                  user=user,
                  password=askForPassword("database password"))
}

con<-connectMe()

#-------------------------------------------------------------------------------

readToBase1<- function(filepath,dbConn,tablename,header=TRUE,size,sep=",",deleteTable=TRUE){
  ap= !deleteTable
  ov= deleteTable
  fileConnection<- file(description = filepath,open = "r")
  data<-read.table(fileConnection,nrows=size,header = header,fill=TRUE,sep=sep)
  columnsNames<-names(data)
  dbWriteTable(conn = dbConn,name=tablename,data,append=ap,overwrite=ov)
  repeat{
    if ( nrow(data)==0){
      close(fileConnection)
      dbDisconnect(dbConn)
      break
    }
    data<-read.table(fileConnection,nrows=size,col.names=columnsNames,fill=TRUE,sep=sep)
    dbWriteTable(conn = dbConn,name=tablename,data,append=TRUE,overwrite=FALSE)
  }
}

con<-connectMe()
readToBase1("pjatk_su.csv",con,"suicides",size=1000)
con<-connectMe()
dbGetQuery(con,"SELECT COUNT(*) FROM suicides;")


suicidesFromFile<-read.csv("pjatk_su.csv")
nrow(suicidesFromFile)
object.size(suicidesFromFile)
dbGetInfo(con)
dbListTables(con)
dbListFields(con, "suicides")
suicideTable<-tbl(con, "suicides")
object.size(suicideTable)

suicideTable%>%select(country, year, age, generation)
tabelaR<- suicideTable%>%select(everything())%>%collect()
View(tabelaR)

ggplot(data=suicideTable)+geom_bar(aes(x=country))+coord_flip()
ggplot(data=tabelaR)+geom_bar(aes(x=country))+coord_flip()

start<-Sys.time()
ggplot(data=tabelaR)+geom_bar(aes(x=country))+coord_flip()
stop<-Sys.time()
w2<-stop-start

#-------------------------------------------------------------------------------

tabelaPoland<-filter(tabelaR, country=="Poland")
View(tabelaPoland)

dataPoland <- suicideTable %>%
  filter(country == 'Poland') %>%
  collect()
dataPoland

ggplot(data=dataPoland)+geom_bar(aes(x=country))
ggplot(data=dataPoland)+geom_bar(aes(x=year))
ggplot(data=dataPoland)+geom_point(aes(x=year, y=suicides_no))


























