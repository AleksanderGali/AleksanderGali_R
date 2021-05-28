###################################################################
# Zadania:
# 1.Utworz funkcje: rankAccount <- function(dataFrame,colName,groupName,valueSort,num)
#    ktora bedzie zwracala dla danej tabeli(dataFrame) n wierszy posiadajace najwieksze wartosci(sortowanie po kolumnie valueSort)
#  
#  dla wybranej grupy(konkretna wartosc komorki , np. "NAUCZYCIEL) z kolumny(colName) np. occupation-zawod.

# 2.Tak jak w 1 tylko z uzyciem datachunku.
# przyklad naglowka:
# rankAccountBigDatatoChunk(filename = "usersAccounts.csv", 1000,
# "occupation", "NAUCZYCIEL", "saldo",10)

# 3.SPRAWIDZIC CZY DA SIE ZROBIC TO SAMO W zapytaniu SQL dla takich wartosci jak: tabelaZbazyDanych,occupation, nauczyciel, saldo
######################################################################

library(DBI)
library(RSQLite)
library(RPostgres)
library(rstudioapi)

konta <- read_csv("konta.csv")
# View(konta)

################################################################################
# Zad1

rankAccount <- function(dataFrame, colName, groupName, valueSort, num) 
{
  # WHERE dataFrame = "NAUCZYCIEL" 
  dataFrame <- subset(dataFrame, dataFrame[[colName]] == groupName)
  
  # ORDER BY saldo DESC
  dataFrame <- dataFrame[order(-dataFrame[valueSort]), ]
  
  # LIMIT num;
  dataFrame <- head(dataFrame, 10)
  dataFrame
}

rankAccount(konta, "occupation", "NAUCZYCIEL", "saldo", 10)

################################################################################
# Zad2

rankAccountBigDatatoChunk <- function(filename, chunkSize, colName, groupName, valueSort, num)
{
  fileConnection<-file(description = filename, open="r")
  
  data <- read.table(fileConnection, nrows=chunkSize, header=TRUE, fill=TRUE, sep=",")
  columnsNames <- names(data)
  
  # Create first 10
  dataFrame <- subset(data, data[[colName]] == groupName)
  dataFrame <- dataFrame[order(-dataFrame[valueSort]), ]
  dataFrame <- head(dataFrame, 10)
  data <-read.table(fileConnection, nrows=chunkSize, col.names = columnsNames, fill=TRUE, sep=",")

  # find top 10 from all the chunks and collect them together
  repeat{
    if(nrow(data) == 0){
     break
    }
   newFrame <- subset(data, data[[colName]] == groupName)
   newFrame <- newFrame[order(-newFrame[valueSort]), ]
   newFrame <- head(newFrame, 10)
   dataFrame <- rbind(dataFrame, newFrame)
   data <-read.table(fileConnection, nrows=chunkSize, col.names = columnsNames, fill=TRUE, sep=",")
 }
  
  # Find the best from the best :)
  dataFrame <- subset(dataFrame, dataFrame[[colName]] == groupName)
  dataFrame <- dataFrame[order(-dataFrame[valueSort]), ]
  dataFrame <- head(dataFrame, 10)
  dataFrame
  
}
rankAccountBigDatatoChunk(filename = "konta.csv", 1000, "occupation", "NAUCZYCIEL", "saldo", 10)

################################################################################
# Zad3

dataFrame <- "konta"
colName <- "occupation"
groupName <- "NAUCZYCIEL"
valueSort <- "saldo"
num <- 10

dbp = "konta"
con = dbConnect(SQLite(),dbp)

dbGetQuery(con, paste0("SELECT * FROM ", dataFrame, " WHERE ", colName, " = \"", groupName, "\" ORDER BY ", valueSort, " DESC LIMIT ", num, ";") )







