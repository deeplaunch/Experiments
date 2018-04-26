rm(list=ls())

library("rvest")
library(httr)
library(jsonlite)
library(lubridate)
library(stringr)
library(dplyr)
library(data.table)

############################## load daily market data and summarize to monthly level ######################################


marketCap <- "https://coinmarketcap.com/currencies/bitcoin/historical-data/?start=20160101&end=20180309"

marketCap.table <- marketCap %>% 
  read_html()%>% 
  html_nodes(xpath ='//*[@id="historical-data"]/div/div[3]/table')%>% 
  html_table()

marketCap.table <- tbl_df(marketCap.table[[1]])
colnames(marketCap.table) <- make.names(names(marketCap.table))

marketCap.table$Date <- mdy(marketCap.table$Date)
marketCap.table$yearmonth <- year(marketCap.table$Date) * 100 + month(marketCap.table$Date)

marketCap.table$Volume<- as.numeric(gsub(",", "", marketCap.table$Volume) )
marketCap.table$Market.Cap<- as.numeric(gsub(",", "", marketCap.table$Market.Cap) )

marketCap.table$Unit <- marketCap.table$Volume/ marketCap.table$Close

marketCap.sum <- marketCap.table%>%
  select(yearmonth, Volume, Unit, Date)%>% 
  group_by(yearmonth) %>% 
  summarise(MonthEnd = max(Date),TotalVolume = sum(Volume),TotalUnit= sum(Unit))


##############################  Recent Daily Ownership data ######################################


ownership <- "https://bitinfocharts.com/top-100-richest-bitcoin-addresses.html"

ownership.table <- ownership %>% 
  read_html()%>% 
  html_nodes(xpath ='//*[@id="tblOne"]')%>% 
  html_table()
ownership.table <- ownership.table[[1]] 

ownership.table2 <- ownership %>%  # Need to merge two tables due to advertisement
  read_html()%>% 
  html_nodes(xpath ='//*[@id="tblOne2"]')%>% 
  html_table()
ownership.table2 <- ownership.table2[[1]]

colnames(ownership.table2) <- colnames(ownership.table) # Merge
ownership.table <- rbind(ownership.table, ownership.table2)


##############################  6-month Bitcoin Exchange data ######################################

exchange <- "http://data.bitcoinity.org/export_data.csv?c=e&currency=USD&data_type=rank&r=month&t=ae&timespan=6m"

exchange.table <- read.csv(exchange) 

exchange.table <-tbl_df(exchange.table)

exchange.table <- select(exchange.table,-Time) 

summarise_all(exchange.table, funs(sum))

exchange.sum <- summarise_all(exchange.table, funs(sum))

exchange.sum <- exchange.sum / sum(exchange.sum)

exchange.sum <- exchange.sum[order(exchange.sum, decreasing = TRUE)]

##############################  6-month Bitcoin Exchange data ######################################




















############################## load transcation data #########################################

options(stringsAsFactors = FALSE)

revenue.table <- fromJSON("https://api.blockchain.info/charts/miners-revenue?timespan=all&format=json")

