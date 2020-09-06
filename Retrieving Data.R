library(httr)
library(jsonlite)
library(data.table)
library(roll)
library(dplyr)
library(magrittr)
library(zoo)
library(ggplot2)
library(scales)

#setwd("C:/Users/Pumjai/OneDrive - ADDA FOOTWEAR (THAILAND) CO., LTD (1)/R/Binance")
options(scipen = 999)

millisecond_to_date = function(x){
  as.numeric(x) %>% divide_by(.,1000) %>%  as.POSIXct(., origin='1970-01-01', tz="Asia/Bangkok") %>% as.character() %>% return
} 

ema = function(vec,decay){
  
  temp = c(1:length(vec))
  temp = rev(decay^temp)
  result = sum(vec*temp)/sum(temp)
  return(result)
  
}


get_klines = function(symbol,interval,limit){
  
  #url = "https://api.binance.com/api/v1/klines?symbol=BNBBTC&interval=15m&limit=1000"
  url = paste0("https://api.binance.com/api/v1/klines?symbol=",symbol,"&interval=",interval,"&limit=",limit)
  result = GET(url) %>% content
  result = lapply(result,unlist) %>% do.call(rbind, .) %>% data.table()
  setnames(result,old = names(result),new = c("openTime", "open", "high", "low", "close", "volume", "closeTime", "quoteAssetVolume", "numberOfTrades", "takerBuyBaseVolume", "takerBuyQuoteVolume", "Ignore"))
  result[,closeTimeDate := vapply(closeTime,millisecond_to_date)]
  return(result)
}

get_klines_with_start = function(symbol,interval,limit,startTime){
  
  # print("startTime")
  # print(as.character(startTime))
  url = paste0("https://api.binance.com/api/v1/klines?symbol=",symbol,"&interval=",interval,"&limit=",limit,"&startTime=",startTime)
  result = GET(url) %>% content
  result = lapply(result,unlist) %>% do.call(rbind, .) %>% data.table()
  tryCatch({
    setnames(result,old = names(result),new = c("openTime", "open", "high", "low", "close", "volume", "closeTime", "quoteAssetVolume", "numberOfTrades", "takerBuyBaseVolume", "takerBuyQuoteVolume", "Ignore"))
    result[,closeTimeDate := millisecond_to_date(closeTime)]
  },error = function(e){
    #print(e)
    #print(result)
    
  })
  
  
  
  return(result)
}

get_looped_klines = function(symbol,interval,startTime,dataPoints){
  
  time_unit = substring(interval,nchar(interval),nchar(interval))
  time_qty =  as.numeric(substring(interval,1,nchar(interval)-1))
  
  if(time_unit == "h"){
    time_qty = time_qty*60
  }else if(time_unit == "d"){
    time_qty = time_qty*60*24
  }else if(time_unit == "w"){
    time_qty = time_qty*60*24*7
  }else{
    
  }
  
  
  n = ceiling(dataPoints/1000) #Number of loops equal loop/1000
  step = 1000*time_qty*60*1000 #1000 loops * n minutes * 60 = seconds * 1000 = milliseconds
  temp = list()
  print(interval)
  
  for(i in c(0:(n-1))){
    
    
    temp[[i+1]] = get_klines_with_start(symbol,interval,"1000", startTime+step*i )
    print(percent((i*1000+nrow(temp[[i+1]]))/dataPoints))
    
  }
  
  result = rbindlist(temp) %>% data.table
  
  result[,close := as.numeric(close)]
  result[,closeTime := as.numeric(closeTime)]
  
  
}


###############################################

## TRY 75,000 Mins = 5,000 datapoints of 15 mins

#coins = c("OKB","KNC","UMA","WAVES","REN","THETA","EWT","YFI","DGB","DAI","DOGE","QTUM","COMP","BTT","ALGO","ZRX","BAT","SNX","HEDG","ONT","MKR","NEM","ZEC","LEND","OMG")

coins = c("BSV","CRO")
f = list.files()
f = f[grepl("BTC_4h",f)]
f = sub("BTC_4h.csv","",f)
#coins = f
for(coin in coins){
  total_minute = 1500000
  startTime = floor(as.numeric(Sys.time())*1000 - total_minute*60*1000)
  startTime = round(startTime/86400000)*86400000 #round to the nearest 15 minute
  symbol = paste0(coin,"BTC")
  #symbol = "BTCUSDT"
  print(symbol)
 
  tryCatch({
      #if(coin %in% f) next
      skip_to_next <<- FALSE
      kline1d_raw = get_looped_klines(symbol,"1d",startTime,total_minute/(60*24)) %>% unique
      write.csv(kline1d_raw,paste0("data/",symbol,"_1d.csv"),row.names = FALSE)
      
      kline12h_raw = get_looped_klines(symbol,"12h",startTime,total_minute/(60*12)) %>% unique
      write.csv(kline12h_raw,paste0("data/",symbol,"_12h.csv"),row.names = FALSE)
      
      kline4h_raw = get_looped_klines(symbol,"4h",startTime,total_minute/(60*4)) %>% unique
      write.csv(kline4h_raw,paste0("data/",symbol,"_4h.csv"),row.names = FALSE)
      
      kline2h_raw = get_looped_klines(symbol,"2h",startTime,total_minute/(60*2)) %>% unique
      write.csv(kline2h_raw,paste0("data/",symbol,"_2h.csv"),row.names = FALSE)
      
      kline1h_raw = get_looped_klines(symbol,"1h",startTime,total_minute/(60)) %>% unique
      write.csv(kline1h_raw,paste0("data/",symbol,"_1h.csv"),row.names = FALSE)
      # 
      # kline15m_raw = get_looped_klines(symbol,"15m",startTime,total_minute/15)
      # write.csv(kline15m_raw,paste0(symbol,"_15m.csv"),row.names = FALSE)
      # 
      # kline5m_raw = get_looped_klines(symbol,"5m",startTime,total_minute/5)
      # write.csv(kline5m_raw,paste0(symbol,"_5m.csv"),row.names = FALSE)
    }
  ,error = function(e) { 
    #print(e)
    skip_to_next <<- TRUE
  })
  
  print(skip_to_next)
  if(skip_to_next == TRUE){ 
    next
  }
  
  
 
}

