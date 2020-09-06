

library(httr)
library(jsonlite)
library(data.table)
library(roll)
library(dplyr)
library(magrittr)
library(zoo)
library(ggplot2)
library(scales)

setwd("C:/Users/Pumjai/OneDrive - ADDA FOOTWEAR (THAILAND) CO., LTD (1)/R/Binance")
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
    result[,closeTimeDate := sapply(closeTime,millisecond_to_date)]
  },error = function(e){
    print(e)
    print(result)
    
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

total_minute = 1500000
startTime = floor(as.numeric(Sys.time())*1000 - total_minute*60*1000)
startTime = round(startTime/86400000)*86400000 #round to the nearest 15 minute
symbol = "ETHBTC"

kline1d_raw = get_looped_klines(symbol,"1d",startTime,total_minute/(60*24))
write.csv(kline1d_raw,paste0(symbol,"_1d.csv"),row.names = FALSE)

kline12h_raw = get_looped_klines(symbol,"12h",startTime,total_minute/(60*12))
write.csv(kline12h_raw,paste0(symbol,"_12h.csv"),row.names = FALSE)

kline4h_raw = get_looped_klines(symbol,"4h",startTime,total_minute/(60*4))
write.csv(kline4h_raw,paste0(symbol,"_4h.csv"),row.names = FALSE)

kline1h_raw = get_looped_klines(symbol,"1h",startTime,total_minute/(60))
write.csv(kline1h_raw,paste0(symbol,"_1h.csv"),row.names = FALSE)

kline15m_raw = get_looped_klines(symbol,"15m",startTime,total_minute/15)
write.csv(kline15m_raw,paste0(symbol,"_15m.csv"),row.names = FALSE)

kline5m_raw = get_looped_klines(symbol,"5m",startTime,total_minute/5)
write.csv(kline5m_raw,paste0(symbol,"_5m.csv"),row.names = FALSE)

# kline1m_raw = get_looped_klines("BNBBTC","1m",startTime,total_minute)
# write.csv(kline1m_raw,"BNBBTC_1m.csv",row.names = FALSE)
#############read csv #############

kline_sim = read.csv("ETHBTC_12h.csv") %>% setDT()
kline_sim = kline_sim[,list(closeTimeDate,closeTime,close)]
kline_boundary = read.csv("ETHBTC_12h.csv") %>% setDT()
kline_boundary = kline_boundary[,list(closeTimeDate,closeTime,close)]


interval = 12*60*60*1000

kline_sim[,minRound := floor(closeTime/interval)]
kline_sim[,minRound := minRound-min(kline_sim$minRound)+1]

kline_boundary[,minRound := floor(closeTime/interval)]
kline_boundary[,minRound := minRound-min(kline_boundary$minRound)+1]

##################################


# kline_sim = kline1m_raw[,list(closeTimeDate,closeTime,close,round15)]
# kline_boundary = kline15m_raw[,list(closeTimeDate,closeTime,close,round15)]


##############################
zscore = 2
#kline_boundary[,mean := c(rep(NA,20),rollapply(data = close,width=21,FUN=ema,decay=0.95))]
kline_boundary[,mean := rollmean(as.numeric(close),120,fill = NA,align = "right")]
kline_boundary[,sd21 := c(rep(NA,119),rollapply(data = close,width=120,FUN=sd))]
kline_boundary[,upper_bound := lag(mean+zscore*sd21,1)]
kline_boundary[,lower_bound := lag(mean-zscore*sd21,1)]


kline_sim[kline_boundary,sd := i.sd21,on = "minRound"]
kline_sim[kline_boundary,lower_bound := i.lower_bound,on = "minRound"]
kline_sim[kline_boundary,upper_bound := i.upper_bound,on = "minRound"]





# kline1[,mean := rollmean(as.numeric(close),21,fill = NA,align = "right")]
# kline1[,sd21 := c(rep(NA,20),rollapply(data = close,width=21,FUN=sd))]
# kline1[,upper_bound := lag(mean+zscore*sd21,1)]
# kline1[,lower_bound := lag(mean-zscore*sd21,1)]

kline_sim = kline_sim[!is.na(upper_bound)]

##############SIMULATION################

startBNB = 50
startBTC = 2
tradingQty = 1
#tradingSdLimit = 0.00672757475


sim = data.table(kline_sim)
sim$bnb_balance = startBNB
sim$btc_balance = startBTC
sim$buy = 0
sim[close <= lower_bound, buy:= 1]
sim[close >= upper_bound, buy:= -1]

temp = list()
nextrow = sim[1]
nextrow$decision = "WAIT"
temp[[1]] = nextrow %>% t %>% as.vector

for(i in 2:(nrow(sim)-1)){
  
  if(i %% 1000 == 0 ) print(percent(i/(nrow(sim)-1)))
  row = data.table(nextrow)
  nextrow = sim[i]
  
  if(row$buy == -1 && row$bnb_balance >= tradingQty ){ #SELL BNB
    
    #trading_qty = row$bnb_balance*tradingQty_pct
    nextrow$bnb_balance = row$bnb_balance - tradingQty
    nextrow$btc_balance = row$btc_balance + tradingQty*row$close*0.999
    nextrow$decision = "BUY"
    
    
  }else if(row$buy == 1 && row$btc_balance >= tradingQty*row$close){ #BUY BNB
    
    #trading_qty = row$btc_balance*tradingQty_pct/row$close
    nextrow$bnb_balance = row$bnb_balance + tradingQty*0.999 # add 0.00025 for price diff 0.99975
    nextrow$btc_balance = row$btc_balance - tradingQty*row$close
    nextrow$decision = "SELL"
    
  }else{
    
    nextrow$bnb_balance = row$bnb_balance
    nextrow$btc_balance = row$btc_balance
    nextrow$decision = "WAIT"
    
  }
  temp[[i]] = nextrow %>% t %>% as.vector
  
}


sim_result = as.data.frame(do.call(rbind, temp),stringsAsFactors=FALSE) %>% setDT()
names(sim_result) = names(nextrow)
sim_result[,bnb_balance := as.numeric(bnb_balance)]
sim_result[,btc_balance := as.numeric(btc_balance)]
sim_result[,close := as.numeric(close)]


sim_result[,bnb_equivalent := bnb_balance+btc_balance/close]
sim_result[,btc_equivalent := btc_balance+bnb_balance*close]

#didnothing
sim_result[nrow(sim_result)]$btc_equivalent/(sim_result[1]$bnb_balance*sim_result[nrow(sim_result)]$close + sim_result[1]$btc_balance)
sim_result[,did_nothing := sim_result[1]$btc_balance + sim_result[1]$bnb_balance*close]
sim_result[,performance := btc_equivalent/did_nothing]
sim_result[,close := 1/close]
sim_result[,index := .I]



#sim_result[,closeTimeDate := lapply(closeTime,millisecond_to_date)]

ggplot(sim_result,aes(x = as.Date.character(closeTimeDate)))+ 
  geom_line(aes(y = btc_equivalent*10,color = "BTC Balance"))+
  geom_line(aes(y = did_nothing*10,color = "BTC Balance if did nothing"))+
  geom_line(aes(y = close,color = "BTC PRICE"))+
  ylab("BTC Price")+
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "BTC Balance"))+
  geom_point(data=sim_result[decision != "WAIT"],aes(y = close,color = decision))+
  geom_label(data=sim_result[index %% 100 == 1],aes(label= format(round(btc_equivalent, 2), nsmall = 2),y = btc_equivalent*10),hjust=0,vjust=0)+
  geom_label(data=sim_result[index %% 100 == 1],aes(label= format(round(did_nothing, 2), nsmall = 2),y = did_nothing*10),hjust=0,vjust=0,colour="Red")+
  geom_bar(aes(y = performance*4),stat = "identity")+
  geom_label(data=sim_result[index %% 100 == 1],aes(label= percent(performance),y = performance*4),hjust=0,vjust=0,colour="Blue")+
  ggtitle("Trade Every 12 Hour, Start with 8000 USD and 1 BTC ,equivalent to 1.91 BTC in value ")+
  theme(plot.title = element_text(hjust = 0.5))
#geom_point(aes(color = decision))

# ggplot(sim_result[year(as.Date.character(closeTimeDate)) >= 2019],aes(x = as.Date.character(closeTimeDate), y= close ))+ 
#   geom_line()+
#   geom_point(aes(color = decision))+
#   scale_color_manual(values=c("green", "red", "black","black"))
#ggplot(sim_result,aes(x = as.Date.character(closeTimeDate), y= performance_bnb ))+ geom_line()


