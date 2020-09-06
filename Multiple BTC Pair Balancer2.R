
library(httr)
library(jsonlite)
library(data.table)
library(roll)
library(dplyr)
library(magrittr)
library(zoo)
library(ggplot2)
library(scales)

# setwd("C:/Users/Pumjai/OneDrive - ADDA FOOTWEAR (THAILAND) CO., LTD (1)/R/Binance")
options(scipen = 999)

millisecond_to_date = function(x){
  as.POSIXct(x/1000, origin='1970-01-01', tz="Asia/Bangkok")%>% return
} 

#coin_list = c("ETH","XRP","LINK","BCH","LTC","XLM","XTZ","BNB","ADA","EOS") #Top 10 2020-08-23
#coin_list = c("ETH","XRP","ATOM","BCH","LTC","XLM","XTZ","BNB","ADA","EOS") #Top 10 2020-01-01
#coin_list = c("ETH","XRP","TRX","BCH","LTC","XLM","XTZ","BNB","ADA","EOS") #Top 10 2019-01-01
# coin_list = c("XRP","ETH","BNB","LTC","EOS","XLM","BSV","ADA","LEO","XMR","DASH","LINK","NEO","IOTA","ATOM","ETC","XEM","ZEC","ONT","XTZ","QTUM","BTG","VET","CRP","BAT") #2019-07-01
coin_list = c("XRP","ETH","BNB","LTC","EOS","XLM","BSV","ADA","TRX","XMR","DASH","NEO","IOTA","ETC","XEM","VET","OMG","ONT","QTUM","ZEC","ICX","LSK","ZIL","BCN","DCR","BTG","AE","STEEM")#2018-07-01
#coin_list = c("XRP","ETH","LTC","EOS","XLM","ADA","TRX","XMR","DASH","NEO","NANO","ETC","XEM","OMG","ARDR","QTUM","ZEC","ICX","LSK","XVG","BTS","PPT","BTG","STRAT","WAVES")#2018-01-01

#Beginning of 2019
#coin_list = c("BTC","ETH","XRP","LTC","BCH","EOS","BNB","BSV","TRX","ADA","XLM","LEO","XMR","DASH","LINK","NEO","MIOTA","ATOM","ETC","XEM","ZEC","ONT","MKR","XTZ","QTUM","BTG","VET","CRO","BAT","DOGE","USDC","OMG","BTT","DCR","VSYS","HOT","RVN","BCD","LSK","NPXS","EGT","HT","HEDG","AOA","HC","WAVES","ZRX","NANO"
#)
mode = "BTC_"
time_frame = "1h"
window = 40
min_zscore = 2

f = list.files()
f = f[grepl(paste0(mode,time_frame),f)]
f = sub(paste0(mode,time_frame,".csv"),"",f)
coin_list = coin_list[coin_list %in% f]
coin_list = coin_list[1:10]
#coin_list = f

record_list = list()

for(i in 1:length(coin_list)){
  
  sym = coin_list[i]
  print(paste0(sym," ",i,"/",length(coin_list)))
  sim_list = list()
  
  
  temp = read.csv(paste0(sym,mode,time_frame,".csv")) %>% setDT()
  # temp = temp[hour(closeTimeDate) %% 2 == 0]
  temp2 = temp[hour(closeTimeDate) %in% c(2,6,10,14,18,22)]
  
  temp = temp[closeTime >= min(temp2$closeTime)]
  temp[,minRound := floor((closeTime+60*60*1000)/(4*60*60*1000))]
  temp[,minRound := minRound-min(temp$minRound)+1]
  
  temp2[,minRound := floor(closeTime/(4*60*60*1000))]
  temp2[,minRound := minRound-min(temp2$minRound)+1]
  
  if(nrow(temp) < 100) next
  temp = temp[order(closeTime)]
  temp2[,mean := rollmean(close,window,fill = NA,align = "right")]
  temp2[,sd := c(rep(NA,window-1),rollapply(data = close,width=window,FUN=sd))]
  temp[temp2,`:=`(
    mean = i.mean,
    sd = i.sd
  ),on = list(minRound)]
  temp[,zscore := (close-mean)/sd]
  temp[,symbol := sym]
  sim_list[[length(sim_list)+1]] = temp
  
  
  
  v = rbindlist(sim_list)
  #v = rbind(v,data.table(close = 1,closeTime = unique(v$closeTime),symbol = "BTC",zscore = 0))
  v[,closeTimeDate := millisecond_to_date(closeTime)]
  v = v[order(closeTime,zscore)][!is.na(zscore)]
  v = v[closeTimeDate >= "2018-07-01" & closeTimeDate <= "2019-07-01"]
  v = v[closeTime %% 100000 == 99999]
  if(nrow(v) == 0) next
  ######################################################
  
  starting_BTC = 0
  
  
  data_point = sort(unique(v$closeTime))
  last_data_point = length(data_point)
  balance_tracker_list = list()
  btc_balance = 0.5
  balance = 0
  
  for(i in 1:last_data_point){
    
    decision = "wait"
    row = v[closeTime == data_point[i]]
    zscore = row$zscore[1]
    
    if( i == 1) balance = (1-btc_balance)/row$close[1]
    
    if(i %% 100 == 0 ){
      
      print(row$closeTimeDate[1])
      percent(i/(last_data_point-1)) %>% paste(btc_balance+balance*row$close) %>% print()
      
      
    } 
    
    if(abs(zscore) > min_zscore){ 
      if(zscore > 0){ # Buy
        
        if(btc_balance > 0){
          balance = balance+btc_balance/row$close*0.999
          btc_balance = 0
          decision = "Sell BTC"
        }
        
      }else{ #Sell
        if(balance > 0) {
          btc_balance = btc_balance+balance*row$close*0.999
          balance = 0
          decision = "Buy BTC"
        }
      }
    }
    
    balance_tracker = data.table(
      closeTime = row$closeTime,
      symbol = row$symbol,
      balance = balance,
      btc_equivalent = balance*row$close+btc_balance,
      close = row$close,
      decision = decision,
      zscore = zscore
    )
    
    
    
    
    balance_tracker_list[[length(balance_tracker_list)+1]] = data.table(balance_tracker)
    
    
    
    # xx = trade[,list(symbol,close,balance = balance,new_balance = new_balance)] %>% unique
    # yy = trade[,list(symbol = i.symbol,close = i.close,balance = i.balance,new_balance = i.new_balance)] %>% unique
    # zz = rbind(xx,yy)
    # print(sum((zz$balance - zz$new_balance)*zz$close))
    
    
  }
  
  
  balance_tracker_table = rbindlist(balance_tracker_list)
  balance_tracker_table = balance_tracker_table[order(closeTime,symbol)]
  balance_tracker_table[,closeTimeDate := millisecond_to_date(closeTime)]
  balance_tracker_table[,balance_diff := percent(balance/lag(balance,1)-1),by =list(symbol)]
  balance_tracker_table[,price_change := percent(close/lag(close,1)-1),by =list(symbol)]
  balance_tracker_table[,total_btc_equivalent := sum(btc_equivalent,na.rm = TRUE),by=list(closeTime)]
  
  #balance_tracker_table[,list(btc_equivalent = sum(btc_equivalent)),by =list(closeTimeDate)][order(closeTimeDate)] %>% View
  
  
  #trade_list_table = rbindlist(trade_list)
  
  
  #record = rbind(record,data.table(symbol = sym,final_btc = sum(balance_tracker$btc_equivalent),price_change = trade_list_table[symbol == sym]$close[nrow(trade_list_table[symbol == sym])]/trade_list_table[symbol == sym]$close[1]  ))
  record_list[[length(record_list)+1]] = data.table(balance_tracker_table)
  
  
}

btcusdt = read.csv("data/BTCUSDT_4h.csv")
record = rbindlist(record_list)
record = record[,.SD[1],by =list(closeTime,symbol)]
#record = record[!symbol == "BTC"]
record = record[symbol %in% coin_list]
record_summary = dcast(record,closeTimeDate+closeTime ~ symbol,value.var = "total_btc_equivalent")
record_summary[is.na(record_summary)] <- 1
record_summary[,investment := length(unique(record$symbol))]
record_summary[, total := rowSums(.SD, na.rm=T), .SDcols=unique(record$symbol)]
record_summary[, perform := percent(total/investment)]
record_summary[btcusdt,btcusdt := i.close,on = "closeTime"]
record_summary[,usd_equivalent := btcusdt*total]


record[,net_price_change := close/.SD$close[1],by=list(symbol)]
price_summary = dcast(record,closeTimeDate+closeTime ~ symbol,value.var = "net_price_change")
price_summary[is.na(price_summary)] <- 1
price_summary[,investment := length(unique(record$symbol))]
price_summary[, total := rowSums(.SD, na.rm=T), .SDcols=unique(record$symbol)]
price_summary[, perform := percent(total/investment)]
record_summary[price_summary, price_change := i.perform,on = "closeTime"]
record_summary[,net_perform := percent(as.numeric(sub("%","",record_summary$perform))/as.numeric(sub("%","",record_summary$price_change)))]

record[,perform_over_price_change := btc_equivalent/net_price_change]
perform_over_price = dcast(record,closeTimeDate+closeTime ~ symbol,value.var = "perform_over_price_change")
perform_over_price[, total := rowMeans(.SD, na.rm=T), .SDcols=unique(record$symbol)]
perform_over_price = perform_over_price[,lapply(.SD,percent),.SDcols=c(unique(record$symbol),"total") ]

