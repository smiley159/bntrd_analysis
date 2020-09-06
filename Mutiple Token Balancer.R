
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
  as.POSIXct(x/1000, origin='1970-01-01', tz="Asia/Bangkok")%>% return
} 

#coin_list = c("TRX","ETH","BNB","LTC","XRP","EOS","XLM","ADA","XMR","DASH","IOTA","NEO","ETC","XEM","ZEC","VET","LINK","XTZ")
#coin_list = c(coin_list,c("KNC","WAVES","REN","THETA","YFI","DGB","DAI","DOGE","QTUM","COMP","BTT","ALGO","ZRX","BAT","SNX","ONT","MKR","LEND","OMG"))
#coin_list = c("LINK","ETH","BNB","XRP")

# Beginning of 2019
coin_list = c("ETH","BSV","XTZ","ATOM","LEO"
               ,"HT","MIN","ETC","USDC","HEDG","MIOTA","MKR","CRO","DASH","ONT"
               ,"BAT","INO","ZEC","FTT","DCR","INB","PZM","THR"
               ,"RVN","THX","BDX","ZRX","SEELE","CNX","OKB","HOT","CENNZ","REP","BTG",
               "ZB","ABBC","XIN","MOF","KCS","KBC","CCA","ZEN","FST","EON","LSK","DGB","LUNA"
               ,"BTM","MCO","ENJ","GAP","BTT","KMD","ICX","BCD","IOST","XVG","FAB","BXK","NEXO","SC","SXP"
               ,"VSYS","NRG","BCN","MONA","NOAH","HC","CSC","EKT","BRZE","QNT","MB","QC")
f = list.files()
f = f[grepl("BTC_4h",f)]
f = sub("BTC_4h.csv","",f)
coin_list = coin_list[coin_list %in% f]


record_list = list()

for(sym in coin_list){
  time_frame = "4h"
  window = 21
  sim_list = list()
  
  
    print(sym)
    temp = read.csv(paste0(sym,"BTC_",time_frame,".csv")) %>% setDT()
    temp = temp[order(closeTime)]
    temp[,mean := rollmean(close,window,fill = NA,align = "right")]
    temp[,sd := c(rep(NA,window-1),rollapply(data = close,width=window,FUN=sd))]
    temp[,zscore := (close-mean)/sd]
    temp[,symbol := sym]
    temp = temp[,list(closeTime,close,symbol,zscore)]
    sim_list[[length(sim_list)+1]] = temp



v = rbindlist(sim_list)
v = rbind(v,data.table(close = 1,closeTime = unique(v$closeTime),symbol = "BTC",zscore = 0))
v[,closeTimeDate := millisecond_to_date(closeTime)]
v = v[order(closeTime,zscore)][!is.na(zscore)]
v = v[closeTimeDate >= "2019-01-01"]
v = v[closeTime %% 100000 == 99999]

######################################################

starting_BTC = 0
amt_per_trade = 1 # half of the remaining
max_per_coin = 1 #20%
min_zscore = 2.5
#data_point = data_point[data_point %% 100000 == 99999]
data_point = sort(unique(v$closeTime))


last_data_point = length(data_point)
#last_data_point = 100

balance_tracker = v[closeTime == data_point[1],list(
  closeTime,symbol
  ,balance = 0
  ,btc_equivalent = 0
  ,close)]
#balance_tracker = rbind(balance_tracker,data.table(closeTime = 1514750399999,symbole = "BCH",balance = 0,btc_equivalent = 0))
balance_tracker[symbol == "BTC", `:=`(balance = 1, btc_equivalent = 1)]
balance_tracker_list = list(data.table(balance_tracker))
#balance_tracker[,remaining_quotas := sum(.SD$btc_equivalent)/.N*max_per_coin-btc_equivalent]

# balance_tracker[,btc_equivalent := 1]
# balance_tracker[,balance := 1/close]

trade_list = list()

for(i in 2:last_data_point){
  
  current_table = v[closeTime == data_point[i]]
  if(i %% 50 == 0 ){
    
  print(current_table$closeTimeDate[1])
   percent(i/(last_data_point-1)) %>% paste(sum(balance_tracker$btc_equivalent),collapse = " - ") %>% print()
    
    
  } 
  # We have to find a token with the highest zscore = The one that we want to sell
  #i = data_point[2]
  
  
  trade = current_table[current_table,list(closeTime,symbol,i.symbol,close,i.close,zscore,i.zscore,diff = i.zscore-zscore),on=list(closeTime),allow.cartesian = TRUE][order(-diff)]
  trade_list[[length(trade_list)+1]] = trade
  trade = trade[diff >= min_zscore]
  
  trade[balance_tracker,`:=`(
      balance = i.balance
    ),on = list(symbol)]
  
  trade[balance_tracker,`:=`(
    i.balance = i.balance

    #,remaining_quotas= remaining_quotas
  ),on = list(i.symbol = symbol)]
  
  #trade = trade[!symbol %in% trade$i.symbol]
  #trade = trade[!(i.balance*i.close) >= max_per_coin*sum(balance_tracker$btc_equivalent)]
  trade[,share := (diff-2)/sum(.SD$diff-2),by = list(symbol)]
  trade[,amt := amt_per_trade*balance*share]
  trade[,new_balance := balance*(1-amt_per_trade)]
  trade[,i.new_balance := i.balance+sum(amt*close/i.close)*0.999,by = list(i.symbol)]
  

  seller_balance = trade[amt > 0,list(closeTime,symbol,close,balance = new_balance)] %>% unique
  buyer_balance = trade[amt > 0,list(closeTime,symbol = i.symbol,close = i.close,balance = i.new_balance)] %>% unique

  temp = rbind(seller_balance,buyer_balance)

  
  balance_tracker[temp,`:=`(
    balance = i.balance
  ),on = list(symbol)]
  
  balance_tracker[current_table,`:=`(
    close = i.close,
    closeTime = i.closeTime  
  ),on = list(symbol)]
  
  balance_tracker[,balance := round(balance,8)]
  balance_tracker[,btc_equivalent := balance*close]
  

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


trade_list_table = rbindlist(trade_list)


#record = rbind(record,data.table(symbol = sym,final_btc = sum(balance_tracker$btc_equivalent),price_change = trade_list_table[symbol == sym]$close[nrow(trade_list_table[symbol == sym])]/trade_list_table[symbol == sym]$close[1]  ))
record_list[[length(record_list)+1]] = balance_tracker_table[!symbol == "BTC"]


}

record = rbindlist(record_list)
record = record[!symbol == "BTC"]
record = record[symbol %in% coin_list]
record_summary = dcast(record,closeTimeDate ~ symbol,value.var = "total_btc_equivalent")
record_summary[,investment := length(unique(record$symbol))]
record_summary[, total := rowSums(.SD, na.rm=T), .SDcols=unique(record$symbol)]
record_summary[, perform := percent(total/investment)]


record[,net_price_change := close/.SD$close[1],by=list(symbol)]
price_summary = dcast(record,closeTimeDate ~ symbol,value.var = "net_price_change")
price_summary[,investment := length(unique(record$symbol))]
price_summary[, total := rowSums(.SD, na.rm=T), .SDcols=unique(record$symbol)]
price_summary[, perform := percent(total/investment)]
# record[!symbol == "BTC" & !final_btc == 0,list(
#   investment = .N,current = sum(final_btc)
#   ,perform_btc = percent(sum(final_btc)/.N)
#   ,perform = percent(sum(final_btc)/sum(price_change))
#   )]
# 
# 
# record[,perform_btc := percent(final_btc/price_change)]
# record

