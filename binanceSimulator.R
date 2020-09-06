

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

kline_sim = read.csv("BTCUSDT_4h.csv") %>% setDT()
kline_sim = kline_sim[,list(closeTimeDate,closeTime,close)]
kline_boundary = read.csv("BTCUSDT_4h.csv") %>% setDT()
kline_boundary = kline_boundary[,list(closeTimeDate,closeTime,close)]

kline_sim[,close := 1/close]
kline_boundary[,close := 1/close]

interval = 4*60*60*1000

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
kline_boundary[,mean := rollmean(as.numeric(close),200,fill = NA,align = "right")]
kline_boundary[,sd21 := c(rep(NA,199),rollapply(data = close,width=200,FUN=sd))]
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
kline_sim[,close := as.numeric(close)]
kline_sim[, sd_per_mean := sd/close]

##############SIMULATION################

startBNB = 0
startBTC = 2
tradingQty = 200
#tradingSdLimit = 0.00672757475


sim = data.table(kline_sim[as.Date.character(closeTimeDate) >= '2019-08-01'])
#sim = data.table(kline_sim)
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
  geom_line(aes(y = btc_equivalent*5000,color = "BTC Balance"))+
  geom_line(aes(y = did_nothing*5000,color = "BTC Balance if did nothing"))+
  geom_line(aes(y = close,color = "BTC PRICE"))+
  ylab("BTC Price")+
  scale_y_continuous(sec.axis = sec_axis(~./5000, name = "BTC Balance"))+
  geom_point(data=sim_result[decision != "WAIT"],aes(y = close,color = decision))+
  geom_label(data=sim_result[index %% 100 == 1],aes(label= format(round(btc_equivalent, 2), nsmall = 2),y = btc_equivalent*5000),hjust=0,vjust=0)+
  geom_label(data=sim_result[index %% 100 == 1],aes(label= format(round(did_nothing, 2), nsmall = 2),y = did_nothing*5000),hjust=0,vjust=0,colour="Red")+
  geom_bar(aes(y = performance*100),stat = "identity")+
  geom_label(data=sim_result[index %% 100 == 1],aes(label= percent(performance),y = performance*100),hjust=0,vjust=0,colour="Blue")+
  ggtitle("Trade Every 12 Hour, Start with 8000 USD and 1 BTC ,equivalent to 1.91 BTC in value ")+
  theme(plot.title = element_text(hjust = 0.5))
  #geom_point(aes(color = decision))

# ggplot(sim_result[year(as.Date.character(closeTimeDate)) >= 2019],aes(x = as.Date.character(closeTimeDate), y= close ))+ 
#   geom_line()+
#   geom_point(aes(color = decision))+
#   scale_color_manual(values=c("green", "red", "black","black"))
#ggplot(sim_result,aes(x = as.Date.character(closeTimeDate), y= performance_bnb ))+ geom_line()


