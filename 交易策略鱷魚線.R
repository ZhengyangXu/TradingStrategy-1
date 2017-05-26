##資料整理####
rm(list=ls());gc()
library(quantmod)
library(data.table)
library(xts)
allstock<-fread("C:\\Users\\boyin\\Downloads\\Rmid.csv", data.table = F, stringsAsFactors = F,na.strings = "NULL",header = T)
head(allstock);str(allstock);tail(allstock)
allstock[,-9]<-apply(allstock[,-9],2,function(x) gsub("-",NA,x))
allstock<-na.omit(allstock)
allstock<-allstock[,-c(1,10)]
allstock[,-c(1,2)]<-apply(allstock[,-c(1,2)],2,function(x) as.numeric(as.character(x)))
allstock$date=paste(paste(substr(allstock$date,1,4),substr(allstock$date,5,6),
                          substr(allstock$date,7,8),sep='-'))



# stock<-allstock[allstock$company=="台積電",]
# stock<- xts(stock[,-c(1,2)], order.by=strptime(stock$date,format = "%Y-%m-%d"))
# ###策略模擬####
Alligatorindicator<-function(stock){
  #鱷魚線
  bluejaw<-lag(SMA((stock[,2]+stock[,3])/2,13),8) #8日前的13平均
  redteeth<-lag(SMA((stock[,2]+stock[,3])/2,8),5)
  greenlip<-lag(SMA((stock[,2]+stock[,3])/2,5),3)
  Alligator<-cbind(bluejaw,redteeth,greenlip)
  sig1<-ifelse(apply(Alligator,1,function(x) x[1]< x[2] && x[2]<x[3]),1,0) #Alligator表格中每一列(橫的) 2>1 3>2
  #外資法人連續三天買超
  foreign<-lag.xts(stock[,6],0:2)
  sig2<-ifelse(apply(foreign,1,function(x) x[1]>0 && x[2]>0 && x[3]>0 ),1,0)
  #量
  ma_volumeday<-runMean(stock[,5],n=8)  #量的8日MA
  sig3<-ifelse((stock[,5]/lag(ma_volumeday[,1],1))>2,1,0)
  #寶塔  今天收盤價>前三天的最高價
  tower<-cbind(stock[,4],lag.xts(stock[,2],1:3)) 
  sig4<-ifelse(apply(tower,1,function(x) x[1]>x[2] & x[1]>x[3] & x[1]>x[4]),1,0)
  ##隔天開盤價>max(收盤或開盤)
  max<-ifelse(stock[,1]>stock[,4],stock[,1],stock[,4]) #1開盤價 4收盤價 4/2開盤價高4/1收盤價
  sig5<-ifelse(stock[,1]>lag(max[,1],1),1,0)
  ##total
  sig <- sig1 +sig2+sig3+sig4+sig5
  #賣出訊號
  sellsig<<-NA
  all<-which(sig>=4)
  all<-all[(all+20)< nrow(stock)]
  if(length(all)!=0){
    for(i in 1:length(all)){
      j<-all[i]+20
      innitial<-rep(stock[all[i],4],21)
      after<-stock[all[i]:j,4]
      return20<-(as.numeric(after[,1])-innitial)/innitial
      a<-which.max(ifelse((return20>0.5) | (return20< -0.015),1,0))
      b<-ifelse((return20>0.5) | (return20< -0.015),1,0)
      if(a==1){
        sellsig[i]<-all[i]+20-1
      }else
        sellsig[i]<-all[i]+a-1  #賣出的天
    }
  }
  #計算損益
  Buydate<-index(stock[all,4])
  Selldate<-index(stock[sellsig,4])
  holdtime<-sellsig-all
  transactioncost=0.3*0.001425;incometax=0.003
  profit<-(as.numeric(stock[sellsig,4])-as.numeric(stock[all,4]))/as.numeric(stock[all,4])
  Sellprice<-as.numeric(stock[sellsig,4])
  Buyprice<-as.numeric(stock[all,4])            
  profit<-profit-transactioncost-incometax
  names(profit)<-Selldate
  return(list(Buydate,Selldate,holdtime,Buyprice,Sellprice,profit))
}
#績效評估####
performance = function (profit,holdtime,Buydate,Selldate) {
  DD = rep(0, length(profit))	## Draw Down
  topprofit = rep(profit[1], length(profit))	## temp maximum profit
  for (m in 2:length(profit)) {
    if (sum(profit[1:m]) > topprofit[m-1]) {
      topprofit[m:length(profit)] = sum(profit[1:m])
    } ## setting top profit
    DD[m] = sum(profit[1:m]) - topprofit[m]	## current draw down
  }
  plot(DD,type="h",col="darkgreen",lwd=2,
       ylab="Return",xlab="Trade Times",font=2,
       main=paste(Buydate[1],"~",Selldate[length(profit)]),
       ylim=c(min(DD),max(cumsum(profit))))
  par(new=T)
  plot(cumsum(profit),type="h",col="Tomato",lwd=2,
       ylab="Return",xlab="Trade Times",font=2,
       main=paste(Buydate[1],"~",Selldate[length(profit)]),
       ylim=c(min(DD),max(cumsum(profit))))
  TPT=rep(1,1);
  i=1
  for (m in 2:length(profit)) {
    if (topprofit[m]>topprofit[m-1]) {
      points(m,topprofit[m],pch=4,col="purple")
      TPT[i]=m
      i=i+1
    }
  }
  ReturnPerTrade<-sum(profit)/length(profit[profit!=0])
  cat("Total Return:",sum(profit)*100,"%","\n"
      ,"Hold Time Average:",mean(holdtime),"\n"
      ,"Trading Times:",length(profit),"\n"
      ,"Return Per Trade:", ReturnPerTrade*100,"%","\n"
      ,"Annualized Return:", (((1+ReturnPerTrade)^(252/mean(holdtime)))-1)*100,"%","\n"
      ,"# of Win:",length(profit[profit>0]),"\n"
      ,"Win Rate:",length(profit[profit>0])*100/length(profit[profit!=0]),"%","\n"
      ,"Winning Average:",mean(profit[profit>0]),"\n"
      ,"Lossing Average:",mean(profit[profit<0]),"\n"
      ,"Maximum Draw Down:",abs(min(DD)),"\n"
      ,"Profit Factor:",sum(profit[profit>0])/-sum(profit[profit<0]),"\n"
      ,"Total Return/MDD:",sum(profit)/abs(min(DD)),"\n")
}

###多檔股票交易紀錄####
j<-0
transaction<-data.frame()
time1<-Sys.time()
for(i in unique(allstock$company)){
  j<-j+1
  allstock1<-allstock[allstock$company %in% i,]
  allstock1 <- xts(allstock1[,-c(1,2)], order.by=strptime(allstock1$date,format = "%Y-%m-%d"))
  if(nrow(allstock1)>2000){
    money<-Alligatorindicator(allstock1)
    transaction1<-cbind(company=rep(i,length(money[[1]])),Buydate=substr(money[[1]],1,10),Selldate=substr(money[[2]],1,10),holdtime=money[[3]],Buyprice=money[[4]],Sellprice=money[[5]],profit=money[[6]])
    transaction<-rbind(transaction,transaction1)
    cat(sprintf("目前正在回測%s資料，進度：%d / %d \n",i,j,length(unique(allstock$company))))
  }
}
Spendtime<-Sys.time()-time1
transaction[,4:7]<-apply(transaction[,4:7],2,function(x) as.numeric(as.character(x)))
transaction$Selldate<-as.Date(transaction$Selldate)
transaction$Buydate<-as.Date(transaction$Buydate)

#排序交易
library(dplyr)
transaction<-arrange(transaction,Selldate)
performance(transaction$profit,transaction$holdtime,transaction$Buydate,transaction$Selldate)
