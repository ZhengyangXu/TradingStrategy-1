allstock<-read.csv('C:/Users/user/Desktop/2006-2017/stockprice.csv',header = TRUE)
idd<-read.csv('C:/Users/user/Desktop/2006-2017/aaa.csv',header = TRUE)

#unique(allstock$id)




#length(unique(allstock$id))

#######此區資料唯有問題資料先將其刪除######
#allstock<-allstock[-which(allstock$id==2065),]
#allstock<-allstock[-which(allstock$id==2748),]
#allstock<-allstock[-which(allstock$id==2897),]
#allstock<-allstock[-which(allstock$id==2936),]
#allstock<-allstock[-which(allstock$id==3346),]

#dim(allstock)

#將資料刪去非必要欄位
allstock<-allstock[,-c(2,5,6,8)]
#head(allstock)
#unique(allstock$id)
#profitTable <- NULL #結算表初始
#class(unique(allstock$id)[1:10])
head(allstock,20)


conclusion<- NULL

for(i in idd$id ){  #此區iid為用sas重新篩選之股票
  
  print(i) #紀錄每次做到位置
  
    Data<-allstock[ which(i == allstock$id),]  #當股票為選定股票之代號時將資料存取  (若為1101  則記錄1101所有資料  去做下面回測)

  
 #which('1101' == allstock$id)
 #allstock[ which('1101' == allstock$id),]
  #class('1101')
  
  colnames(Data)<-c("stock","date","open","close")
  library(TTR)
  MA7<-SMA(Data[,4],7)  #計算MA7
  MA30<-SMA(Data[,4],30) #計算MA30
  MA60<-SMA(Data[,4],60)  #計算MA60
  ####以上資料能改為任三條均線
  
  Data3<-cbind(Data,MA7,MA30,MA60)
  colnames(Data3)<-c("stock","date","open","close","MA7","MA30","MA60")
  Data3<-Data3[-c(1:59),]
  head(Data3,20)
  
  #均線糾結  任意兩條均線差距不超過X%
  
  Data3$MA7MA30<-abs((Data3$MA7-Data3$MA30)/(Data3$MA7))    #MA7和MA30 的差距   
  Data3$MA7MA60<-abs((Data3$MA7-Data3$MA60)/(Data3$MA7))
  Data3$MA30MA60<-abs((Data3$MA30-Data3$MA60)/(Data3$MA30))
  Data3$MA30MA7<-abs((Data3$MA30-Data3$MA7)/(Data3$MA30))
  Data3$MA60MA7<-abs((Data3$MA60-Data3$MA7)/(Data3$MA60))
  Data3$MA60MA30<-abs((Data3$MA60-Data3$MA30)/(Data3$MA60))
  
  MRate<-0.05  #設定任意兩條均線的差距  此處選取0.04% 即任兩條均線差距部會超過4%  (此處可變動)
  
  #此處設定初始訊號  若Data滿足均線糾結(三天均線中 任意兩條差距皆在4%內) 即標註1 反之為0
  for (i in 1:nrow(Data3)) {
    ifelse(Data3$MA7MA30[i] < MRate & Data3$MA7MA60[i] < MRate & Data3$MA30MA60[i] < MRate & Data3$MA30MA7[i] < MRate & Data3$MA60MA7[i] < MRate & Data3$MA60MA30[i] < MRate,Data3$Signal2[i] <-1,Data3$Signal2[i]<-0)
    
  }
  head(Data3,350)
  
  #若均線糾結訊號連續出現10天(此處可變動，若只想取三天 ifelse中取到[i-2]即可，若想去到16天 ifelse中則需要多加 &至[i-15]  以此類推)
  #則賦予交易訊號1，其餘則為0
  #此交易手法沒有設定出場點，除非達成停損條件或停利條件，才會出場
  for (i in 11:nrow(Data3)) {
    ifelse(Data3$Signal2[i]==1 & Data3$Signal2[i-1]==1 & Data3$Signal2[i-2]==1 & Data3$Signal2[i-3]==1 & Data3$Signal2[i-4]==1 & Data3$Signal2[i-5]==1 & Data3$Signal2[i-6]==1 & Data3$Signal2[i-7]==1 & Data3$Signal2[i-8]==1 & Data3$Signal2[i-9]==1 & Data3$Signal2[i-10]==1 ,Data3$Signal[i] <-1,Data3$Signal[i]<-0)
    #& Data3$Signal2[i-11]==1  & Data3$Signal2[i-12]==1  & Data3$Signal2[i-13]==1  & Data3$Signal2[i-14]==1  
  }
  head(Data3,350)
  
  Data2<-Data3
  
  # 回測
  stockID<-Data2$stock[1]
  initCash <-1000000
  cash <- initCash
  holdSignal <- 0    #此處為持有信號
  stopLossRatio <- 0.05   #此處為停損比率(可調整)
  stopProfitRatio <- 0.3#此處為停利比率(可調整)
  buyCostR <- 0.001425    #此處為購買成本(交易手續費若為0.3%  則輸入0.003)
  sellCostR <- 0.004425
  stopLossPrice <- 0  #停損金額初始
  stopProfitPrice <- Inf #停利金額初始
  profitTable <- NULL #結算表初始
  cashTable <- NULL  #現金表初始
  buyDate<-20060102
  for(ix in 2:(nrow(Data2)-1)){     #迴圈中 信號第一欄為NA  故從2開始計算
    
    #print(ix)
    
    #若交易信號出現1  且  持有信號為0(持有信號為0表示手上沒有持股)  且  出現紅棒(此處紅K棒表示當天紅K棒的漲幅超過1% (此%數可變動，大紅K可將漲幅調高))
    if(  Data2$Signal[ix]==1   & holdSignal==0 & (((Data2$close[ix]-Data2$open[ix])/(Data2$open[ix]))>0.025)){
      
      holdPrice <- Data2[ix+1,3]   #持有價為隔天的開盤價
      stopLossPrice <- holdPrice*(1-stopLossRatio)   #停損價為購買價 減去 停損比率
      stopProfitPrice <- holdPrice*(1+stopProfitRatio)  #停利價為購買價 加上 停利比率
      holdVolume <- (cash)%/%(holdPrice*(1+buyCostR)*1000)  #購買張數為現金 整除 購買價加上單張交易手續費
      cash <- cash-(holdVolume*holdPrice*(1+buyCostR))*1000  #剩餘現金為  原金額 減去 持有量*當時購買金額(含手續費) 
      buyDate <- Data2[ix+1,2]  #紀錄購買時間
      holdSignal <- 1   #將信號轉換成1  表示此時為持股狀態
      
    }else if( Data2$Signal[ix]==-1 
             & holdSignal==1){    #因為此策略沒設定賣出訊號  故迴圈部會跑入此層  Data2$Signal[ix]==-1
                                  #(as.Date(paste0(substr(Data2[ix+1,2],1,4),"-",substr(Data2[ix+1,2],5,6),"-",substr(Data2[ix+1,2],7,8)))-
      as.Date(paste0(substr(buyDate,1,4),"-",substr(buyDate,5,6),"-",substr(buyDate,7,8)))==11)
      #進入此層條件為 1.發現賣出訊號 -1  2.持股狀態(非持股狀態沒股票賣)
      # 賣出訊號出場
      sellPrice <- Data2[ix+1,3]   #紀錄賣出金額 此賣出金額為 發現賣出訊號隔天開盤價
      cash <- cash + holdVolume*sellPrice*(1-sellCostR)*1000   #記錄當時現金  
      
      sellDate <- Data2[ix+1,2]  #紀錄賣出日期 此處賣出日期為 發現賣出信號的隔天
      tradeDetail <- c(stockID,buyDate, sellDate, holdPrice, sellPrice, holdVolume, 1)  #紀錄交易細節    最後的1表示 此交易細節是偵測到賣出訊號才出場 
      profitTable <- rbind(profitTable, tradeDetail)  #將現有的結算表(剛開始是空值) 逐欄加入交易細節
      
      holdVolume <- 0   #將交易量歸0 代表持有股票全數脫手
      holdSignal <- 0   #將交易訊號歸0 代表脫手後 進入非持股狀態   可以重新偵測購買訊號
      
    }else if(Data2$close[ix]<=stopLossPrice & holdSignal==1){    #此處為停損出場條件  
      #出場條件為 1.當日收盤價 低於 停損金額(此停損金額已在購買時已設定)
      #           2.持股狀態(非持股狀態沒股票賣)
      # 停損出場
      sellPrice <- Data2[ix+1,3]   #紀錄賣出價  為偵測到停損訊號後隔天的開盤價
      cash <- cash + holdVolume*sellPrice*(1-sellCostR)*1000  #紀錄現金 為 剩餘現金 + 持有張數*交易價(含手續費)
      
      sellDate <- Data2[ix+1,2]   #紀錄賣出日期
      tradeDetail <- c(stockID ,buyDate, sellDate, holdPrice, sellPrice, holdVolume, 2)  #紀錄交易細節  最後的2表示  此交易是偵測到停損條件才出場
      profitTable <- rbind(profitTable, tradeDetail)  #將現有的結算表(剛開始是空值) 逐欄加入交易細節
      
      holdVolume <- 0  #將交易量歸0 代表全數脫手
      holdSignal <- 0  #將交易訊號歸0  代表脫手後 進入非持股狀態   可以重新偵測交易訊號
      
    }else if(Data2$close[ix]>=stopProfitPrice & holdSignal==1){    #此處為停利出場條件
      #出場條件為 1.當日收盤價 高於 停利金額(此停利金額在購買時已設定)
      #           2.持股狀態(非持股狀態沒股票賣)
      
      # 停利出場
      sellPrice <- Data2[ix+1,3]   #紀錄交易價
      cash <- cash + holdVolume*sellPrice*(1-sellCostR)*1000   #紀錄現金  為  剩餘金額 + 持有張數*交易價(含手續費)
      
      sellDate <- Data2[ix+1,2]  #紀錄購買日期
      tradeDetail <- c(stockID,buyDate, sellDate, holdPrice, sellPrice, holdVolume, 3)  #紀錄交易細節  最後的3表示   此交易是偵測到停利條件才出場
      profitTable <- rbind(profitTable, tradeDetail)  #將現有結算表(剛開始是空值)  逐欄加入交易細節
      
      holdVolume <- 0   #將交易量歸0 代表全數脫手
      holdSignal <- 0   #將交易訊號歸0 代表脫手後 進入非持股狀態
      
    }
    
    cashTable <- rbind(cashTable, c(Data2[ix+1,2], cash))   #每一次for迴圈 記錄一次剩餘金額(此表格方便作圖)
    
  }
  

  #profitTable <- as.data.frame(profitTable)
  #colnames(profitTable) <- c("stockID","buyDate", "sellDate", "buyPrice", "sellPrice", "holdVolume", "type")
  #profitTable$gain <-  (profitTable$sellPrice*(1-sellCostR) - profitTable$buyPrice*(1+buyCostR))*profitTable$holdVolume*1000  #將最後的結算表 加入 單次交易的賺賠金額
  #profitTable$cumGain <- cumsum(profitTable$gain)+ initCash  #將賺賠金額加總 加上 初始金額   設定為累積金額
  #conclusion<-rbind(conclusion,profitTable)
  if(!is.null(profitTable)){
    profitTable <- as.data.frame(profitTable)
    colnames(profitTable) <- c("Code","buyDate", "sellDate", "buyPrice", "sellPrice", "holdVolume", "type")
    profitTable$gain <-  (profitTable$sellPrice*(1-sellCostR) - profitTable$buyPrice*(1+buyCostR))*profitTable$holdVolume*1000  #將最後結算表加入單次交易的賺賠金額
    profitTable$cumGain <- cumsum(profitTable$gain)+ initCash      #將賺賠金額加總 加上 初始金額 設定為累積金額
    conclusion<-rbind(conclusion,profitTable)
  }
  
}


#conclusion$cumGain[which(conclusion$stockID[length(1108)])]
#length(which(conclusion$stockID=='1101'))
#conclusion$cumGain[length(which(conclusion$stockID=='1101'))]


#計算累積獲利
TotalCluData<-NULL


for( ii in unique(conclusion$Code)){
 
  ClumData<-conclusion[which(conclusion$Code == ii),]
  CluVol<-ClumData$cumGain[length(which(conclusion$Code==ii))]
  CluID<-ii
  TradVol<-length(which(conclusion$Code==ii))
  winRate <- sum(ifelse((ClumData$gain>0),1,0))/length(ClumData$gain)
  
  TotalClu<-c(ii,CluVol,TradVol,winRate)
  
  TotalCluData<-rbind(TotalCluData,TotalClu)
  
}
colnames(TotalCluData)<-c("ID","CluProfit","TradeVol","winRate")

#conclusion[which(conclusion$Code == 5304),]

#class(conclusion)

TotalCluData001<-as.data.frame(TotalCluData)
which(TotalCluData001$CluProfit>2000000)
TotalCluData002<-TotalCluData001[which(TotalCluData001$CluProfit>2000000),]    #將累積獲利大於200萬的股票資料輸出
as.data.frame(TotalCluData002)
TotalCluData003<-TotalCluData002[order(TotalCluData002$CluProfit,decreasing = TRUE),]  #將資料排序



write.csv(TotalCluData001,'C:/Users/user/Desktop/2006-2017/TotalCluData001')
write.csv(TotalCluData003,'C:/Users/user/Desktop/2006-2017/TotalCluData003')
write.csv(conclusion,'C:/Users/user/Desktop/2006-2017/conclusion')

conclusion$buyPriceTc <- conclusion$buyPrice*(1+buyCostR)
conclusion$sellPriceTc <- conclusion$sellPrice*(1-sellCostR)
conclusion$retAfterTc <- conclusion$sellPriceTc/conclusion$buyPriceTc-1
avgRet <- mean(conclusion$retAfterTc, na.rm=T)

conclusion$buyDate1 <- as.Date(paste0(substr(conclusion$buyDate,1,4),"-",substr(conclusion$buyDate,5,6),"-",substr(conclusion$buyDate,7,8)))
conclusion$sellDate1 <- as.Date(paste0(substr(conclusion$sellDate,1,4),"-",substr(conclusion$sellDate,5,6),"-",substr(conclusion$sellDate,7,8)))
conclusion$tradeDayDiff <- conclusion$sellDate1-conclusion$buyDate1
mean(conclusion$tradeDayDiff, na.rm=T)



