#Data<-read.csv('C:/Users/user/Desktop/price.csv',header = FALSE)

####輸入資料########
Data<-read.csv('C:/Users/user/Desktop/玉山.csv',header = FALSE)

colnames(Data)<-c("stock","date","open","close")

####資料加入MA10
library(TTR)
MA10<-SMA(Data[,4],10)
Data2<-cbind(Data,MA10)
Data2<-Data2[-c(1:9),]
head(Data)


####將 收盤價 大於 MA10 的股票做標記######
for (i in 1:nrow(Data2)) {
  if(Data2[i,4]>Data2[i,5])
    Data2[i,6]<-1
  if(Data2[i,4]<Data2[i,5])
    Data2[i,6]<-0
}
colnames(Data2)<-c("stock","date","open","close","MA10","Signal1")
as.numeric(Data2[,6])
head(Data2,10)


####將 標記信號轉換成交易信號 #####
for(i in 2:nrow(Data2)){
  if((as.numeric(Data2[i,6])==1) &((as.numeric(Data2[i,6])-as.numeric(Data2[i-1,6]))==0) )
    Data2[i,7]<-1
  if((as.numeric(Data2[i,6])==1) &((as.numeric(Data2[i,6])-as.numeric(Data2[i-1,6]))==1) )
    Data2[i,7]<-2
  if((as.numeric(Data2[i,6])==0) &((as.numeric(Data2[i,6])-as.numeric(Data2[i-1,6]))==(-1)) )
    Data2[i,7]<-3
  if((as.numeric(Data2[i,6])==0) &((as.numeric(Data2[i,6])-as.numeric(Data2[i-1,6]))==0) )
    Data2[i,7]<-4
}

colnames(Data2)<-c("stock","date","open","close","MA10","Signal1","signal2")
head(Data2,40)


##########回測#######

#####起始金額100萬
#####買進賣出接扣除手續費0.58%
cash<-1000000
page<-(cash%/%((Data2[1,3])*1000))
res<-(cash)-(page*(1000)*(Data2[1,3]))




cash<-1000000
page<-(cash%/%((Data2[2199,3])*1000))
res<-(cash)-(page*(1000)*(Data2[2199,3]))

for(i in 2:nrow(Data2)) {
  
  if(Data2[i,7]==1){
    page<-page
    cash<-0
    res<-res
    print(c(cash,page,res,i))} else
      
      if(Data2[i,7]==2){
        page<-(cash)%/%(Data2[i,3]*1000)
        
        res<-(cash)-(page)*(1000)*(Data2[i,3])-(0.585*Data2[i,3]*10)
        cash<-0
        print(c(cash,page,res,i))} else
          
          if(Data2[i,7]==3){
            cash<-page*(Data2[i,3])*(1000)+res-(0.585*Data2[i,3]*10)
            page<-0
            res<-0
            
            print(c(cash,page,res,i))} else
              
              if(Data2[i,7]==4){
                cash<-cash
                page<-0
                res<-res
                print(c(cash,page,res,i))}
  
}


####計算勝陪比#####

which(Data2$signal2==2)
length(which(Data2$signal2==2))
which(Data2$signal2==3)
length(which(Data2$signal2==3))

Data2[,4][which(Data2$signal2==2)]
Data2[,4][which(Data2$signal2==3)]


rate1<-(Data2[,4][which(Data2$signal2==2)][1:223])-(Data2[,4][which(Data2$signal2==3)][2:224])
rate1
length(which(rate1>0))
####計算最大獲利
max(rate1)

####計算最大虧損
min(rate1)

####計算勝率
rate2<- length(which(rate1>0))/length(which(Data2$signal2==2))
rate2
########################################################
#for(i in 2:nrow(Data2)) {
  
#  if(Data2[i,7]==1){
#    page<-page
#    cash<-0
#    res<-res
#    print(c(cash,page,res,i))} else
    
#   if(Data2[i,7]==2){
#    page<-(cash)%/%(Data2[i,3]*1000)
    
#    res<-(cash)-(page)*(1000)*(Data2[i,3])
#    cash<-0
#    print(c(cash,page,res,i))} else
    
#  if(Data2[i,7]==3){
#    cash<-page*(Data2[i,3])*(1000)+res
#   page<-0
#    res<-0
#    print(c(cash,page,res,i))} else
#    
#  if(Data2[i,7]==4){
#    cash<-cash
#    page<-0
#    res<-res
#    print(c(cash,page,res,i))}
#    
#}




##################################
#length(Data2$V3)

#cash<-1000000
#inital<-0
#QQ<-0

#for (i in 1:nrow(Data2)) {
  
#  if(Data2[i,4]>Data2[i,5])
#  D1<-Data2[i,2]
#  BuyPrice<-Data2[i,3]#如果最後一天出現訊號，需要修正
#  unit1<- cash/(BuyPrice*1000)
    
#  for(j in i:nrow(Data2)){
#    if(BuyPrice < Data2[j,5])
#      D3<-Data2[j,2]
#       SellPrice<-Data2[j,3]
#       cash<-unit1*SellPrice*1000
#    }
#}
#G<-cbind(Data2,0)
#head(G)