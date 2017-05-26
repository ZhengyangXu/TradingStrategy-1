N<-100
################(1)################
#(1)sum(1:N)
#(2)
N<-100
a<-0
for(i in 1:50){
  a<-a+(2*i-1);
}
a
####################################


###############(2)##################
#(1)factorial(N)
#(2)
N<-100
a<-1
for(i in 1:100){
  a<-a*i
}
a

####################################
##################(3)#################

Student = c(1:12) 
Grade = c(80, 95, 70,64,53,42,15,60,99,78,88,43) 
評分=c(1:12)
df = data.frame(cbind(Student, Grade, 評分)) 

for (i in 1:nrow(df)) {
  if(as.numeric(df[i,2]) < 60){
    df[i,3]<-"E"
  }
  if(as.numeric(df[i,2]) < 70 & as.numeric(df[i,2]) > 59 ){
    df[i,3]<-"D"
  }
  if(as.numeric(df[i,2]) < 80 & as.numeric(df[i,2]) > 69 ){
    df[i,3]<-"C"
  }
  if(as.numeric(df[i,2]) < 90 & as.numeric(df[i,2]) > 79 ){
    df[i,3]<-"B"
  }
  if( as.numeric(df[i,2]) > 89 ){
    df[i,3]<-"A"
  }
}

df


#######################################


S<-100
K<-100
r<-0.01
sigma<-0.3
TT<-0.3
N<-1000
deltatt<-TT/N
u<-exp(sigma*(deltatt)^(0.5))
d<-1/u
q<-(exp(r*deltatt)-d)/(u-d)


DDFF<-as.data.frame(matrix(data=0,nrow = 1000,ncol = 1000))
View(DDFF)

  for(i in seq(N,0)){
    DDFF[1,1001-i]<-max((u^i)*(d^(1000-i))*S-K,0)
}

for (j in 2:1000) {
  for(i in 1:1000){
    DDFF[j,i]<-DDFF[j-1,i]*q+DDFF[j-1,i+1]*(1-q)
  }
}



plot(DDFF[,1])
DDFF[1000,1]






#blackscholes <- function(S, K, r, TT, sigma) {
#  values <- c(2)
  
#  d1 <- (log(S/K)+(r+sigma^2/2)*TT)/(sigma*sqrt(TT))
#  d2 <- d1 - sigma * sqrt(TT)
  
#  values[1] <- S*pnorm(d1) - K*exp(-r*TT)*pnorm(d2)
#  values[2] <- K*exp(-r*TT) * pnorm(-d2) - S*pnorm(-d1)
  
#  values
#}

#blackscholes(100,100,0.01,0.3,0.3)
