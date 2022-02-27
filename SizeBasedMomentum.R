# Beckett Zhang 
# Wenjie Zhan 
# Kaixin Zhang 
# Ziye Zhang 

rm(list = ls()) # clear memory
load("CRSP_Data_195901-201506.RData")
crsp.data <- crsp.data
startdate<- 196001
enddate<- 201506
unique_months<- sort(unique(crsp.data$yearmonth))
index<- 1:length(unique_months)

momentum_by_size <- function(largecap=TRUE,quantile){
  
  ret_month<- array(NA,c(0))
  number_stocks<- array(NA,c(0))
  
  for (i in index[which(unique_months==startdate)]:index[which(unique_months==enddate)]) {	
    
    print(unique_months[i])
    
    ## calculating momentum for a given i --------------------
    data_i_minus_2<- crsp.data[which(crsp.data$yearmonth==unique_months[i-2]),]
    data_i_minus_12<- crsp.data[which(crsp.data$yearmonth==unique_months[i-12]),]
    data_i = merge(data_i_minus_2, data_i_minus_12, by="CUSIP")
    data_i$momentum<- abs(data_i$PRC.x)/abs(data_i$PRC.y) - 1
    
    # add quantile sorted by size --------------------
    decile_size <- quantile(data_i$ME.x[which(data_i$EXCHCD.x==1)],seq(0,1.0,by = 0.1))
    decile_size[1]<- min(data_i$ME.x)
    decile_size[length(decile_size)]<- max(data_i$ME.x)
    port_decile_size<- cut(data_i$ME.x,breaks=decile_size,labels=F,include.lowest=TRUE)
    data_i$sizegroup <- port_decile_size
    # define whether are large-cap stocks or small-cap stocks --------------------
    if(largecap){
      data_i <- data_i[data_i$sizegroup>=quantile,]
    }else{
      data_i <- data_i[data_i$sizegroup<=quantile,]
    }
    ## sort by momentum--------------------
    decile = quantile(data_i$momentum[which(data_i$EXCHCD.x==1)],seq(0,1.0,by = 0.1))
    decile[1]<- min(data_i$momentum)
    decile[length(decile)]<- max(data_i$momentum) 
    
    port10<- data_i               
    port_decile<- cut(port10$momentum,breaks=decile,labels=F,include.lowest=TRUE)
    
    number_stocks_yearmonth<- array(0,c(0))
    value_wgt<- array(NA,c(nrow(port10),1))
    for (k in 1:10){
      value_wgt[port_decile==k]<- (port10$ME.x)[port_decile==k]/sum((port10$ME.x)[port_decile==k])
      number_stocks_yearmonth<- cbind(number_stocks_yearmonth,length(value_wgt[port_decile==k]))
      #		print(number_stocks_yearmonth)
    }
    number_stocks<- rbind(number_stocks,number_stocks_yearmonth)
    
    port10<- cbind(port10,port_decile,value_wgt)
    
    ## calculate returns of the portfolios in holding period	--------------------
    data_i_holding<- crsp.data[which(crsp.data$yearmonth==unique_months[i]),]
    ret<- array(0,c(0))
    
    for (l in 1:10){
      
      port10_l<- port10[port_decile==l,]
      
      data_j_l = merge(data_i_holding, port10_l, by="CUSIP")
      fn <- function(x) ifelse(length(na.exclude(as.numeric(as.character(x))))!=0, 
                               as.numeric(as.character(x)), 0) # filter out NA returns
      data_j_l$RET = sapply(data_j_l$RET, fn)
      rets = data_j_l$value_wgt * data_j_l$RET
      ret_l = sum(rets)
      
      ret<- cbind(ret,ret_l)		
    }
    ret_month<- rbind(ret_month,ret)
  }
  ret_month <- as.data.frame(ret_month)
  number_stocks <- as.data.frame(number_stocks)
  names(ret_month)=c("Low","2","3","4","5","6","7","8","9","High")
  names(number_stocks)=c("Low","2","3","4","5","6","7","8","9","High")
  return(list("Monthly_return"=ret_month,"Number_Stocks"=number_stocks))
  
}
#############################################################################################
List_largecap <- momentum_by_size(TRUE,10)
List_smallcap <- momentum_by_size(FALSE,1)
Top10_largecap_ret<- List_largecap$Monthly_return
Bottom10_smallcap_ret<- List_smallcap$Monthly_return
#write.csv(Top10_largecap_ret,file="~/Dropbox/Fine 435 Project/Our Data/196001-201506Top10percent_MktCap_monthly_ret.csv")	
#write.csv(Bottom10_smallcap_ret,file="~/Dropbox/Fine 435 Project/Our Data/196001-201506Bottom10percent_MktCap_monthly_ret.csv")	
List_largecap <- momentum_by_size(TRUE,9)
List_smallcap <- momentum_by_size(FALSE,2)
Top20_largecap_ret<- List_largecap$Monthly_return
Bottom20_smallcap_ret<- List_smallcap$Monthly_return
#write.csv(Top20_largecap_ret,file="~/Dropbox/Fine 435 Project/Our Data/196001-201506Top20percent_MktCap_monthly_ret.csv")	
#write.csv(Bottom20_smallcap_ret,file="~/Dropbox/Fine 435 Project/Our Data/196001-201506Bottom20percent_MktCap_monthly_ret.csv")	
List_largecap <- momentum_by_size(TRUE,8)
List_smallcap <- momentum_by_size(FALSE,3)
Top30_largecap_ret<- List_largecap$Monthly_return
Bottom30_smallcap_ret<- List_smallcap$Monthly_return
#write.csv(Top30_largecap_ret,file="~/Dropbox/Fine 435 Project/Our Data/196001-201506Top30percent_MktCap_monthly_ret.csv")	
#write.csv(Bottom30_smallcap_ret,file="~/Dropbox/Fine 435 Project/Our Data/196001-201506Bottom30percent_MktCap_monthly_ret.csv")	
List_largecap <- momentum_by_size(TRUE,7)
List_smallcap <- momentum_by_size(FALSE,4)
Top40_largecap_ret<- List_largecap$Monthly_return
Bottom40_smallcap_ret<- List_smallcap$Monthly_return
#write.csv(Top40_largecap_ret,file="~/Dropbox/Fine 435 Project/Our Data/196001-201506Top40percent_MktCap_monthly_ret.csv")	
#write.csv(Bottom40_smallcap_ret,file="~/Dropbox/Fine 435 Project/Our Data/196001-201506Bottom40percent_MktCap_monthly_ret.csv")	
List_largecap <- momentum_by_size(TRUE,6)
List_smallcap <- momentum_by_size(FALSE,5)
Top50_largecap_ret<- List_largecap$Monthly_return
Bottom50_smallcap_ret<- List_smallcap$Monthly_return
write.csv(Top50_largecap_ret,file="~/Dropbox/Fine 435 Project/Our Data/196001-201506Top50percent_MktCap_monthly_ret.csv")	
write.csv(Bottom50_smallcap_ret,file="~/Dropbox/Fine 435 Project/Our Data/196001-201506Bottom50percent_MktCap_monthly_ret.csv")	




#############################################################################################
#save(ret_month,file="Output/Replication_Momentum10_Monthly_Returns.RData")	

# Get 1 dollar evolution of the 20 portfolios over the period --------------------
One_dollar_evolution <- function(data){
  data <- as.data.frame(data)
  names(data)=c("Low","2","3","4","5","6","7","8","9","High")
  cumret_largecap_month <- rbind(rep(1,10),1+data)
  for (i in 1:10){
    cumret_largecap_month[,i]=cumprod(cumret_largecap_month[,i])
  }
  return(cumret_largecap_month)
}
#############################################################################################
largecap_evolution <- One_dollar_evolution(List_largecap$Monthly_return)
smallcap_evolution <- One_dollar_evolution(List_smallcap$Monthly_return)
# Plot Large Cap Returns ------------------------------------------------------------

plot(ts(largecap_evolution[,1],start=1959 + 12/12,frequency=12),type = "l",
     ylim=c(min(largecap_evolution),max(largecap_evolution)),col=1,lwd=2,ylab=c("largecap"))
for(i in 2:10){
lines(ts(largecap_evolution[,i],start=1959 + 12/12,frequency=12),type = "l",col=i,lwd=2)
}
legend("topleft",names(largecap_evolution),col=1:10,lty=1,bty="n",lwd=2)

# Plot Small Cap Returns --------------------------------------------------

plot(ts(smallcap_evolution[,1],start=2000 + 1/12,frequency=12),type = "l",
     ylim=c(min(smallcap_evolution),max(smallcap_evolution)),col=1,lwd=2,ylab=c("smallcap"))
for(i in 2:10){
  lines(ts(smallcap_evolution[,i],start=2000 + 1/12,frequency=12),type = "l",col=i,lwd=2)
}
legend("topleft",names(smallcap_evolution),col=1:10,lty=1,bty="n",lwd=2)

## Professor's original ret_month for all stocks
## test correlations and R_2
load("~/Dropbox/Fine 435 Project/Replication_Momentum10_Monthly_Returns.RData")	
FF_factors<- read.table("~/Fine435/FFfactors.txt",header=TRUE,sep="")
FF_factors <- FF_factors[FF_factors[,1]>=196001 & FF_factors[,1]<=201506,]
FF_factors[,2:5] <- FF_factors[,2:5]/100
summary(lm((ret_month[,10]-ret_month[,1])~FF_factors[,2]+
             FF_factors[,3]+FF_factors[,4]))
FF_factors$umd <- ret_month[,10]-ret_month[,1]
library(psych)
pairs.panels(FF_factors[,c(2,3,4,6)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
#############################################################################################
## Compare Size Cut-off
Top10Large_cap_MOM <- read.csv("~/Dropbox/Fine 435 Project/Our Data/196001-201506Top10percent_MktCap_monthly_ret.csv")
Bottom10Small_cap_MOM <- read.csv("~/Dropbox/Fine 435 Project/Our Data/196001-201506Bottom10percent_MktCap_monthly_ret.csv")
Top20Large_cap_MOM <- read.csv("~/Dropbox/Fine 435 Project/Our Data/196001-201506Top20percent_MktCap_monthly_ret.csv")
Bottom20Small_cap_MOM <- read.csv("~/Dropbox/Fine 435 Project/Our Data/196001-201506Bottom20percent_MktCap_monthly_ret.csv")
Top30Large_cap_MOM <- read.csv("~/Dropbox/Fine 435 Project/Our Data/196001-201506Top30percent_MktCap_monthly_ret.csv")
Bottom30Small_cap_MOM <- read.csv("~/Dropbox/Fine 435 Project/Our Data/196001-201506Bottom30percent_MktCap_monthly_ret.csv")
Top40Large_cap_MOM <- read.csv("~/Dropbox/Fine 435 Project/Our Data/196001-201506Top40percent_MktCap_monthly_ret.csv")
Bottom40Small_cap_MOM <- read.csv("~/Dropbox/Fine 435 Project/Our Data/196001-201506Bottom40percent_MktCap_monthly_ret.csv")
Top50Large_cap_MOM <- read.csv("~/Dropbox/Fine 435 Project/Our Data/196001-201506Top50percent_MktCap_monthly_ret.csv")
Bottom50Small_cap_MOM <- read.csv("~/Dropbox/Fine 435 Project/Our Data/196001-201506Bottom50percent_MktCap_monthly_ret.csv")

rf_mean=mean(FF_factors[,5])*1200

mean=mean(Top10Large_cap_MOM[,11]-Top10Large_cap_MOM[,2])*1200
vol=sd(Top10Large_cap_MOM[,11]-Top10Large_cap_MOM[,2])*100*sqrt(12)
sharpe=(mean-rf_mean)/vol

mean=mean(Bottom10Small_cap_MOM[,11]-Bottom10Small_cap_MOM[,2])*1200
vol=sd(Bottom10Small_cap_MOM[,11]-Bottom10Small_cap_MOM[,2])*100*sqrt(12)
sharpe=(mean-rf_mean)/vol

mean=mean(Top20Large_cap_MOM[,11]-Top20Large_cap_MOM[,2])*1200 ## Most difference
vol=sd(Top20Large_cap_MOM[,11]-Top20Large_cap_MOM[,2])*100*sqrt(12)
sharpe=(mean-rf_mean)/vol

mean=mean(Bottom20Small_cap_MOM[,11]-Bottom20Small_cap_MOM[,2])*1200 ## Most difference
vol=sd(Bottom20Small_cap_MOM[,11]-Bottom20Small_cap_MOM[,2])*100*sqrt(12) ## Most difference
sharpe=(mean-rf_mean)/vol

mean=mean(Top30Large_cap_MOM[,11]-Top30Large_cap_MOM[,2])*1200
vol=sd(Top30Large_cap_MOM[,11]-Top30Large_cap_MOM[,2])*100*sqrt(12)
sharpe=(mean-rf_mean)/vol

mean=mean(Bottom30Small_cap_MOM[,11]-Bottom30Small_cap_MOM[,2])*1200
vol=sd(Bottom30Small_cap_MOM[,11]-Bottom30Small_cap_MOM[,2])*100*sqrt(12)
sharpe=(mean-rf_mean)/vol

mean=mean(Top40Large_cap_MOM[,11]-Top40Large_cap_MOM[,2])*1200
vol=sd(Top40Large_cap_MOM[,11]-Top40Large_cap_MOM[,2])*100*sqrt(12)
sharpe=(mean-rf_mean)/vol

mean=mean(Bottom40Small_cap_MOM[,11]-Bottom40Small_cap_MOM[,2])*1200
vol=sd(Bottom40Small_cap_MOM[,11]-Bottom40Small_cap_MOM[,2])*100*sqrt(12)
sharpe=(mean-rf_mean)/vol

mean=mean(Top50Large_cap_MOM[,11]-Top50Large_cap_MOM[,2])*1200
vol=sd(Top50Large_cap_MOM[,11]-Top50Large_cap_MOM[,2])*100*sqrt(12)
sharpe=(mean-rf_mean)/vol

mean=mean(Bottom50Small_cap_MOM[,11]-Bottom50Small_cap_MOM[,2])*1200
vol=sd(Bottom50Small_cap_MOM[,11]-Bottom50Small_cap_MOM[,2])*100*sqrt(12)
sharpe=(mean-rf_mean)/vol
#############################################################################################
apply(Top20Large_cap_MOM[,2:11],2,mean)*1200
apply(Bottom20Small_cap_MOM[,2:11],2,mean)*1200
Evolution_Top20 <- c(1,cumprod(1+(Top20Large_cap_MOM[,11]-Top20Large_cap_MOM[,2])))
Evolution_Bottom20 <- c(1,cumprod(1+(Bottom20Small_cap_MOM[,11]-Bottom20Small_cap_MOM[,2])))
plot(ts(Evolution_Bottom20,start=1959 + 12/12,frequency=12),type = "l",
     ylim=c(min(Evolution_Bottom20),max(Evolution_Bottom20)),col=1,lwd=2,main="Figure 1: One-Dollar Evolution over 1965-2015",ylab="Portfolio Value")
lines(ts(Evolution_Top20,start=1959 + 12/12,frequency=12),type = "l",col=2,lwd=2)
legend("topleft",c("Small-cap Momentum Strategy","Large-cap Momentum Strategy"),col=1:2,lty=1,bty="n",lwd=1)

#############################################################################################
## Test the difference
library(ggplot2)
library(tidyverse)
histdata <- data.frame(Top20=Top20Large_cap_MOM[,11]-Top20Large_cap_MOM[,2],
           Bottom20=Bottom20Small_cap_MOM[,11]-Bottom20Small_cap_MOM[,2]) %>% pivot_longer(cols=everything(),names_to="Size")
ggplot(data=histdata,aes(x=value*12,group=Size))+
  geom_histogram(aes(y=..density..,fill=Size),bins=15,position="dodge")+geom_density(aes(col=Size),size=1,adjust=2)+
  xlim(-2,3)+ggtitle("Density Plot of the Two Momentum Portfolios")+xlab("Annulized Return")+ylab("Density")
## t test of whether the difference is zero
t.test((Top20Large_cap_MOM[,11]-Top20Large_cap_MOM[,2]),alternative = "greater")
t.test(Bottom20Small_cap_MOM[,11]-Bottom20Small_cap_MOM[,2],alternative = "greater")
t.test((Bottom20Small_cap_MOM[,11]-Bottom20Small_cap_MOM[,2])-
         (Top20Large_cap_MOM[,11]-Top20Large_cap_MOM[,2]),alternative = "greater")

#############################################################################################
## During 200001-200912 with high market volatility

Comparison=data.frame(LargeCap_MOM=Top20Large_cap_MOM[480:600,11]-Top20Large_cap_MOM[480:600,2],
                      SmallCap_MOM=Bottom20Small_cap_MOM[480:600,11]-Bottom20Small_cap_MOM[480:600,2],
                      Cross_MOM=Bottom20Small_cap_MOM[480:600,11]-Top20Large_cap_MOM[480:600,2])
CumEvolution=apply((1+Comparison[,1:3]),2,cumprod)
plot(ts(CumEvolution[,1],start=1999 + 12/12,frequency=12),type = "l",
     ylim=c(min(CumEvolution),max(CumEvolution)),col=1,lwd=2,main="1$ Evolution over 2000-2010",ylab="Portfolio Value")
lines(ts(CumEvolution[,2],start=1999 + 12/12,frequency=12),type = "l",col=2,lwd=2)
lines(ts(CumEvolution[,3],start=1999 + 12/12,frequency=12),type = "l",col=3,lwd=2)
legend("topleft",c("Large-cap Momentum Strategy","Small-cap Momentum Strategy","Cross Momentum Strategy"),
       col=1:3,lty=1,bty="n",lwd=2)


load("Monthly_Returns_Rm_196001-201506.RData")	
Comparison2=data.frame(market_risk_return=market_ret_month[480:600,2],
                       Cross_MOM=Bottom20Small_cap_MOM[480:600,11]-Top20Large_cap_MOM[480:600,2])
CumEvolution=apply((1+Comparison2[,1:2]),2,cumprod)
plot(ts(CumEvolution[,1],start=1999 + 12/12,frequency=12),type = "l", ylim=c(min(CumEvolution),max(CumEvolution)),col=4,lwd=2,main="1$ Evolution over 2000-2010",xlab="time",ylab="Portfolio Value")
lines(ts(CumEvolution[,2],start=1999 + 12/12,frequency=12),type = "l",col=3,lwd=2)

legend("topleft",c("Market Portfolio strategy","Cross Momentum Strategy"),
       col=c(4,3),lty=1,bty="n",lwd=2)


load("Size_Monthly_Returns_196307-201506.RData")	
Comparison3=data.frame(size_risk_return=ret_size_month[438:558,1]-ret_size_month[438:558,10],
                       Cross_MOM=Bottom20Small_cap_MOM[480:600,11]-Top20Large_cap_MOM[480:600,2])
CumEvolution=apply((1+Comparison3[,1:2]),2,cumprod)
plot(ts(CumEvolution[,1],start=1999 + 12/12,frequency=12),type = "l", ylim=c(min(CumEvolution),max(CumEvolution)),col=5,lwd=2,main="1$ Evolution over 2000-2010",xlab="time",ylab="Portfolio Value")
lines(ts(CumEvolution[,2],start=1999 + 12/12,frequency=12),type = "l",col=3,lwd=2)

legend("topleft",c("size-risk-factor strategy","Cross Momentum Strategy"),
       col=c(5,3),lty=1,bty="n",lwd=2)


LargeCap_MOM<-Top20Large_cap_MOM[480:600,11]-Top20Large_cap_MOM[480:600,2]
SmallCap_MOM<-Bottom20Small_cap_MOM[480:600,11]-Bottom20Small_cap_MOM[480:600,2]
Cross_MOM<-Bottom20Small_cap_MOM[480:600,11]-Top20Large_cap_MOM[480:600,2]
market_risk_return<-market_ret_month[480:600,2]
size_risk_return<-ret_size_month[438:558,1]-ret_size_month[438:558,10]
mean(LargeCap_MOM)*12*100
mean(SmallCap_MOM)*12*100
mean(Cross_MOM)*12*100
mean(market_risk_return)*12*100
mean(size_risk_return)*12*100

sd(LargeCap_MOM)*sqrt(12)*100
sd(SmallCap_MOM)*sqrt(12)*100
sd(Cross_MOM)*sqrt(12)*100
sd(market_risk_return)*sqrt(12)*100
sd(size_risk_return)*sqrt(12)*100

FF_factors<- read.table("FFfactors.txt",header=TRUE,sep="")
FF_factors <- FF_factors[FF_factors[,1]>=199912 & FF_factors[,1]<=200912,]
FF_factors[,2:5] <- FF_factors[,2:5]/100
FF_factors
minus<-FF_factors[FF_factors[,1]>=199912 & FF_factors[,1]<=200912,5]
minus

SR<-mean(LargeCap_MOM - minus)/sd(LargeCap_MOM)
SR_annualized<-SR*sqrt(12)
SR_annualized

SR<-mean(SmallCap_MOM - minus)/sd(SmallCap_MOM)
SR_annualized<-SR*sqrt(12)
SR_annualized

SR<-mean(Cross_MOM - minus)/sd(Cross_MOM)
SR_annualized<-SR*sqrt(12)
SR_annualized

SR<-mean(market_risk_return - minus)/sd(market_risk_return)
SR_annualized<-SR*sqrt(12)
SR_annualized

SR<-mean(size_risk_return - minus)/sd(size_risk_return)
SR_annualized<-SR*sqrt(12)
SR_annualized


