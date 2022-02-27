rm(list = ls()) # clear memory

#############################################################################
# loads data

load("Output/CRSP_Data_195901-201506.RData")

######################################
##       GLOBAL PARAMETERS          ##
######################################

startdate<- 196702
enddate<- 199912

unique_months<- sort(unique(crsp.data$yearmonth))

index<- 1:length(unique_months)

################################################################################
##       PART II: SORT STOCKS INTO 10 MOMENTUM BASED PORTFOLIOS        ##
################################################################################

ret_month<- array(NA,c(0))
number_stocks<- array(NA,c(0))

#for (i in index[which(unique_months==startdate)]:length(unique_months)) {
for (i in index[which(unique_months==startdate)]:index[which(unique_months==enddate)]) {	
	
	print(unique_months[i])
		
	## calculating momentum for a given i
	data_i_minus_2<- crsp.data[which(crsp.data$yearmonth==unique_months[i-2]),]
	data_i_minus_12<- crsp.data[which(crsp.data$yearmonth==unique_months[i-12]),]

	data_i = merge(data_i_minus_2, data_i_minus_12, by="CUSIP")
	data_i$momentum<- abs(data_i$PRC.x)/abs(data_i$PRC.y) - 1
#	data_i<- na.exclude(data_i) 
	
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
		
	## calculate returns of the 10 portfolios in holding period	
	data_i_holding<- crsp.data[which(crsp.data$yearmonth==unique_months[i]),]
	ret<- array(0,c(0))
	
	for (l in 1:10){

		port10_l<- port10[port_decile==l,]

		data_j_l = merge(data_i_holding, port10_l, by="CUSIP")
		fn <- function(x) ifelse(length(na.exclude(as.numeric(as.character(x))))!=0, 
    			                         as.numeric(as.character(x)), 0) # filter out NA returns
    	data_j_l$RET.x = sapply(data_j_l$RET.x, fn)
    	rets = data_j_l$value_wgt * data_j_l$RET.x
    	ret_l = sum(rets)

		ret<- cbind(ret,ret_l)		
		}
	ret_month<- rbind(ret_month,ret)
	}


#save(ret_month,file="Output/Replication_Momentum10_Monthly_Returns.RData")	
#############################################################################################

## 1. What are the average returns on the 10 momentum-sorted portfolios?

means<- apply(ret_month,2,mean)

## 2. What is the average return earned by the Winner-Loser strategy?

means[ncol(ret_month)] - means[1]

## 3. What are the volatilities on the 10 momentum-sorted portfolios?

vol<- apply(ret_month,2,sd)