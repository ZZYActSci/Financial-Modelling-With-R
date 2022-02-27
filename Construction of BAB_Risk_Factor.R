rm(list = ls()) # clear memory

#############################################################################
# loads data

load("Output/CRSP_Data_195901-201506.RData")

FF<- data.frame(read.table("FFfactors.txt", header=T, sep="", na.strings="na",strip.white=T))

FF_beta<- data.frame(read.table("Portfolios_Formed_on_BETA.txt", header=T, sep="", na.strings="na",strip.white=T))

######################################
##       PART I: GLOBAL VARIABLES          ##
######################################

startdate<- 200001

unique_months<- sort(unique(crsp.data$yearmonth))

index<- 1:length(unique_months)

FF$mkt<- (FF[,2] + FF[,ncol(FF)])/100

################################################################################
##       PART II: SORT STOCKS INTO 10 BETA-SORTED PORTFOLIOS        ##
################################################################################

ret_month<- array(NA,c(0))
number_stocks<- array(NA,c(0))

for (i in seq(index[which(unique_months==startdate)],length(unique_months)-1,by=12)) {
	
	print(unique_months[i])
		
	data_i<- crsp.data[which(crsp.data$yearmonth==unique_months[i]),]
		
	## calculate beta for each stock in portfolio formation month i
	beta_i<- array(NA,c(nrow(data_i),1))
	for (j in 1:nrow(beta_i)){
#		print(j)
		data_i_j<- crsp.data[which((crsp.data$CUSIP)==(data_i$CUSIP)[j] & crsp.data$yearmonth>=unique_months[i-60] & crsp.data$yearmonth<=unique_months[i-1]),]
		data_i_j<- data_i_j[order(data_i_j$yearmonth),]
		data_merge = merge(data_i_j, FF, by="yearmonth")
		if (nrow(data_merge)>=24){
			y_x<- na.exclude(cbind(as.numeric(as.character(data_merge$RET)),data_merge$mkt))
			beta_i[j]<- cov(y_x[,1],y_x[,2])/var(y_x[,2])
			}
		}
		
	data_i$beta<- beta_i
	data_i<- na.exclude(data_i) ## removing stocks with no data on past beta	
			
	decile = quantile(data_i$beta[which(data_i$EXCHCD==1)],seq(0,1.0,by = 0.25))
	decile[1]<- min(data_i$beta) 
	decile[length(decile)]<- max(data_i$beta) 
	
	port10<- data_i               
	port_decile<- cut(port10$beta,breaks=decile,labels=F,include.lowest=TRUE)
	
	number_stocks_yearmonth<- array(0,c(0))
	value_wgt<- array(NA,c(nrow(port10),1))
	for (k in 1:(length(decile)-1)){
		value_wgt[port_decile==k]<- (port10$ME)[port_decile==k]/sum((port10$ME)[port_decile==k])
		number_stocks_yearmonth<- cbind(number_stocks_yearmonth,length(value_wgt[port_decile==k]))
		}
	number_stocks<- rbind(number_stocks,number_stocks_yearmonth)
	
	port10<- cbind(port10,port_decile,value_wgt)
		
	for (j in 1:12){
		
		ret<- array(0,c(0))
		data_j<- crsp.data[which(crsp.data$yearmonth==unique_months[i+j]),]
		for (l in 1:(length(decile)-1)){
			port10_l<- port10[port_decile==l,]
				
			data_j_l = merge(data_j, port10_l, by="CUSIP")
			fn <- function(x) ifelse(length(na.exclude(as.numeric(as.character(x))))!=0, 
    			                         as.numeric(as.character(x)), 0) # filter out NA returns
    		data_j_l$RET.x = sapply(data_j_l$RET.x, fn)
    		rets = data_j_l$value_wgt * data_j_l$RET.x
    		ret_l = sum(rets)
 
			ret<- cbind(ret,ret_l)		
			}
		ret_month<- rbind(ret_month,ret)
		}
	}


	
#save(ret_month,file="Output/Replication_Beta10_Monthly_Returns_196407-201506.RData")	




