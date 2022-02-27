rm(list = ls()) # clear memory

################################################################################
##       PART I: LOAD DATA        ##
################################################################################

load("Output/CRSP_Data_195901-201506.RData")
ls()
################################################################################
##       PART II: GLOBAL VARIABLES        ##
################################################################################
startdate<- 196001 ## the first month for which the market return will be calculated

unique_months<- sort(unique(crsp.data$yearmonth))

index<- 1:length(unique_months)

################################################################################
##       PART III: CONSTRUCTION OF MARKET RISK FACTOR        ##
################################################################################

ret_month<- array(NA,c(0)) ## save the monthly returns on the market
number_stocks<- array(NA,c(0))
## loop over the number of months for which we want to compute the market return
start.time <- Sys.time()

for (i in index[which(unique_months==startdate)]:length(unique_months)) {
	
	## a given month
	print(unique_months[i])
	
	## for this given month, obtain weights on different stocks using last month's market cap
	data<- crsp.data[which(crsp.data$yearmonth==unique_months[i-1]),]
	
	value_wgt<- (data$ME)/sum(data$ME)
	data$value_wgt<- value_wgt

	number_stocks<- rbind(number_stocks,nrow(data))
		
	## calculate return	for that month
	data_i<- crsp.data[which(crsp.data$yearmonth==unique_months[i]),]
	
	# ## APPROACH 1
	# ret<- 0	
	# start.time <- Sys.time()	
	# for (m in 1:nrow(data)){
		# data_j_m<- data_i[(data_i$CUSIP)==(data$CUSIP)[m],]
		# if (length(na.exclude(as.numeric(as.character(data_j_m[1]))))>0){
			# if (length(na.exclude(as.numeric(as.character(data_j_m$RET))))==0){data_j_m$RET<- 0}
			# ret<- ret + value_wgt[m]*as.numeric(as.character(data_j_m$RET))
			# }						
		# }
	# end.time <- Sys.time()
	# time.taken <- end.time - start.time
	# print(time.taken)
	

	## APPROACH 2
#	start.time <- Sys.time()
	data_i_l = merge(data_i, data, by="CUSIP")
	fn <- function(x) ifelse(length(na.exclude(as.numeric(as.character(x))))!=0, 
    			                         as.numeric(as.character(x)), 0) # filter out NA returns
	data_i_l$RET.x = sapply(data_i_l$RET.x, fn)
	rets = data_i_l$value_wgt * data_i_l$RET.x
    ret = sum(rets)
	# end.time <- Sys.time()
	# time.taken <- end.time - start.time
	# print(time.taken)
    
	
	
	ret_month<- rbind(ret_month,cbind(unique_months[i],ret))
#	print(ret)
	}
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)



## save the output file
save(ret_month,file="Output/Monthly_Returns_Rm_196001-201506.RData")	

##################################################################################
## Assessing Accuracy and Performance

## 1. Calculate the tracking error of the market portfolio constructed above with respect to the rm portfolio in the FFfactors.txt file.

FF_factors<- read.table("FFfactors.txt",header=TRUE,sep="")

mkt<- (FF_factors[FF_factors[,1]>=196001 & FF_factors[,1]<=201506,2] + FF_factors[FF_factors[,1]>=196001 & FF_factors[,1]<=201506,5])/100

tracking_error<- sd(ret_month[,2] - mkt)


## 2. In the same graph, plot (a) the time series of returns on the market portfolio, and (b) the time series of returns on the rm portfolio in the FFfactors.txt file.

plot(ts(ret_month[,2],start=1960 + 1/12,frequency=12),ylab=c("Monthly Returns"),main=c("Comparison Between Us and FF"), col = "blue",lwd=2,lty=1)		      
	  lines(ts(mkt,start=1960 + 1/12,frequency=12), col="red", lty=2,lwd=2) 
	  legend("topright",
		c("Us","FF"),col=c("blue","red"),lty=c(1,2),bty="n",lwd=2)




## 3. If $1 were invested in the market portfolio in 1959:12, what would be the cumulated return on this investment by 2015:06? 

cum_ret<- 1
for (i in 1:nrow(ret_month)){
	cum_ret<- cum_ret*(ret_month[i,2]+1)
	}
print(cum_ret)



## 4. Plot the time series of cumulated returns obtained in (3) above.

cum_ret<- array(1,c(nrow(ret_month)+1))
for (i in 2:(nrow(ret_month)+1)){
	cum_ret[i]<- cum_ret[i-1]*(ret_month[i-1,2]+1)
	}
	
plot(ts(cum_ret,start=1959 + 12/12,frequency=12),ylab=c("Monthly Returns"),main=c("Cumulated Return"), col = "blue",lwd=1,lty=1)		
