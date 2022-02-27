rm(list = ls()) # clear memory

#############################################################################
# loads data

load("Output/CRSP_Data_195901-201506.RData")

######################################
##       PART I: GLOBAL VARIABLES          ##
######################################

# a year before the actual years
fiscalyear = 1963:2014 #a year before 

sort_month<- 6  ## the month of year when sorting is done and portfolios are formed, June

################################################################################
##       PART II: SORT STOCKS INTO 10 MARKET EQUITY BASED PORTFOLIOS        ##
################################################################################

ret_month<- array(NA,c(0)) # monthly return
number_stocks<- array(NA,c(0))# 
for (i in 1:length(fiscalyear)) {
	
	print(fiscalyear[i])
	
	data<- crsp.data[which((crsp.data$year==fiscalyear[i] & crsp.data$month>=sort_month) | (crsp.data$year==(fiscalyear[i]+1) & crsp.data$month<=sort_month)),]
	# obtain NYSE deciles
	decile = quantile(data$ME[which(data$year==fiscalyear[i] & data$month==sort_month & data$EXCHCD==1)],seq(0,1.0,by = 0.1)) # get 10 cut-off point
	decile[1]<- min(data$ME[which(data$year==fiscalyear[i] & data$month==sort_month)]) 
	decile[length(decile)]<- max(data$ME[which(data$year==fiscalyear[i] & data$month==sort_month)]) 
	
	port10<- data[which(data$year==(fiscalyear[i]) & data$month==sort_month),]               
	port_decile<- cut(port10$ME,breaks=decile,labels=F,include.lowest=TRUE)
	
	number_stocks_year<- array(0,c(0)) # initialize
	value_wgt<- array(NA,c(nrow(port10),1))
	for (k in 1:10){
		value_wgt[port_decile==k]<- (port10$ME)[port_decile==k]/sum((port10$ME)[port_decile==k])
		number_stocks_year<- cbind(number_stocks_year,length(value_wgt[port_decile==k]))  # number of stocks in port 1
		}
	number_stocks<- rbind(number_stocks,number_stocks_year) # add rows
	
	port10<- cbind(port10,port_decile,value_wgt)
 # calculate the monthly returns		
	for (j in 1:12){
 		
# 		start.time <- Sys.time()
		if (j<=6){
			ret<- array(0,c(0))
			data_j<- data[which(data$year==fiscalyear[i] & data$month==j+6),]
			for (l in 1:10){				
				port10_l<- port10[port_decile==l,]  # Set of tickers in Port l (june)
				
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
			# end.time <- Sys.time()
			# time.taken <- end.time - start.time
			# print(time.taken)


		if (j>6){
			ret<- array(0,c(0))
			data_j<- data[which(data$year==(fiscalyear[i]+1) & data$month==j-6),]
			for (l in 1:10){
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
	}

#save(ret_month,file="Output/Replication_FF10_Monthly_Returns_196307-201506.RData")	

#########################################################################
##

## (1) Calculate the average returns on each of the 10 portfolios.

apply(ret_month,2,mean)*12*100

## (2) Repeat (1) but over each of two halves of the sample.

apply(ret_month[1:(nrow(ret_month)/2),],2,mean)*12*100

apply(ret_month[((nrow(ret_month)/2)+1):nrow(ret_month),],2,mean)*12*100

## (3) calculate average returns and volatility of returns on long-short strategy
rp<- ret_month[,1] - ret_month[,ncol(ret_month)]
mean(rp)*1200
sd(rp)*sqrt(12)*100

## (4) calculate kurtosis of the return
kurtosis<- mean((rp-mean(rp))^4)/(sd(rp)^4)

## (5) perform the hypothesis test that the size premium is zero
t_num<- mean(rp)
t_den<- sd(rp)/sqrt(length(rp))
t<- t_num/t_den
qnorm(0.05, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)