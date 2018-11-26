#' @title Imputation of a large gap based on DTW for multivariate signals
#' @author Thi-Thu-Hong Phan, Andre Bigand, Emilie Poisson-Caillault
#' @description Initialize gaps using trapezoidal function.
#' @param data a multivariate signals containing gaps
#' @param large_gap_threshold  a gap is large if its size is larger or equal to the large_gap_threshold
#' This function returns completed multivariate time series


.Initialization_imputation_values<-function(data,large_gap_threshold){
SB1=data
SB2=data
	store_miss=Indexes_size_missing(data)
	for (icol in 1:ncol(data)){
	
	
		pos1= which(store_miss[[icol]][,2]==1)#Positions of the size of missing value are 1 in the store_w matrix
		posDTW=which(store_miss[[icol]][,2]>=large_gap_threshold) #Positions will be applied DTW
		pos_DTW_1=c(posDTW,pos1)
		posMA=setdiff(1:nrow(store_miss[[icol]]),pos_DTW_1)
		# posMA <- which((store_miss[[icol]][, 2]>1)&&(store_miss[[icol]][, 2]<large_gap_threshold))
		#------------------1NA-----------------------------------------------------------------------------
		if(length(pos1)>0){
			p1=store_miss[[icol]][pos1,1]
			imp_value1=imp_1NA(data[,icol],p1)
			SB1[p1,icol]=imp_value1
			SB2[p1,icol]=imp_value1
			}
		
		#-------------------WMA-----------------------------------------------------------------------------
		if(length(posMA)>0){
			pMA=c()
			for (i in posMA){
				count=0
				num=store_miss[[icol]][i,2]
				while (count<num){
					j=store_miss[[icol]][i,1]+count
					pMA=c(pMA,j)
					count=count+1
				}
			}	
		
		imp_valueMA=imp_small_gaps_WMA(data[,icol],posMA,pMA,store_miss[[icol]],1)
		SB1[pMA,icol]=imp_valueMA
		SB2[pMA,icol]=imp_valueMA
		}
	}
	
		#------------Fuzzy logic----------------------------------------------------------------------------
	store_miss2 <- Indexes_size_missing(SB2)
	for (icol in 1:ncol(data)){	
		posDTW=which(store_miss2[[icol]][,2]>=large_gap_threshold)
		if(length(posDTW)>0){
		
			pos_dtw=store_miss2[[icol]][posDTW,1]  
			size_dtw=store_miss2[[icol]][posDTW,2]
			fuzzy_min=c()
			fuzzy_max=c()
			mi=min(data[,icol],na.rm=T) 
			ma=max(data[,icol],na.rm=T) 
			print(paste("Position of gap(s) on the signal",icol,":",pos_dtw))
			
			
			for (id in 1: length(pos_dtw)){
				T=size_dtw[id]
				pos=pos_dtw[id]
			
				alpha_tra=c()
				t1=pos
				t2=t1+T-1
				for (t in t1:t2) alpha_tra=c(alpha_tra,.trapezoid(t,t1,t2))
				# fuzzy_min=c(fuzzy_min,mi*alpha_tra) 
				fuzzy_min=mi*alpha_tra 
				# print(fuzzy_min)
				# fuzzy_max=c(fuzzy_max,ma*alpha_tra) 
				fuzzy_max=ma*alpha_tra 
				SB1[t1:t2,icol]=fuzzy_min
				SB2[t1:t2,icol]=fuzzy_max
			}
		}	
	}
return (list("Init_fuzzy_min"=SB1,"Init_fuzzy_max"=SB2))
 
}