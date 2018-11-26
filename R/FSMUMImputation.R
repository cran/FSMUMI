#' @title Imputing large gaps based on the new fuzzy-weighted similarity measure
#' @author Thi-Thu-Hong Phan, Andre Bigand, Emilie Poisson-Caillault
#' @description Fill large gaps in low or uncorrelated multivariate time series using the fuzzy-weighted similarity measure
#' @param data a multivariate signals containing gaps
#' @param large_gap_threshold threshold used to determine a gap is large 
#' @param step_threshold increment used for finding the threshold 
#' @param step_finding increment used for retrieving  a similar sequences to the queries
#' @return returns a completed multivariate time series
#' @import lsa

FSMUMImputation <- function(data, large_gap_threshold, step_threshold, step_finding){
  	if(is.null(ncol(data))){stop("There are no data, please add them!")}
	store_miss=Indexes_size_missing(data)
	DB=.Initialization_imputation_values(data,large_gap_threshold)
	data1=DB$Init_fuzzy_min
	data2=DB$Init_fuzzy_max
	imp_data1=data1
	imp_data2=data2

	store_miss=Indexes_size_missing(data)
	N=nrow(data)
	for (icol in 1:ncol(data)){
		fuzzypos<-which(store_miss[[icol]][,2]>=large_gap_threshold) 
		if(length(fuzzypos)>0){
	
			pos_dtw=store_miss[[icol]][ ,1][fuzzypos]  
			size_dtw=store_miss[[icol]][ ,2][fuzzypos]  
			imp_value_DTW2=c()
		#	print(paste("posdtw, size, colume",pos_dtw,size_dtw,icol) )
				
				for (id in 1: length(pos_dtw)){
					T=size_dtw[id]
					pos=pos_dtw[id]
					# print(pos)
					gap=pos:(pos+T-1)
					# print(pos+T-1)
					
							
			# Finding after the gap
			#		print(paste("Findind on CSDL 2 after the postion:",pos))
					pos_start=pos+T
					Researchbase_a=data2[pos_start:N,icol]
					la=length(Researchbase_a)
					# print(la)
					if (la>2*T)
					{
						ind=pos_start:(pos_start+T-1)
						query_a=data2[ind,icol] #query_a=Researchbase_a[1:T,]
						i_start=T+1
						i_finish=la-T
						# Finding a threshold
						threshold <- Finding_fuzzy_based_global_threshold(query_a,Researchbase_a,i_start,i_finish,step_threshold)
						#print(paste("threshold fuzzy after 2:",threshold))
						#Finding positions having the costs_DTW <= the threshold				
						id_similar_window<-Finding_fuzzy_based_similar_windows(query_a,Researchbase_a,i_start,i_finish,step_finding,threshold)
						id_dtw_imp=id_similar_window[[1]]
					#	print(paste("id_dtw_imp fuzzy after 2:",id_dtw_imp))
						#Performing the imputation values
							id_similar_finish2=id_dtw_imp-1
							id_similar_start2=id_similar_finish2-T + 1
							imp_value2a=Researchbase_a[id_similar_start2:id_similar_finish2]#,icol]
									
					} 
					if (la<=2*T) { imp_value2a=c()}
					
					#Finding before the gap
				#	print(paste("Findind on CSDL 2 before postion:",pos))
					Researchbase_b=data2[1:(pos-1),icol]
					l=length(Researchbase_b)
					
					if (l>2*T) {
						ind=(l-T+1):l
						query_b=Researchbase_b[ind]#,icol]
					
						i_start=1
						i_finish=l-2*T
						
						# Finding a threshold
						
						threshold=Finding_fuzzy_based_global_threshold(query_b,Researchbase_b,i_start,i_finish,step_threshold)
						#print(paste("threshold fuzzy before 2:",threshold))
						#Finding positions having the costs_DTW <= the threshold				
						id_similar_window=Finding_fuzzy_based_similar_windows(query_b,Researchbase_b,i_start,i_finish,step_finding,threshold)
						id_dtw_imp=id_similar_window[[1]]
						#print(paste("id_dtw_imp fuzzy before 2:",id_dtw_imp ))
						
						id_similar_start2=id_dtw_imp+T
						id_similar_finish2=id_similar_start2+T - 1
						imp_value2b=Researchbase_b[id_similar_start2:id_similar_finish2]#,icol]
					
					} 
					if (l<=2*T) {imp_value2b=c()}	
					#Saving the imputation values	#---Chua nghi duoc lam the nao ket hop cac cua so truoc va sau-----------------
					
					if ((length(imp_value2b)>0) && (length(imp_value2a)>0)) {imp_value_dtw=(imp_value2b+imp_value2a)/2} 
					if ((length(imp_value2b)==0) && (length(imp_value2a)>0)) {imp_value_dtw=imp_value2a} 
					if ((length(imp_value2b)>0) && (length(imp_value2a)==0)) {imp_value_dtw=imp_value2b} 	
					#imp_value_DTW2=c(imp_value_DTW2,imp_value_dtw)	
					imp_data2[gap,icol]=imp_value_dtw
				}
		}	
	}
	return (imp_data2)

}