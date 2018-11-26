#' @title Finding the similar windows using based on fuzzy-weighted similarity 
#' @author Thi-Thu-Hong Phan, Andre Bigand, Emilie Poisson-Caillault
#' @description Find the similar windows to queries using the fuzzy-weighted similarity to impute large missing data in univariate time series.
#' @import FuzzyR
#' @keywords internal


Finding_fuzzy_based_similar_windows<-function(query,database,i_start,i_finish,step_finding,threshold){

	T<-length(query)
	
		id_found_start=c();
		CC=c()
		dist_found=c();
		i=i_start
		while(i <= i_finish){
			k=i+T-1
			ref=database[i:k]	
			
		
			
			cout=fbsm(query,ref)
			
			if (cout>=threshold)
			{
				id_found_start<-c(id_found_start,i);
				dist_found<-c(dist_found,cout)
				
			}
			
					
			i<-i+step_finding
		}
			
	max_similarity <- max(dist_found)
	id_similar <- id_found_start[which(dist_found==max_similarity)]		
	
	return(list(id_similar,
			
				id_found_start) )
	
}
