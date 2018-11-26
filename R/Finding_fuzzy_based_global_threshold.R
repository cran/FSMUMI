#' @title Global threshold based on fuzzy-weighted similarity threshold
#' @author Thi-Thu-Hong Phan, Andre Bigand, Emilie Poisson-Caillault
#' @description Find the global threshold using Fuzzy Logic Theory to impute large missing data in univariate time series.
#' @import FuzzyR
#' @keywords internal

Finding_fuzzy_based_global_threshold<-function(query,database,i_start,i_finish,step_threshold){
T=length(query)
i=i_start
threshold=c();

	while (i <=i_finish){ 
		k=i+T-1
		ref=database[i:k]	
		# print (k)
		cout=fbsm(query,ref)
		# print(cout)
		threshold<-c(threshold,cout)
		i=i+step_threshold
	}
				
seuil=max(threshold)
return (seuil)

}	

