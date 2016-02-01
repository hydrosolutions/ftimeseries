#' Function for data acquisition. Find correlation in data set.
#' 
#' @param time series
#' @examples 
#' findCorrelation(qQ.Mo)
#' 
findCorrelation <- function(input_data){
  correlation.of.data <-NULL
  input_data.acf <- acf(input_data,lag.max = 15)
  for(i in 1:length(input_data.acf$acf)){
    if(input_data.acf$acf[i] > 0.3){correlation.of.data <-c(correlation.of.data,i)}}
  return(correlation.of.data)
}
