#' Read and process data.
#' 
#' Reads in time series date read from csv and aggregated values to monthly intervalls. Also produces a rescaled copy of the aggregated time series. 
#' @param data.input Time series to be processed.
#' @param start.date.input Date of the first entry of the time series. e.g. c(2000,01,01)
#' 
readAndProcessData <- function(data.input,start.date.input) {
  qQ <- data.input # input at top
  #qQ <- read.csv(file= file.path(dataPath,"stations/Qilian_runoff_00_14.csv"), header=TRUE, sep=",")
  qQ <- qQ[,-1]
  qQ <- as.vector(as.matrix(qQ))
  qQ <- na.omit(qQ)
  qQts <- dwmTimeseries(qQ,start.date.input[1],start.date.input[2],start.date.input[3],"mean"); 
  
  qQ.Mo <- qQts[[3]]
  
  qQ.Mo.rs <- rescale(qQ.Mo,to=c(-1,1))
  
  correlation.of.data <- findCorrelation(qQ.Mo)
  
  out <- list(qQ.Mo,qQ.Mo.rs)
  return(out)
}