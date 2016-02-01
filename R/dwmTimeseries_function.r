#' A Timeseries Function
#'
#' This function creates daily, weekly, monthly timeseries from daily input data
#' @param xData dataset
#' @param y start year of the timeseries
#' @param m start months of the timeseries
#' @param d start day of the timeseries
#' @param aggFun function how the data should be combined (mean or sum)
#' @seealso \code{\link{zooreg}} which creates the timeseries
#' @export
#' @examples
#' qZ <- read.csv(file= file.path(dataPath,"stations/Zhamashike_runoff_00_14.csv"), header=TRUE, sep=",")
#' dwmTimeseries(qZ,2004,04,04,sum)


dwmTimeseries <- function(xData,y,m,d,aggFun)
{
  tStart <- c(y,m,d)
  tStartDate <- as.Date(paste(c(y,"-",m,"-",d),collapse=''))
  qZ <- zooreg(xData, start = tStartDate)
  qZ.Da <- qZ[format(time(qZ), "%m %d") != "02 29"] # remove leap days (makes leap days 'not' predicatble)
  qZ.Mo <- aggregate(qZ.Da,as.yearmon,aggFun) # monthly aggregation
  qZ1 <- as.xts(qZ) # weekly aggregation
  ep <- endpoints(qZ1, on = 'weeks')
  qZ.We <- period.apply(x = qZ1, INDEX = ep, FUN = aggFun)
  qZ.Da <- ts(as.vector(coredata(qZ.Da)), start = tStart,frequency = 365)
  qZ.We <- ts(as.vector(coredata(qZ.We)), start = tStart,frequency = 52)
  qZ.Mo <- ts(as.vector(coredata(qZ.Mo)), start = tStart,frequency = 12)
  tsList <- list(qZ.Da,qZ.We,qZ.Mo)
  return(tsList)
}



