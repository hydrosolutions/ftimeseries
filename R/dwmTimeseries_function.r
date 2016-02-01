#' A Timeseries Function
#'
#' This function creates daily, weekly, monthly timeseries from daily input data
#' @param dataset, start year, months and day of the timeseries, function how the data should be combined (mean or sum)
#' @seealso \code{\link{zooreg}} which creates the timeseries
#' @export
#' @examples
#' dwmTimeseries(xData,y,m,d,aggFun)

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