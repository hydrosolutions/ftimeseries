#' A Timeseries Function
#' 
#'  This function creates a scatter plot from time series data.
#'  @param predictions
#'  @param observations
#'  @param h
#'  @param pointS
#'  @param title of the plot, 
#'  @param labels for x- and 
#'  @param y-axis
#'  @param nRuns Ensemble size
#'  @export
#'  @examples 
#'  ensScatterPlot() 
ensScatterPlot <- function(predD,obsD,h,pointS,plotTitle,xlabel,ylabel,nRuns)
{
  le.p <-  length(predD$pred[[1]]);
  qZ.Da.pred.ens <- data.frame(matrix(nrow=le.p*nRuns,ncol = 2))
  for(idx in 1:nRuns){
    qZ.Da.pred.ens[((idx-1)*le.p+1):((idx-1)*le.p+le.p),1] <- (1:le.p) #rep((1:1332),each=nRuns)
    qZ.Da.pred.ens[((idx-1)*le.p+1):((idx-1)*le.p+le.p),2] <- predD$pred[[idx]]
  }
  qZ.Da.pred.ens.summary <- qZ.Da.pred.ens %>% group_by(X1) %>%
    summarize(ymin = min(X2),
              ymax = max(X2),
              ymean = mean(X2))
  ggplot(qZ.Da.pred.ens.summary, aes(x = obsD$y[h$ts], y = ymean)) +
    geom_point(size = pointS) +
    geom_errorbar(aes(ymin = ymin, ymax = ymax)) +
    ggtitle(plotTitle) +
    xlab(xlabel) +
    ylab(ylabel) +
    coord_cartesian(xlim = c(-1,.5), ylim = c(-1,.5)) +
    coord_fixed(ratio=1) +
    geom_abline(intercept = 0 , slope = 1, col="red")
}