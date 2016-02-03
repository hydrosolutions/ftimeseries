#' Predict discharge with discharge
#' 
#' Function that predicts discharge with discharge.
#' @param data.input Timeseries generated with dwmTimeseries()
#' @param start.date Date, list with year, month, and day. e.g. c(2000,01,01)
#' @param qQ.mlpe Fitted model saved previously
#' @param nRuns Ensemble size
#' @param model.type Which model to run (currently only 'mlpe')
#' @param test.for.fit Which performance criteria to apply (currently only 'MAE')
#' @param error.test Some stuff ('MAE','R22','RMSE')
#' 

predictDischargeWithDischarge2 <- function(data.input,
                                          start.date,
                                          qQ.mlpe,
                                          nRuns,
                                          model.type,
                                          test.for.fit,
                                          error.test){ 
  # HEIHE RIVER RUNOFF PREDICTION
  #
  # tobias siegfried, 07. January 2016
  # Version: 12 Jan. 2016
  
  # INSTALL DOWNSCALING PACKAGE
  #install_github(c("SantanderMetGroup/downscaleR.java","SantanderMetGroup/downscaleR"))
  #install_github(c("SantanderMetGroup/loadeR.java","SantanderMetGroup/loadeR"))
  #install_github("metno/esd") # not working due to ncdf dependence! :(
  
  # REQUIREMENTS ----
  #rm(list = ls()); cat("\014"); graphics.off(); # Clear Workspace
  
  # 
  # 
  # library(devtools)
  # library(forecast); 
  # library(rminer); 
  # library(R.matlab) ;
  # library(scales); 
  # library(timeSeries); 
  # library(ggplot2); 
  # library(plyr); 
  # library(zoo); 
  # library(xts);
  # library(matrixStats) # Load Packages
  # library(dplyr); 
  # library(numDeriv); 
  # library(astsa); 
  # library(tsDyn)
  # library(raster); 
  # library(rgdal);
  # library(ggmap); 
  # library(hddtools)
  # library(RNetCDF)
  # library(rts)
  # library(rgeos)
  # library(ncdf4)
  # library(ftimeseries)
  # library(nnet)
  
  # wdPath <- "~/Dropbox (hydrosolutions)/RRM_Heihe/R";
  # dataPath <- "~/Dropbox (hydrosolutions)/RRM_Heihe/R/data";
  # setwd(wdPath)
  
# INPUT ----
# User
# data.input   <- read.csv(file= file.path(dataPath,"stations/Qilian_runoff_00_14.csv"), header=TRUE, sep=",")

# Import pre-fit model
#load("qQ.mlpe.fit.RData")

# Set-Up
# start.date   <- c(2000,01,01)
# nRuns        <- 5 # for model fitting
# model.type   <- "mlpe"    #options: "cubist" "mars" "randomForest" "ksvm"
# 
# test.for.fit <- "MAE"
# error.test   <- c("MAE","R22", "RMSE")

# # FUNCTIONS ----
# # Function for scatterplot with ensemble mean forecast and range
# ensScatterPlot <- function(predD,obsD,h,pointS,plotTitle,xlabel,ylabel)
# {
#   le.p <-  length(predD$pred[[1]]);
#   qZ.Da.pred.ens <- data.frame(matrix(nrow=le.p*nRuns,ncol = 2))
#   for(idx in 1:nRuns){
#     qZ.Da.pred.ens[((idx-1)*le.p+1):((idx-1)*le.p+le.p),1] <- (1:le.p) #rep((1:1332),each=nRuns)
#     qZ.Da.pred.ens[((idx-1)*le.p+1):((idx-1)*le.p+le.p),2] <- predD$pred[[idx]]
#   }
#   qZ.Da.pred.ens.summary <- qZ.Da.pred.ens %>% group_by(X1) %>%
#     summarize(ymin = min(X2),
#               ymax = max(X2),
#               ymean = mean(X2))
#   ggplot(qZ.Da.pred.ens.summary, aes(x = obsD$y[h$ts], y = ymean)) +
#     geom_point(size = pointS) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax)) +
#     ggtitle(plotTitle) +
#     xlab(xlabel) +
#     ylab(ylabel) +
#     coord_cartesian(xlim = c(-1,.5), ylim = c(-1,.5)) +
#     coord_fixed(ratio=1) +
#     geom_abline(intercept = 0 , slope = 1, col="red")
# }
# # Function to generate timeseries
# dwmTimeseries  <- function(xData,y,m,d,aggFun)
# {
#   tStart <- c(y,m,d)
#   tStartDate <- as.Date(paste(c(y,"-",m,"-",d),collapse=''))
#   qZ <- zooreg(xData, start = tStartDate)
#   qZ.Da <- qZ[format(time(qZ), "%m %d") != "02 29"] # remove leap days (makes leap days 'not' predicatble!)
#   qZ.Mo <- aggregate(qZ.Da,as.yearmon,aggFun) # monthly aggregation
#   qZ1 <- as.xts(qZ) # weekly aggregation
#   ep <- endpoints(qZ1, on = 'weeks')
#   qZ.We <- period.apply(x = qZ1, INDEX = ep, FUN = aggFun)
#   qZ.Da <- ts(as.vector(coredata(qZ.Da)), start = tStart,frequency = 365)
#   qZ.We <- ts(as.vector(coredata(qZ.We)), start = tStart,frequency = 52)
#   qZ.Mo <- ts(as.vector(coredata(qZ.Mo)), start = tStart,frequency = 12)
#   tsList <- list(qZ.Da,qZ.We,qZ.Mo)
#   return(tsList)
# }

# DATA ACQUISITION ----

# Generate monthly TimeSeries ----
qQ           <- data.input # input at top
qQ           <- qQ[,-1]
qQ           <- as.vector(as.matrix(qQ))
qQ           <- na.omit(qQ)
qQts         <- dwmTimeseries(qQ,start.date[1],start.date[2],start.date[3],"mean")
qQ           <- qQts[[3]]


# Find correlation ----
correlation.of.data <- NULL
qQ.acf       <- acf(qQ,lag.max = 15)
for(i in 1:length(qQ.acf$acf)){
                if((qQ.acf$acf[i] > 0.3)|(qQ.acf$acf[i] < -0.3)){correlation.of.data <-c(correlation.of.data,i)}}

## Prepare Cases Series ----
qQ.lag       <- CasesSeries(qQ,correlation.of.data)

# Prepare objects ----
P1           <- matrix(numeric(0),nRuns, 1)
Target       <- vector("list",nRuns)
qQ.mlpe.err  <- vector("list", nRuns)
P1           <- vector("list",length = nRuns)
Target       <- vector("list",nRuns)

# comment if pre-fit

# qQ.mlpe      <- vector("list",length = nRuns)
# Fitting of Models ----
# for(i in 1:nRuns){  qQ.mlpe[[i]]=fit(y~.,qQ.lag, method=c("holdoutorder",2/3,seed = 12345),model=model.type,
#                     search=list(search=mparheuristic(model.type,n=5), method=c("kfoldorder",2,12345),metric=test.for.fit),
#                     feature="sabs")}

# Prediction ----
for(i in 1:nRuns){  P1[[i]]          <- predict(qQ.mlpe[[i]], qQ.lag[(length(qQ.lag$y)*2/3+1):length(qQ.lag$y),])
                    Target[[i]]      <- qQ.lag$y[(length(qQ.lag$y)*2/3+1):length(qQ.lag$y)]
                    error            <- print(mmetric(P1[[i]],Target[[i]],metric = error.test))
                    qQ.mlpe.err[[i]] <- error}

# Error Calculation ----
err           <- matrix(numeric(0),nRuns, length(error.test))
for(i in 1:nRuns){  
for(j in 1:length(error.test)){
                    err[i,j]          <- qQ.mlpe.err[[i]][[j]]}}
err.mean     <- colMeans(err)

# OUTPUT ----

# # Plot for one-step prediction ----
# predD        <- vector("list",1)
# predD$pred   <- P1
# h            <- holdout(qQ.lag$y, ratio = 2/3, mode = "order", seed = 12345) # just for ID later on!
# ensScatterPlot(predD,qQ.lag,h,2,paste("mlpe: Qilian Monthly, MAE",
#                                                toString(signif(err.mean[1], digits = 4)),
#                                       ", R22", toString(signif(err.mean[2], digits = 4)), 
#                                       ", RMSE",toString(signif(err.mean[3], digits = 4))),
#                                       "observation","forecast")
# 
# pred         <- vector("list",nRuns)
# test         <- vector("list",nRuns)
# L            <- vector("list",1)
# L[[1]]       <- list(pred=P1,test=Target,runs=nRuns)
# mgraph       (L,graph="REG",Grid=10,leg=c("Target","P1"),col=c("black","blue"))
# mgraph       (L,graph="RSC",Grid=10,leg=c("P1"),col=c("blue"))


# print(paste("mlpe: Qilian Monthly, MAE",
#             toString(signif(err.mean[1], digits = 4)),
#             ", R22", toString(signif(err.mean[2], digits = 4)), 
#             ", RMSE",toString(signif(err.mean[3], digits = 4))))



out <- list(P1,
            err.mean)
# Output:
# pred List of ensembles with prediction results
# err.mean Mean error over ensemble for the three error tests ('MAE','R22','RMSE')
return(out)

}