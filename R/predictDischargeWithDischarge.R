#' Predict discharge with discharge
#' 
#' Function that predicts discharge with discharge.
#' @param data.input Timeseries generated with dwmTimeseries()
#' @param start.date.input Date, list with year, month, and day. e.g. c(2000,01,01)
#' @param nRuns Ensemble size
#' @param model.type Which model to run (currently only 'mlpe')
#' @param test.for.fit Which performance criteria to apply (currently only 'MAE')
#' @param error.test Some stuff ('MAE','RMSE)
#' 

predictDischargeWithDischarge <- function(data.input,
                                          start.date.input,
                                          nRuns,
                                          model.type,
                                          test.for.fit,
                                          error.test){ 
# DISCHARGE - Read and Process Station 
qQ <- data.input # input at top
#qQ <- read.csv(file= file.path(dataPath,"stations/Qilian_runoff_00_14.csv"), header=TRUE, sep=",")
qQ <- qQ[,-1]
qQ <- as.vector(as.matrix(qQ))
qQ <- na.omit(qQ)
qQts <- dwmTimeseries(qQ,start.date.input[1],start.date.input[2],start.date.input[3],"mean"); qQ.Mo <- qQts[[3]]

qQ.Mo.rs <- rescale(qQ.Mo,to=c(-1,1))

correlation.of.data <- findCorrelation(qQ.Mo)

# # 
# if(data.input.new != 0)
#   {
# qQ.Mo.rs.lag.fit <- CasesSeries(qQ.Mo.rs[1:length(qQ.Mo.rs)],correlation.of.data)
# qQ.pred <- data.input.new  # input at top
# #qQ <- read.csv(file= file.path(dataPath,"stations/Qilian_runoff_00_14.csv"), header=TRUE, sep=",")
# qQ.pred <- qQ[,-1]
# qQ.pred <- as.vector(as.matrix(qQ))
# qQ.pred <- na.omit(qQ)
# qQts.pred <- dwmTimeseries(qQ,start.date.input[1],start.date.input[2],start.date.input[3],"mean"); qQ.Da.pred <- qQts[[1]]; qQ.We.pred <- qQts[[2]]; qQ.Mo.pred <- qQts[[3]]
# 
# qQ.Mo.rs.pred <- rescale(qQ.Mo,to=c(-1,1))
# qQ.Mo.rs.lag.pred <- CasesSeries(qQ.Mo.rs[(length(qQ.Mo.rs.pred)):length(qQ.Mo.rs)],correlation.of.data)
#  
# } else #continue with script
# {
############ when(data.input.new == NULL)
## MONTHLY Prediction ----
qQ.Mo.rs.lag.fit <- CasesSeries(qQ.Mo.rs[1:(length(qQ.Mo.rs)-max(correlation.of.data))],correlation.of.data)
qQ.Mo.rs.lag.pred <- CasesSeries(qQ.Mo.rs[(length(qQ.Mo.rs)-max(correlation.of.data)):length(qQ.Mo.rs)],correlation.of.data)
Target <- qQ.Mo.rs.lag.pred$y

# Transform vector to get result one step to the future:
qQ.Mo.rs.lag.pred2 <- qQ.Mo.rs.lag.pred
for(i in 2:length(qQ.Mo.rs.lag.pred)){qQ.Mo.rs.lag.pred[[i-1]] <- qQ.Mo.rs.lag.pred2[[i]]}
qQ.Mo.rs.lag.pred[[length(qQ.Mo.rs.lag.pred)]] <- 999
#qQ.Mo.rs.lag.pred$y <- 0
#nRuns <- 100  #input at top
#}
####### jump here!


P1=vector("list",nRuns)
Target=vector("list",nRuns)
#qQ.Mo.mlpe.err <- rep(0, nRuns)
qQ.Mo.mlpe.err.fit <- rep(0, nRuns)

for(i in 1:nRuns){
  qQ.Mo.mlpe=fit(y~.,qQ.Mo.rs.lag.fit, method=c("holdoutorder",1),model=model.type,
                 search=list(search=mparheuristic(model.type,n=20),method=c("kfoldorder",2,123),metric=test.for.fit),
                 feature="sabs")  # method=c("holdoutorder",2/3,12345)
  P1[[i]]=predict(qQ.Mo.mlpe, qQ.Mo.rs.lag.pred)
  #Target[[i]] <- qQ.Mo.rs.lag.pred$y  #-0.89
  #error <- print(mmetric(qQ.Mo.rs.lag.pred$y,P1[[i]],metric=error.test))
  #qQ.Mo.mlpe.err[i] <- error[[1]]
  error.fit <-print(mmetric(qQ.Mo.rs.lag.fit$y,qQ.Mo.mlpe@object$mlp[[1]]$fitted.values,metric = error.test))
  qQ.Mo.mlpe.err.fit[i] <- error.fit[[1]]
  
}

#Results ---- 
#for(i in 1:nRuns){Target[[i]] <- qQ.Mo.rs[(length(qQ.Mo.rs)*2/3):length(qQ.Mo.rs)]}
# L=vector("list",2); pred=vector("list",1);test=vector("list",1)
pred=P1
pred.mean <-mean(as.numeric(pred))
pred.sd   <-sd(as.numeric(pred))
# test=Target; L[[1]]=list(test=test,pred=pred,runs=nRuns)
# mgraph(L,graph="REG",Grid=10,leg=c("Target","P1"),col=c("black","blue"))
# mgraph(L,graph="RSC",Grid=10,leg=c("P1"),col=c("blue"))
# 
# ###
# qQ.Mo.mlpe.meanerr <- mean (qQ.Mo.mlpe.err)
qQ.Mo.mlpe.meanerr <- mean (qQ.Mo.mlpe.err.fit)
# P.model=vector("list",2)
# P.model$pred <-P1
# h.Mo <- holdout(qQ.Mo.rs.lag.pred$y, ratio = length(Target[[1]]), mode = "order", seed = 12345) # just for ID later on!
# ensScatterPlot(P.model,qQ.Mo.rs.lag.pred,h.Mo,2,paste(model.type, ": Qilian Monthly, MAE: ",toString(qQ.Mo.mlpe.meanerr)),"observation","forecast")

predict.date <- as.yearmon(time(qQ.Mo)[length(qQ.Mo)]+1/12)

out <- list(pred,
            predict.date,
            qQ.Mo.mlpe.meanerr)

return(out)

}