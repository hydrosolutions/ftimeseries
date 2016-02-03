rm(list = ls()); cat("\014"); graphics.off(); # Clear Workspace


wdPath <- "~/Dropbox (hydrosolutions)/RRM_Heihe/R";
dataPath <- "~/Dropbox (hydrosolutions)/RRM_Heihe/R/data";
setwd(wdPath)


data.input   <- read.csv(file= file.path(dataPath,"stations/Qilian_runoff_00_14.csv"), header=TRUE, sep=",")
start.date   <- c(2000,01,01)
load("qQ.mlpe.fit.RData")
nRuns        <- 5 # for model fitting
model.type   <- "mlpe"    #options: "cubist" "mars" "randomForest" "ksvm"
test.for.fit <- "MAE"
error.test   <- c("MAE","R22", "RMSE")

result <- predictDischargeWithDischarge2(data.input,
                                           start.date,
                                           qQ.mlpe,
                                           nRuns,
                                           model.type,
                                           test.for.fit,
                                           error.test)
