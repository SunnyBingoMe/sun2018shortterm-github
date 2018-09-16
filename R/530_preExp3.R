# results. gen dpKnnFinalResult (overall MAE error) from paramPredictDt, withAnalysisFlag for holiday or workday, dist types.
# dependencies: 460( !!! auto sourced !!! ).
# next: 540.R (for buggy only?)
# auto source: semi ( need to config Workday/Holiday/Both in 460.R )

## ----
#setwd("D:/hNow/Dropbox/phd/2.knn/knn-r-project")
setwd("c:/hNow/Dropbox/knn-r-project")
source('./460_postExp2-dataLoading.R')
source('./010_projectF.R')
source('./539_exp3-restEnv.R')
Sys.setenv(TZ="UTC")

library(data.table)
library(plyr)

expEnvSetup = NULL
expEnvSetup$myOwnAlgorithm = F
expEnvSetup$useWindow = analysisSetup$useWindow
expEnvSetup$globalRecordOrderToAnalysisListing = max(algorithmSetup$searchStepLengthListing):algorithmSetup$inputRecordTotalNr
expEnvSetup$globalRecordOrderStart = 1
expEnvSetup$globalRecordOrderEnd = algorithmSetup$completeDayNr * 288
expEnvSetup$globalRecordToAnalysis = expEnvSetup$globalRecordOrderStart:expEnvSetup$globalRecordOrderEnd
expEnvSetup$count = 0
expEnvSetup$countFinal = 0
expEnvSetup$rateErrSum  = as.data.table(setNames(replicate(4, 0, simplify = F), paste0('err_flow', algorithmSetup$predictStepLengthListing)))
expEnvSetup$speedErrSum = as.data.table(setNames(replicate(4, 0, simplify = F), paste0('err_speed', algorithmSetup$predictStepLengthListing)))
expEnvSetup$expStarted          = format(Sys.time(), '%Y%m%d-%H%M%S')
expEnvSetup$sinkFileOnlyName    = paste0('sinkFile-expStarted-UTC_', expEnvSetup$expStarted, '-', '530.R')
expEnvSetup$sinkFileNameString  = paste0('log/', expEnvSetup$sinkFileOnlyName, '.log')
expEnvSetup$useSinkFile         = T
expEnvSetup$saveFinalToRdata    = F

if(expEnvSetup$useSinkFile){
    print(expEnvSetup$sinkFileNameString)
    expEnvSetup$sinkFile = file(expEnvSetup$sinkFileNameString)
    sink(expEnvSetup$sinkFile, append=T); sink(expEnvSetup$sinkFile, append=T, type="message")
    print(Sys.info())
    print(paste('memory.limit(): ', memory.limit(), 'MB'))
}

addOneResult = function(oneTimePointResult){
    #debugS('in addOneResult')
    globalRecordOrderToAnalysis = oneTimePointResult$recordOrder

    # TODO: improve: multi d2 searchStepLengthLevel2
    d2 = algorithmSetup$searchStepLengthLevelTwoListing[1]

    resultOldRowNr = nrow(result)
    resultKeepRowStart = resultOldRowNr + 1 - algorithmSetup$combinationNrLevel2PerTimePointNoPrediction * (d2 + 1)
    resultKeepRowStart = max(resultKeepRowStart, 1)
    result <<- rbind(result, oneTimePointResult)[resultKeepRowStart : (resultOldRowNr + 1), ]

    #print(oneTimePointResult)
    #print(result)
}

addOneFinalResult = function(globalRecordOrderToAnalysis){
    #debugS('in addOneFinalResult')
    t = result[recordOrder < globalRecordOrderToAnalysis, ]
    if(nrow(t) == 0){
        #debugS('nrow(t) == 0')
        return(NA)
    }
    t = t[, lapply(.SD, mae), by = list(speedWeighting, k2)]
    t[, c('recordOrder') := NULL]
    tOrderedByRate  = t[order(err_flow1)]
    tOrderedBySpeed = t[order(err_speed1)] # TODO: use each predict step for its own sorting.
    rateErr = result[recordOrder == globalRecordOrderToAnalysis & speedWeighting == tOrderedByRate[1,]$speedWeighting  & k2 == tOrderedByRate[1,]$k2, ]
    rateErr = rateErr[, paste0('err_flow', algorithmSetup$predictStepLengthListing), with = FALSE]
    speedErr = result[recordOrder == globalRecordOrderToAnalysis & speedWeighting == tOrderedBySpeed[1,]$speedWeighting & k2 == tOrderedBySpeed[1,]$k2, ]
    speedErr = speedErr[, paste0('err_speed', algorithmSetup$predictStepLengthListing), with = FALSE]
    expEnvSetup$rateErrSum  <<- expEnvSetup$rateErrSum  + rateErr
    expEnvSetup$speedErrSum <<- expEnvSetup$speedErrSum + speedErr
    expEnvSetup$countFinal  <<- expEnvSetup$countFinal + 1

    l2ResultListing <<- rbind(l2ResultListing, cbind(recordOrder = globalRecordOrderToAnalysis, rateErr, speedErr))

    #debugS('ut addOneFinalResult')
}


print('analysisSetup'); print(analysisSetup)
print('expEnvSetup'); print(expEnvSetup)
print('algorithmSetup'); print(algorithmSetup)

cat('useWindow: ', expEnvSetup$useWindow, '\n')
expEnvSetup$startTime = Sys.time()
paste('Exp started:', expEnvSetup$startTime)
result = data.table()
l2ResultListing = data.table()

for(globalRecordOrderToAnalysis in expEnvSetup$globalRecordToAnalysis){
    #debugS('globalRecordOrderToAnalysis', globalRecordOrderToAnalysis)
    expEnvSetup$count = expEnvSetup$count + 1;
    #defaultResult = data.table(recordOrder = -999, speedWeighting = NA, k2 = NA, rateErr = NA, speedErr = NA)
    if(globalRecordOrderToAnalysis %% as.integer(length(expEnvSetup$globalRecordToAnalysis)/10) == 0){
        timestamp();
        finishedRatio = expEnvSetup$count/length(expEnvSetup$globalRecordToAnalysis)
        print(paste('globalRecordOrderToAnalysis:', globalRecordOrderToAnalysis, '/', algorithmSetup$inputRecordTotalNr,
                    ', finishedRatio:', finishedRatio,
                    ', used time mins:', as.numeric(Sys.time() - expEnvSetup$startTime, units = 'mins'),
                    ', expected finish time UTC:',  expectedFinishTime(expEnvSetup$startTime, finishedRatio)))
        head(result)
        ##debugS(paste('MAE:', result[analysis_level2 == 1, .(mae(rateErr), mae(speedErr))]))
    }
    if(! globalRecordOrderToAnalysis %in% expEnvSetup$globalRecordOrderToAnalysisListing){
        #debugS('expEnvSetup$globalRecordOrderToAnalysisListing')
        next
    }

    if(expEnvSetup$useWindow){
        algorithmSetup$combinationNrPerTimePointConsiderWindowSwitch = algorithmSetup$combinationNrPerTimePointNoPrediction
    }else{ # no window
        algorithmSetup$combinationNrPerTimePointConsiderWindowSwitch = algorithmSetup$combinationNrPerTimePointNoPrediction / length(algorithmSetup$windowSizeListing)
    }

    aggregateStartOrder = (globalRecordOrderToAnalysis - 1 - algorithmSetup$dd) * algorithmSetup$combinationNrPerTimePointConsiderWindowSwitch + 1
    aggregateEndOrder = (globalRecordOrderToAnalysis - 1) * algorithmSetup$combinationNrPerTimePointConsiderWindowSwitch
    if(aggregateStartOrder <= 0){
        aggregateStartOrder = 1
    }
    if(aggregateEndOrder >= nrow(paramPredictDt)){
        #debugS('aggregateEndOrder >= nrow(paramPredictDt)')
        next
    }
    #if(sum(paramPredictDt[aggregateStartOrder:aggregateEndOrder, ]$analysis) < 0.9 * (aggregateEndOrder - aggregateStartOrder + 1)){
    #    ##debugS('incident or workday/holiday is out of tolerance.')
    #    next
    #}

    queryRecordCombinationStartOrder = aggregateEndOrder + 1
    queryRecordCombinationEndOrder = queryRecordCombinationStartOrder + algorithmSetup$combinationNrPerTimePointConsiderWindowSwitch - 1
    if(paramPredictDt[queryRecordCombinationStartOrder, ]$analysis == 0){
        #debugS('paramPredictDt_queryRecordCombinationStartOrder_analysis_0')
        next
    }

    for(w in algorithmSetup$speedWeightingRange){
        weightedErrDt = paramPredictDt[aggregateStartOrder:aggregateEndOrder, lapply(.SD, mae), by = config_id]         # got {\hat{E}}_{[t]}, level-1 DP evaluation result matrix (before normalization). (logically, we got E[t-1] and p related predictions at [t] as we don't have t yet.)
        weightedErrDt = weightedErrDt[, weightedErr := (1-w)*err_flow1 + w*err_speed1*mean(err_flow1)/mean(err_speed1)] # got {E^+}_{[t]} by normalization & applying w.
        for(k2 in algorithmSetup$kLevel2Listing){
            bestConfigListing = weightedErrDt[order(weightedErr)][1:k2, ]$config_id                                     # actually, this is only for prediction measurement, not evaluation. # got \overline{{E^+}_{[t]}} by sorting {E^+}_{[t]} and applying k' (k2) to the last column; then extract only configs.
            error = paramPredictDt[queryRecordCombinationStartOrder:queryRecordCombinationEndOrder, ][config_id %in% bestConfigListing, lapply(.SD, mae)] # use the selected configs to predict [t] and calculate averaged results, then calculate \varepsilon'[t]{i_{p'}}.
            error = error[, c('config_id', 'analysis') := NULL]
            oneTimePointResult = data.table(recordOrder = globalRecordOrderToAnalysis, speedWeighting = w, k2 = k2)
            oneTimePointResult = cbind(oneTimePointResult, error)
            addOneResult(oneTimePointResult)                                                                            # finished and added one p'. addOneResult will only keep the last 1 hours results + one time point extra (cuz the globalRecordOrderToAnalysis related one is still "unknown" for DP).
        }
    }                                                                                                                   # end-for, finished all p' for one time point. due to addOneResult, only the last 1 hour results and one extra are kept.
    addOneFinalResult(globalRecordOrderToAnalysis)                                                                      # averaged previous evaluations of p' \varepsilon'[t-1 ... t-n2] and sort, use corresponding k' & w to predict (r,s)[t] and get \varepsilon'[t] (t is future logically, here is simplified programmingly).
}

newVarnameString = paste0('finalResult',
                          getHumanDistanceCamelStringByCapitalChar(analysisSetup$paramCombinationDistanceType),
                          getHumanCamelStringByHolidayWorkdayCharacter(analysisSetup$analysisFlagWorkdayOrHoliday),
                          getWindowStatusHumanCamelStringByInt(analysisSetup$useWindow),
                          'L2dd', algorithmSetup$dd
                          )
assign(newVarnameString, data.table(cbind(expEnvSetup$rateErrSum / expEnvSetup$countFinal, expEnvSetup$speedErrSum / expEnvSetup$countFinal)))
if(expEnvSetup$saveFinalToRdata){
    save(list = newVarnameString, file = paste0(expEnvSetup$sinkFileNameString, newVarnameString, '.RData'))
}
print(newVarnameString)
print(get(newVarnameString))
print(paste('Total time:', Sys.time() - expEnvSetup$startTime))

if(expEnvSetup$saveFinalToRdata){
    save(l2ResultListing, file = paste0(expEnvSetup$sinkFileNameString, 'l2ResultListing.RData'))
}

gc()

if(expEnvSetup$useSinkFile){
    sink();sink(type="message");
}
