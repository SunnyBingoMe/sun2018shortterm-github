# results. get paramPredictDt for each time point, with AnalysisFlag for holiday or workday, dist types.
# dependencies: gpu raw result & *corresponding* analysis flags.
# next: 530 ( auto sourcing 460 )
# auto source: semi. need to config Workday/Holiday/Both

## init ----
#setwd("D:/hNow/Dropbox/phd/2.knn/knn-r-project")
setwd("c:/hNow/Dropbox/knn-r-project")
source('./010_projectF.R')
source('./439_exp2-restEnv.R')
Sys.setenv(TZ="UTC")

library(data.table, plyr)

#### big, param combinations results
gc()

analysisSetup = NULL
analysisSetup$useWindow = 0
analysisSetup$exeCodeVersion = '8.3' # char: 5, 8.1, 8.3
analysisSetup$paramCombinationDistanceType = 'P' # char: E, 2(for E2D), M, P
analysisSetup$analysisFlagWorkdayOrHoliday = 'W' # char: Workday/Holiday/Both
analysisSetup$analysisFlagWeightGenOrTest  = 'W' # char: Weight-Gen or Predict
#analysisSetup$analysisFlagTrainRatio = 1.0

pFile = file(paste0('t145325_expResult-v', analysisSetup$exeCodeVersion, '-', analysisSetup$paramCombinationDistanceType, ".bin"), "rb")
#pFile = file('t145325_expResult-v11.1-detect-gsw-missingOnly.bin', "rb")
gc()
system.time({
t = readBin(pFile, n = algorithmSetup$completeDayNr*288*algorithmSetup$combinationNrPerTimePoint*2, single(), size = 4, endian = "little")
})
close(pFile)
gc()
length(t);

system.time({
t = matrix(t, ncol = length(algorithmSetup$predictStepLengthListing)*2, byrow = TRUE);
})
gc()
nrow(t)
# to-modify: format(t[288:292,], scientific=FALSE)

paramPredictDt_t = t
paramPredictDt_t = data.table(paramPredictDt_t)

nrow(paramPredictDt_t); head(paramPredictDt_t)

## add analysis flag
pFile = file(paste0("analysisFlag-EntireYear-", getHumanCamelStringByHolidayWorkdayCharacter(analysisSetup$analysisFlagWorkdayOrHoliday), ".bin"), "rb")
t = readBin(pFile, n = 407*288, integer(), size = 4, endian = "little")
close(pFile)
length(t);
gc()

format(t[288:292], scientific=FALSE)
length(t) #117216

analysis = t[1:(algorithmSetup$completeDayNr * 288)]
analysis = as.data.frame(analysis)

nrow(analysis); head(analysis)

t = rep(analysis$analysis, each = algorithmSetup$combinationNrPerTimePointNoPrediction)
t = data.table(t); setnames(t, 'analysis');
dim(t)

t = cbind(paramPredictDt_t, t)
dim(t)

paramPredictDt_t = t
rm(t)
gc()

## add param config columns
configDt = data.table(
    config_id = seq(1:algorithmSetup$combinationNrPerTimePointNoPrediction),
    search_step = rep(algorithmSetup$searchStepLengthListing, each = length(algorithmSetup$windowSizeListing) * length(algorithmSetup$kLevel1Listing) ) ,
    window = rep(algorithmSetup$windowSizeListing, each = length(algorithmSetup$kLevel1Listing) ),
    k = rep(algorithmSetup$kLevel1Listing, length(algorithmSetup$searchStepLengthListing) * length(algorithmSetup$windowSizeListing) )
    )

t = rep(configDt$config_id, nrow(paramPredictDt_t) / nrow(configDt))
t = data.table(t); setnames(t, 'config_id');
dim(t)

t = cbind(t, paramPredictDt_t)
dim(t)

paramPredictDt_t = t
rm(t)

## add col names
setnames(paramPredictDt_t, c('config_id', 'err_flow1', 'err_speed1', 'err_flow2', 'err_speed2', 'err_flow4', 'err_speed4', 'err_flow8', 'err_speed8', 'analysis' ))
print("it should start with 2014-01-01 00:00")
head(paramPredictDt_t)

## remove window != 0
if(!analysisSetup$useWindow){
    cat('deleting non-0 window results, nrow = ', nrow(paramPredictDt_t))
    paramPredictDt_t = paramPredictDt_t[config_id %in% configDt[window == 0, ]$config_id, ]
    cat('deleted non-0 window results, nrow = ', nrow(paramPredictDt_t))
}

## if all ok, rename to paramPredictDt
paramPredictDt = paramPredictDt_t
rm(paramPredictDt_t)
gc()

## save -----------------------------------------------------------------------
if(0){
    system.time(
        save(paramPredictDt, file = paste0(
        'paramPredictDt.noTime.withAnalysisFlag_',
        tolower(getHumanCamelStringByHolidayWorkdayCharacter(analysisSetup$analysisFlagWorkdayOrHoliday)),
        '.rdata'))
    )
    rm(paramPredictDt)
    s()
    gc()
}
