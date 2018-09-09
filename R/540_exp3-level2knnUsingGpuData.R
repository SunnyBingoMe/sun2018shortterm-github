# dpKnnLevel2-listing to dpKnnFinalResult, Holiday vs Workday (overall MAE error)
# dependencies: 530(sinkFile-expStarted-UTC_......-530.R.logl2ResultListing.RData)
# next: 560.
# auto source: no.

## ----
setwd("D:/hNow/Dropbox/phd/2.knn/knn-r-project")
#setwd("c:/hNow/Dropbox/knn-r-project")
source('./010_projectF.R')
Sys.setenv(TZ="UTC")

library(data.table)

dpKnnFinalResult = data.table()

# holiday recordOrder
filename = 'analysisFlag-EntireYear-Holiday.bin'
pFile = file(filename, "rb")
analysisFlag = readBin(pFile, n = file.size(filename)/4, single(), size = 4, endian = "little")
close(pFile)
recordOrderGlobalToAnalysisHoliday = c(1:length(analysisFlag))[as.logical(analysisFlag)]
# Workday recordOrder
filename = 'analysisFlag-EntireYear-Workday.bin'
pFile = file(filename, "rb")
analysisFlag = readBin(pFile, n = file.size(filename)/4, single(), size = 4, endian = "little")
close(pFile)
recordOrderGlobalToAnalysisWorkday = c(1:length(analysisFlag))[as.logical(analysisFlag)]


# E, a little buggy, accumulated/sumed error --------------------------------------------------------------------------
load('log/sinkFile-expStarted-UTC_20161015-123051-530.R.logl2ResultListing.RData')

# handle buggy
t = l2ResultListing[1,]
l2ResultListing[, err_flow1:= err_flow1 - shift(err_flow1)]
l2ResultListing[, err_flow2:= err_flow2 - shift(err_flow2)]
l2ResultListing[, err_flow4:= err_flow4 - shift(err_flow4)]
l2ResultListing[, err_flow8:= err_flow8 - shift(err_flow8)]
l2ResultListing[, err_speed1:= err_speed1 - shift(err_speed1)]
l2ResultListing[, err_speed2:= err_speed2 - shift(err_speed2)]
l2ResultListing[, err_speed4:= err_speed4 - shift(err_speed4)]
l2ResultListing[, err_speed8:= err_speed8 - shift(err_speed8)]
l2ResultListing[1,] = t

# holiday
l2ResultListingBackup = l2ResultListing
l2ResultListing = l2ResultListing[recordOrder %in% recordOrderGlobalToAnalysisHoliday, ]
t = l2ResultListing[, ('recordOrder') := NULL][, lapply(.SD, mae)]
dpKnnFinalResult = rbind(dpKnnFinalResult, cbind(t, distanceType = 'Euclidean', holidayFlagCharacter = 'H'))


# workday
l2ResultListing = l2ResultListingBackup; rm(l2ResultListingBackup)
l2ResultListing = l2ResultListing[recordOrder %in% recordOrderGlobalToAnalysisWorkday, ]
t = l2ResultListing[, ('recordOrder') := NULL][, lapply(.SD, mae)]
dpKnnFinalResult = rbind(dpKnnFinalResult, cbind(t, distanceType = 'Euclidean', holidayFlagCharacter = 'W'))


# E2d, a little buggy, accumulated/sumed error --------------------------------------------------------------------------
load('log/sinkFile-expStarted-UTC_20161015-123149-530.R.logl2ResultListing.RData')

# handle buggy
t = l2ResultListing[1,]
l2ResultListing[, err_flow1:= err_flow1 - shift(err_flow1)]
l2ResultListing[, err_flow2:= err_flow2 - shift(err_flow2)]
l2ResultListing[, err_flow4:= err_flow4 - shift(err_flow4)]
l2ResultListing[, err_flow8:= err_flow8 - shift(err_flow8)]
l2ResultListing[, err_speed1:= err_speed1 - shift(err_speed1)]
l2ResultListing[, err_speed2:= err_speed2 - shift(err_speed2)]
l2ResultListing[, err_speed4:= err_speed4 - shift(err_speed4)]
l2ResultListing[, err_speed8:= err_speed8 - shift(err_speed8)]
l2ResultListing[1,] = t

# holiday
l2ResultListingBackup = l2ResultListing
l2ResultListing = l2ResultListing[recordOrder %in% recordOrderGlobalToAnalysisHoliday, ]
t = l2ResultListing[, ('recordOrder') := NULL][, lapply(.SD, mae)]
dpKnnFinalResult = rbind(dpKnnFinalResult, cbind(t, distanceType = 'E2d', holidayFlagCharacter = 'H'))


# workday
l2ResultListing = l2ResultListingBackup; rm(l2ResultListingBackup)
l2ResultListing = l2ResultListing[recordOrder %in% recordOrderGlobalToAnalysisWorkday, ]
t = l2ResultListing[, ('recordOrder') := NULL][, lapply(.SD, mae)]
dpKnnFinalResult = rbind(dpKnnFinalResult, cbind(t, distanceType = 'E2d', holidayFlagCharacter = 'W'))


# M, a little buggy, accumulated/sumed error --------------------------------------------------------------------------
load('log/sinkFile-expStarted-UTC_20161015-123259-530.R.logl2ResultListing.RData')

# handle buggy
t = l2ResultListing[1,]
l2ResultListing[, err_flow1:= err_flow1 - shift(err_flow1)]
l2ResultListing[, err_flow2:= err_flow2 - shift(err_flow2)]
l2ResultListing[, err_flow4:= err_flow4 - shift(err_flow4)]
l2ResultListing[, err_flow8:= err_flow8 - shift(err_flow8)]
l2ResultListing[, err_speed1:= err_speed1 - shift(err_speed1)]
l2ResultListing[, err_speed2:= err_speed2 - shift(err_speed2)]
l2ResultListing[, err_speed4:= err_speed4 - shift(err_speed4)]
l2ResultListing[, err_speed8:= err_speed8 - shift(err_speed8)]
l2ResultListing[1,] = t

# holiday
l2ResultListingBackup = l2ResultListing
l2ResultListing = l2ResultListing[recordOrder %in% recordOrderGlobalToAnalysisHoliday, ]
t = l2ResultListing[, ('recordOrder') := NULL][, lapply(.SD, mae)]
dpKnnFinalResult = rbind(dpKnnFinalResult, cbind(t, distanceType = 'Mahalanobis', holidayFlagCharacter = 'H'))


# workday
l2ResultListing = l2ResultListingBackup; rm(l2ResultListingBackup)
l2ResultListing = l2ResultListing[recordOrder %in% recordOrderGlobalToAnalysisWorkday, ]
t = l2ResultListing[, ('recordOrder') := NULL][, lapply(.SD, mae)]
dpKnnFinalResult = rbind(dpKnnFinalResult, cbind(t, distanceType = 'Mahalanobis', holidayFlagCharacter = 'W'))



#### save  --------------------------------------------------------------------------
save(dpKnnFinalResult, file = 'dpKnnFinalResult.noWindow.separateHolidayAndWorkday.RData')
