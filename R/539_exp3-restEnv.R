# those configs are pre-processing results
algorithmSetup = NULL

algorithmSetup$inputFileByteSize = file.size('./t145325.clean_as.vector_single.bin')
algorithmSetup$variableNr = 2
algorithmSetup$searchStepLengthListing  = as.integer(c(2, 4, 8, 16, 32, 64, 128, 256));
algorithmSetup$dd = 12                                                      # one hour
algorithmSetup$searchStepLengthLevelTwoListing  = as.integer(c(12));        # one hour
algorithmSetup$windowSizeListing        = as.integer(c(0, 4, 8, 16, 32));
algorithmSetup$kLevel1Listing           = as.integer(c(2, 4, 8, 16, 32, 64, 128, 256));
algorithmSetup$predictStepLengthListing = as.integer(c(1, 2, 4, 8));
algorithmSetup$kLevel2Listing           = as.integer(c(2, 4, 8, 16, 32));
algorithmSetup$speedWeightingRange      = seq(0, 1, by = 0.2)
algorithmSetup$expResultSizePerRow = length(algorithmSetup$predictStepLengthListing) * algorithmSetup$variableNr

algorithmSetup$inputRecordTotalNr = algorithmSetup$inputFileByteSize / 4 / algorithmSetup$variableNr
algorithmSetup$dailyRecordNr = 288
algorithmSetup$completeDayNr = floor(algorithmSetup$inputRecordTotalNr / algorithmSetup$dailyRecordNr)
algorithmSetup$completeExpResultRowNr = (algorithmSetup$completeDayNr - 1) * algorithmSetup$dailyRecordNr # first day is not completely analyzed
algorithmSetup$resultRowNr = algorithmSetup$completeExpResultRowNr + (max(algorithmSetup$predictStepLengthListing) - 1) * 2
algorithmSetup$combinationNrPerTimePointNoPrediction =
    length(algorithmSetup$searchStepLengthListing) *
    length(algorithmSetup$windowSizeListing) *
    length(algorithmSetup$kLevel1Listing)
algorithmSetup$combinationNrLevel2PerTimePointNoPrediction = length(algorithmSetup$kLevel2Listing) * length(algorithmSetup$speedWeightingRange)
algorithmSetup$combinationNrPerTimePoint =
    algorithmSetup$combinationNrPerTimePointNoPrediction *
    length(algorithmSetup$predictStepLengthListing)

algorithmSetup$flowUpDown_segmentNr = 2
algorithmSetup$flowIndicator_segmentNr = 10
algorithmSetup$flowSegmentNr = algorithmSetup$flowUpDown_segmentNr * algorithmSetup$flowIndicator_segmentNr

# check:
if(max(algorithmSetup$windowSizeListing) + max(algorithmSetup$searchStepLengthListing) > algorithmSetup$dailyRecordNr){
    stop("search-history left-match out of boundry.")
}

# echo:
cat('combinationNrPerTimePoint: ', algorithmSetup$combinationNrPerTimePoint, '\n')
