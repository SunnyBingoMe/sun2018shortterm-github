source('./010_projectF.R')
algorithmSetup = NULL

algorithmSetup$inputFileByteSize = file.size('./t145325.clean_as.vector_single.bin')
algorithmSetup$variableNr = 2
algorithmSetup$searchStepLengthListing  = as.integer(c(2, 4, 8, 16, 32, 64, 128, 256));
algorithmSetup$windowSizeListing        = as.integer(c(0, 4, 8, 16, 32));
algorithmSetup$kLevel1Listing           = as.integer(c(2, 4, 8, 16, 32, 64, 128, 256));
algorithmSetup$predictStepLengthListing = as.integer(c(1, 2, 4, 8));
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
algorithmSetup$combinationNrPerTimePoint =
    algorithmSetup$combinationNrPerTimePointNoPrediction *
    length(algorithmSetup$predictStepLengthListing)
algorithmSetup$outputSize = algorithmSetup$resultRowNr * algorithmSetup$combinationNrPerTimePoint * 2 # 2: both rate & speed

algorithmSetup$flowUpDown_segmentNr = 2
algorithmSetup$flowIndicator_segmentNr = 10
algorithmSetup$flowSegmentNr = algorithmSetup$flowUpDown_segmentNr * algorithmSetup$flowIndicator_segmentNr

# check:
if(max(algorithmSetup$windowSizeListing) + max(algorithmSetup$searchStepLengthListing) > algorithmSetup$dailyRecordNr){
    stop("search-history left-match out of boundry.")
}

# echo:
cat('combinationNrPerTimePoint: ', algorithmSetup$combinationNrPerTimePoint, '\n')
