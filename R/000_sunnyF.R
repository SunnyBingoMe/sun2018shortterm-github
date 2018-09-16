originTime_sunny = "1970-01-01"
s = function(){rm(t, tt, t1, t2, mainDataDt, mainDt, plotData, paramPredictDt, paramPredictDt_t, trainingData_paramPredictDt, envir = .GlobalEnv);save.image()}

debugS = function(message, variableToDisplay = NULL, level = NULL, append = FALSE, position = 'NO') {
    #print(paste(Sys.time(), message, variableToDisplay));
    #return(0);

    positionIsFound = FALSE
    if(length(grep(paste('yes', position, sep='|'), expEnvSetup$appendDebugTPositionSeries)) > 1) {
        positionIsFound = TRUE
    }

    # using global variable "expEnvSetup"
    if(append & positionIsFound) {
        print(paste('append @ ', position))
        debugT <<- append(debugT, message)
        return(NA)
    }

    # using global variable "debugLevel"
    levelForThisDebug = level
    if((not.null(variableToDisplay)) & (is.null(levelForThisDebug))) {
        levelForThisDebug = 0 # show some selected variables inside functions
    }else if(is.null(levelForThisDebug)) {
        levelForThisDebug = 1 # show the function the cpu is in
    }
    if((levelForThisDebug == 2) | positionIsFound) { # if lower than 2, must have position
        print(message)
        logT <<- append(logT, list(variableToDisplay))
        if(not.null(variableToDisplay)) {
            print(variableToDisplay)
        }
    }
    # debugLevel = 2 # only r warnings & errors
}
debugSOk = function(...) {}
not.exists = function(t) {return(!exists(t))}
not.na = function(t) {return(!is.na(t))}
not.null = function(t) {return(!is.null(t))}
not.finite = function(t) {return(!is.finite(t))}
warn = function(warnObj) {
    print(paste("WARN: ", as.character(warnObj)))
    logT <<- append(logT, as.character(warnObj))
}

# .Last = function() {
#     if(expEnvSetup$nodeName == 'SOLOHP') {
#         stop("Returning on SOLOHP")
#     } else {
#         return(0)
#     }
# }

multiplot <- function(..., plotlist=NULL, file, cols=2, layout=NULL) {
    library(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }

    if (numPlots==1) {
        print(plots[[1]])

    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

dfVectorReformatForPlot = function(originDf) {
    rowNameSeriesDuplicate = originDf[,1]
    columnNameSeriesDuplicate = originDf[,2]

    rowNameSeries = sort(unique(rowNameSeriesDuplicate))
    columnNameSeries = sort(unique(columnNameSeriesDuplicate))
    nrOfRow = length(rowNameSeries)
    nrOfColumn = length(columnNameSeries)

    newMatrix = matrix(data = NA, nrow = nrOfRow, ncol = nrOfColumn)
    returnResult = NULL

    for(rowIndex in 1:nrOfRow) {
        for(columnIndex in 1:nrOfColumn) {
            t = subset(originDf, originDf[,1] == rowNameSeries[rowIndex] & originDf[,2] == columnNameSeries[columnIndex])
            if(nrow(t) > 1) {
                stop('duplicate param combination in the first two columns.')
            }
            newMatrix[rowIndex, columnIndex] = t[,3]
        }
    }
    returnResult = expand.grid(algorithmSetup$searchStepRange, algorithmSetup$predictStepRange)
    returnResult = cbind(returnResult, as.vector(newMatrix))

    return(returnResult)
}


dfVectorToMatrixListForPlot = function(originDf) {
    rowNameSeriesDuplicate = originDf[[1]]
    columnNameSeriesDuplicate = originDf[[2]]

    rowNameSeries = sort(unique(rowNameSeriesDuplicate))
    columnNameSeries = sort(unique(columnNameSeriesDuplicate))
    nrOfRow = length(rowNameSeries)
    nrOfColumn = length(columnNameSeries)

    newMatrix = matrix(data = NA, nrow = nrOfRow, ncol = nrOfColumn)
    returnResult = NULL

    for(rowIndex in 1:nrOfRow) {
        for(columnIndex in 1:nrOfColumn) {
            t = subset(originDf, originDf[[1]] == rowNameSeries[rowIndex] & originDf[[2]] == columnNameSeries[columnIndex])
            if(nrow(t) > 1) {
                print(head(t))
                stop('duplicate param combination in the first two columns.')
            }else if(nrow(t) < 1) {
                print(paste("row filter:", rowNameSeries[rowIndex], "col filter:", columnNameSeries[columnIndex]))
                warn('no param combination in the first two columns.')
                newMatrix[rowIndex, columnIndex] = NA
            }else {
                newMatrix[rowIndex, columnIndex] = t[[3]]
            }
        }
    }
    returnResult$x = rowNameSeries
    returnResult$y = columnNameSeries
    returnResult$z = newMatrix

    return(returnResult)
}

removeValueFromVector = function(originVector, valueToRemoveListing){
    return(originVector[! originVector %in% valueToRemoveListing])
}

roundTimePosixct = function(originPosixct, second = 300){
    # 300 means round to nearest 5 minute; 300s = 60s * 5min
    return(as.POSIXct(round(as.numeric(originPosixct)/second) * second, origin = "1970-01-01"))
}

expectedFinishTime = function(startPosixct, finishedRatio){
    nowPosixct = Sys.time()
    totalTimeSec = as.numeric(nowPosixct - startPosixct, units = "secs") / finishedRatio
    return(startPosixct + totalTimeSec)
}

# Function that returns Root Mean Squared Error
rmse = function(error) {
    sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae = function(error) {
    mean(abs(error), na.rm = TRUE)
}

createCluster = function(nrCores, logfile = "rSnowLog.log", export = NULL, lib = NULL) {
    require(snow)
    require(doSNOW)
    cl <- makeCluster(nrCores, type = "SOCK", outfile = logfile)
    if(!is.null(export)) clusterExport(cl, export)
    if(!is.null(lib)) {
        l_ply(lib, function(dum) {
            clusterExport(cl, "dum", envir = environment())
            clusterEvalQ(cl, library(dum, character.only = TRUE))
        })
    }
    registerDoSNOW(cl)
    return(cl)
}

correctTimeFormat = function(timePoint) {
    if(class(timePoint)[1] == "numeric"){
        as.POSIXct(timePoint, origin = "1970-01-01")
    }else{
        timePoint
    }
}

timeToFilenameString = function(timePoint) {
    timePoint = correctTimeFormat(timePoint)
    strftime(timePoint, format = '%Y_%m_%d-%H_%M_%S')
}

timeToFilenameYMDString = function(timePoint) {
    timePoint = correctTimeFormat(timePoint)
    strftime(timePoint, format = '%Y_%m_%d')
}
timeToFilenameHMSString = function(timePoint) {
    timePoint = correctTimeFormat(timePoint)
    strftime(timePoint, format = '%H_%M_%S')
}
timeToFilenameHString = function(timePoint) {
    timePoint = correctTimeFormat(timePoint)
    strftime(timePoint, format = '%H')
}

stepFunction = function(t) {
    t = as.numeric(t)
    (as.integer(t > 0) -0.5) * 2
}

library(R.utils)
stringToCamelVariable = function(t){
    toCamelCase(tolower(t), split="[_ \t]+")
}
stringToCamelVariableCapitalStart = function(t){
    toCamelCase(tolower(t), capitalize = TRUE, split="[_ \t]+")
}
stringToCamelTitle = function(t){
    t = gsub("_", " ", t)
    t = gsub("\t", " ", t)
    t = gsub('\\s+', ' ', t, perl=TRUE)
    gsub("\\b([[:lower:]])([[:lower:]]+)", "\\U\\1\\E\\2", t, perl=TRUE)
}

qm = function(...) {
    # Get the arguments as a list
    arg <- eval(substitute(alist(...)))
    # Initialize l as a list of vecors, each vector in l corresponds to one row
    # of the matrix.
    l <- list(c())
    # rhl_l is a list where we will push the rhs of expressions like 1 | 2 | 3 ,
    # which parses as (1 | 2) | 3 , while we deal with the left hand side (1 |
    # 2)
    rhl_l <- list()
    while (length(arg) > 0) {
        a <- arg[[1]]
        arg <- tail(arg, -1)
        if (length(a) > 1 && a[[1]] == "|") {
            # Push the left hand side of the ... | ... expression back on the arguments
            # list and push the rhs onto rhl_l
            arg <- c(a[[2]], arg)
            rhl_l <- c(a[[3]], rhl_l)
        } else {
            # Just a normal element, that we'll evaluate and append to the last
            # vector/row in l.
            l[[length(l)]] <- c(l[[length(l)]], eval(a))
            # If there are rhs elements left in rhs_l we'll append them as new
            # vectors/rows on l and then we empty rhs_l.
            for (i in seq_along(rhl_l)) {
                l[[length(l) + 1]] <- eval(rhl_l[[i]])
            }
            rhl_l <- list()
        }
    }
    do.call(rbind, l)
}

colmeansTransposeToDf = function(input) {
    return(data.frame(t(colMeans(input))))
}

shapeManual = c(0, 2:4, 15, 17, 5,6, 26, 35, 42, 23, 18, 1, 16) # all shapes: bit.ly/2csOYTI >> scale_shape_identity # the 1st & 16th circle can be seen in plot's pdf, but disappears after latex. 
colorblindManual = c("#9ad0f3", "#0072B2", "#D55E00", "#CC79A7", "#F0E442", "#000000", "#009E73", "#e79f00")
colorblindBlueManual = c('#023858', '#045a8d', '#0570b0', '#3690c0', '#74a9cf', '#a6bddb', '#d0d1e6', '#ece7f2', '#fff7fb')

unlib <- function(pkg, character.only = FALSE)
{
    if(!character.only)
    {
        pkg <- deparse(substitute(pkg))
    }
    search_item <- paste("package", pkg, sep = ":")
    while(search_item %in% search())
    {
        detach(search_item, unload = TRUE, character.only = TRUE)
    }
}

ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
        fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {
        capture.output(format(utils::object.size(x), units = "auto")) })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}

# shorthand
lsos <- function(..., n=10) {
    cat(sprintf("Top %d memory usage:\n", n))
    ls.objects(..., order.by = "Size", decreasing = TRUE, head = TRUE, n = n)
}

# lsos()

ls.by.type = function(type = 'closure'){
    inlist = ls(.GlobalEnv)
    if (type == 'function')
        type = 'closure'
    typelist = sapply(sapply(inlist,get),typeof)
    return(names(typelist[typelist == type]))
}

rm.all.objects = function() {
    rm(list=ls())
}
rm.all.functions = function() {
    rm(list = lsf.str())
}
rm.all.except.functions = function(){
    rm(list = setdiff(ls(), lsf.str()))
}

beep = function(n = 9){
    i = 0
    while(i < n) {
        alarm()
        Sys.sleep(.9)
    }
}
