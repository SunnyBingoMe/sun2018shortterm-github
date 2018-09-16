source('./000_sunnyF.R')
gc()

tt = list()

Sys.setenv(TZ="UTC")
options(max.print=200)

paramCharacterListing = c('k', 'd', 'v')
getColumnStringByParamCharacter = function(tString){
    switch (tString,
        'k' = {return('k')},
        'd' = {return('search_step')},
        'v' = {return('window')}
    )
}

getWindowStatusHumanCamelStringByInt = function(useWindow){
    if(useWindow){
        return('WithWindow')
    }else{
        return('NoWindow')
    }
}

getHumanCamelStringByHolidayWorkdayCharacter = function(tString){
    switch (tString,
            'W' = {return('Workday')},
            'H' = {return('Holiday')},
            'B' = {return('HolidayAndWorkday')}
    )
}

getHumanDistanceCamelStringByCapitalChar = function(tString){
    switch (tString,
            'E' = {return('Euclidean')},
            '2' = {return('E2d')},
            'M' = {return('Mahalanobis')}
    )
}
