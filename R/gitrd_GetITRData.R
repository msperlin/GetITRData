#' Downloads and reads financial reports from Bovespa
#'
#' Quarterly financial reports are downloaded from Bovespa for a combination of companies and time period. The downloaded
#' zip file is read by a custom function, which outputs several information that are organized and structured by this function.
#' The easist way to get started with gitrd.GetITRData is looking for the official name of traded companies using function gitrd.search.company('nametolookfor').
#' Alternatively, you can use function gitrd.get.info.companies to import a dataframe with information for all available companies and time periods.
#'
#' @param name.companies Official names of companies to get financial reports (e.g. 'PETROBRAS'). Names of companies can be found using function gitrd.get.info.companies()
#' @param first.date First date (YYYY-MM-DD) to get data. Character or Date. E.g. first.date = '2010-01-01'.
#' @param last.date Last date (YYYY-MM-DD) to get data. Character or Date. E.g. last.date = '2017-01-01'.
#' @param type.info Type of financial statements, 'individual' (default) or 'consolidated'. Argument can be a single value or a vector with the same
#' length as name.companies. The individual type only includes financial statements from the company itself, while consolidated statements adds information
#' about controlled companies
#' @param inflation.index Set which inflation index to use for finding inflation adjusted values of all reports. Possible values: 'none' (default), 'IPCA' - main brazilian inflation index and 'dollar'.
#' When using 'IPCA', the base date as set as the last date found in the inflation dataset.
#' @param folder.out Folder where to download and manipulate the zip files. Default = tempdir()
#' @param be.quiet Should the function output information about progress? TRUE (default) or FALSE
#'
#' @return A tibble (dataframe with lists) object with all gathered financial statements, with each company as a row in the tibble.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' name.companies <- 'PETROBRAS'
#' first.date <- '2005-01-01'
#' last.date <-  '2006-01-01'
#' last.date <- Sys.Date()
#'
#' df.statements <- gitrd.GetITRData(name.companies = name.companies,
#'                                   first.date = first.date,
#'                                   last.date = last.date)
#'  }
gitrd.GetITRData <- function(name.companies,
                             first.date = Sys.Date()-6*30,
                             last.date = Sys.Date(),
                             type.info = 'individual',
                             inflation.index = 'none',
                             folder.out = tempdir(),
                             be.quiet = FALSE) {
  
  # sanity check
  possible.values <- c('individual', 'consolidated')
  if ( !(any(type.info %in% possible.values)) ){
    stop('Input type.info should be "individual" or "consolidated"')
  }
  
  if (length(type.info) == 1) {
    type.info <- rep(type.info, length(name.companies))
  }
  
  if (length(type.info) != length(name.companies)) {
    stop('Length of type.info does not match the length of name.companies')
  }
  
  if (!dir.exists(folder.out)) dir.create(folder.out)
  
  # check input inflation.index
  possible.values <- c('none', 'IPCA', 'dollar')
  if ( !(any(inflation.index %in% possible.values)) ) {
    stop(paste0('Input inflation.index should be one of:\n' , paste0(possible.values, collapse = '\n') ) )
  }  
  
  # check internet
  if (!curl::has_internet()) {
    stop('You need an internet connection to download files from Bovespa.')
  }
  
  # get data from github
  df.info <- gitrd.get.info.companies()
  unique.names <- unique(df.info$name.company)
  
  idx <- !(name.companies %in% unique.names)
  if (any( idx)) {
    stop(paste0('Name of companies: \n\n ', paste0(name.companies[idx], collapse = '\n'), '\n\n',
                'not found in registry. Use df.info <- gitrd.get.info.companies() to find the names of all available companies.'))
  }
  
  # check dates
  first.date <- as.Date(first.date)
  last.date <- as.Date(last.date)
  
  if ( (class(first.date) != 'Date')|(class(last.date) != 'Date') )  {
    stop('Inputs first.date or last.date does not seem to be dates. Use format YYYY-MM-DD')
  }
  
  if (last.date < first.date) {
    stop('Your last.date is older than first.date. Did you mix them up?')
  }
  
  # find available dates for selected companies
  idx <- (df.info$name.company %in% name.companies)&
    (df.info$id.date >= first.date)&(df.info$id.date <= last.date)
  
  df.to.process <- df.info[idx, ]
  
  # remove duplicates and NA
  idx <- !duplicated(df.to.process[, c('id.company', 'id.date')])
  df.to.process <- df.to.process[idx, ]
  
  idx <- !is.na(df.to.process$id.company)
  df.to.process <- df.to.process[idx, ]
  
  
  if (nrow(df.to.process) == 0){
    stop('Cannot find any dates related to companies in registry..')
  }
  
  # msg
  cat(paste0('\n\nDownloading data for ', length(name.companies), ' companies',
             '\nType of financial statements: ', paste0(type.info, collapse = '\t'),
             '\nFirst Date: ',first.date,
             '\nLaste Date: ',last.date,
             '\nInflation index: ', inflation.index, 
             '\n\n') )

  cat(paste0('Downloading ', inflation.index, ' data using BETS'))
  
  # download inflation data using BETS
  if (inflation.index == 'IPCA') {
    id.ipca <- '433'
    df.inflation <- BETS::BETS.get(code = id.ipca, 
                                   from = as.character(first.date), 
                                   to = as.character(last.date), data.frame = T )
  }
  
  if (inflation.index == 'dollar') {
    id.ipca <- '1'
    df.inflation <- BETS::BETS.get(code = id.ipca, 
                                   from = as.character(first.date), 
                                   to = as.character(last.date), data.frame = T )
    
  }
  
  cat('\tDone\n\n')
  
  # try to find company's names
  idx <- !name.companies %in% unique(df.to.process$name.company)
  
  if (any(idx)) {
    cat(paste0('\nWARNING: Cant find available dates for ', paste0(name.companies[idx], collapse = ', ')))
  }
  
  # warn user for lack of cash flow data
  if (any(df.to.process$id.date < as.Date('2009-01-01'))) {
    cat('\nWARNING: For quarters before 2009, the cash flow statements are not available\n\n')
  }
  
  # start processing
  cat(paste0('Starting processing stage:' ) )
  
  for (i.company in unique(df.to.process$name.company) ) {
    temp.df <- df.to.process[df.to.process$name.company == i.company, ]
    cat(paste0('\n', i.company) )
    
    cat(paste0('\n\tAvailable quarters: ', paste0(temp.df$id.date, collapse = '\t')) )
  }
  
  cat('\n\n')
  
  tibble.out <- tibble::tibble()
  
  for (i.company in unique(df.to.process$name.company)) {
    
    temp.df <- df.to.process[df.to.process$name.company == i.company,  ]
    
    #browser()
    type.info.now <- type.info[which(i.company == name.companies)]
    df.assets <- data.frame()
    df.liabilities <- data.frame()
    df.income <- data.frame()
    df.cashflow <- data.frame()
    for (i.date in as.character(temp.df$id.date) ) {
      
      temp.df2 <- temp.df[temp.df$id.date == i.date,  ]
      
      # cases for more than one file per quarter
      if (nrow(temp.df2)> 1) {
        # find id with highest value (most recent file)
        temp.df2 <- temp.df2[which.max(temp.df2$id.file), ]
      }
      
      if (!be.quiet) {
        cat(paste0('\nProcessing ', i.company, ', Quarter = ', i.date  ) )
      }
      
      
      dl.link <- temp.df2$dl.link
      
      # fix file names for latin characters
      my.filename <- iconv(temp.df2$name.company, to = 'ASCII//TRANSLIT')
      my.filename <- stringr::str_replace_all(my.filename, stringr::fixed('?'), '_')
      
      temp.file = file.path(folder.out, paste0(temp.df2$id.company, '_',
                                               stringr::str_sub(my.filename,1,4), '_',
                                               i.date, '.zip') )
      
      
      if (file.exists(temp.file)) {
        cat('\tfile exists (no dl)')
      } else {
        cat('\tDownloading')
        
        utils::download.file(url = dl.link,
                             destfile = temp.file,
                             quiet = T,
                             mode = 'wb')
      }
      
      cat(' | Reading file')
      
      suppressWarnings({ 
        l.out <- gitrd.read.zip.file(my.zip.file = temp.file, folder.to.unzip = tempdir(),
                                     id.type = temp.df2$id.type )
      })
      
      if (type.info.now == 'individual') {
        out.df <- l.out$ind.dfs
      }
      
      if (type.info.now == 'consolidated') {
        out.df <- l.out$cons.dfs
      }
      
      # set some cols for long format
      out.df$df.assets$ref.date <- as.Date(i.date)
      out.df$df.assets$company.name <- i.company
      out.df$df.liabilities$ref.date <- as.Date(i.date)
      out.df$df.liabilities$company.name <- i.company
      out.df$df.income$ref.date <- as.Date(i.date)
      out.df$df.income$company.name <- i.company
      out.df$df.cashflow$company.name <- i.company
      out.df$df.cashflow$ref.date <- as.Date(i.date)
      
      df.assets <- rbind(df.assets, out.df$df.assets)
      df.liabilities <- rbind(df.liabilities, out.df$df.liabilities)
      df.income <- rbind(df.income, out.df$df.income)
      df.cashflow <- rbind(df.cashflow, out.df$df.cashflow)
      
    }
    
    # clean up dataframes before saving
    df.assets <-      gitrd.fix.dataframes(stats::na.omit(df.assets), inflation.index, df.inflation)
    df.liabilities <- gitrd.fix.dataframes(stats::na.omit(df.liabilities), inflation.index, df.inflation)
    df.income <-      gitrd.fix.dataframes(stats::na.omit(df.income), inflation.index, df.inflation)
    df.cashflow <-    gitrd.fix.dataframes(stats::na.omit(df.cashflow), inflation.index, df.inflation)
    
    tibble.company <- tibble::tibble(company.name = i.company,
                                     company.code = temp.df$id.company[1],
                                     type.info = type.info.now,
                                     min.date = min(temp.df$id.date),
                                     max.date = max(temp.df$id.date),
                                     n.quarters = length(temp.df$id.date),
                                     assets = list(df.assets),
                                     liabilities = list(df.liabilities),
                                     income = list(df.income),
                                     df.cashflow = list(df.cashflow))
    
    tibble.out <- dplyr::bind_rows(tibble.out, tibble.company)
    
  }
  
  return(tibble.out)
}
