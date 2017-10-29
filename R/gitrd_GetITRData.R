#' Downloads and reads financial reports from Bovespa
#'
#' Quarterly and annual financial reports are downloaded from B3 for a combination of companies and time period.
#' The easist way to get started with gitrd.GetITRData is looking for the official name of traded companies using function gitrd.search.company('nametolookfor').
#' Alternatively, you can use function gitrd.get.info.companies('companies') to import a dataframe with information for all available companies and time periods.
#'
#' @param name.companies Official names of companies to get financial reports (e.g. 'ELETROPAULO METROPOLITANA EL.S.PAULO S.A').
#' Names of companies can be found using function gitrd.search.company('nametolookfor') or gitrd.get.info.companies('companies')
#' @param first.date First date (YYYY-MM-DD) to get data. Character or Date. E.g. first.date = '2010-01-01'.
#' @param last.date Last date (YYYY-MM-DD) to get data. Character or Date. E.g. last.date = '2017-01-01'.
#' @param type.info Type of financial statements, 'individual' (default) or 'consolidated'. Argument can be a single value or a vector with the same
#' length as name.companies. The individual type only includes financial statements from the company itself, while consolidated statements adds information
#' about controlled companies
#' @param periodicy.fin.report The frequency of financial reports: 'annual' (default) or 'quarterly'
#' @param inflation.index Sets the inflation index to use for finding inflation adjusted values of all reports. Possible values: 'dollar' (default) or 'IPCA', the brazilian main inflation index.
#' When using 'IPCA', the base date is set as the last date found in itr/dfp dataset.
#' @param max.levels Sets the maximum number of levels of accounting items in financial reports
#' @param folder.out Folder where to download and manipulate the zip files. Default = tempdir()
#' @param be.quiet Should the function output information about progress? TRUE (default) or FALSE
#'
#' @return A tibble (dataframe with lists) object with all gathered financial statements, with each company as a row in the tibble.
#' @export
#'
#' @examples
#'
#' \dontrun{ #dontrun: keep cran check time short
#' name.companies <- 'ELETROPAULO METROPOLITANA EL.S.PAULO S.A'
#' first.date <- '2005-01-01'
#' last.date <-  '2006-01-01'
#'
#' df.statements <- gitrd.GetITRData(name.companies = name.companies,
#'                                   first.date = first.date,
#'                                   last.date = last.date)
#'  }
gitrd.GetITRData <- function(name.companies,
                             first.date = Sys.Date()-12*30,
                             last.date = Sys.Date(),
                             type.info = 'individual',
                             periodicy.fin.report = 'annual',
                             inflation.index = 'dollar',
                             max.levels = 3,
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

  possible.values <- c('annual', 'quarterly')
  if ( !(any(periodicy.fin.report %in% possible.values)) ){
    stop('Input periodicy.fin.report should be "annual" or "quarterly"')
  }

  type.fin.report <- switch(periodicy.fin.report,
                            'annual' = 'dfp',
                            'quarterly' = 'itr')

  # check input inflation.index
  possible.values <- c('IPCA', 'dollar')
  if ( !(any(inflation.index %in% possible.values)) ) {
    stop(paste0('Input inflation.index should be one of:\n' , paste0(possible.values, collapse = '\n') ) )
  }

  if (max.levels < 1) {
    stop('Input max.levels should be higher than one')
  }
  # check internet
  if (!curl::has_internet()) {
    stop('You need an active internet connection to download files from Bovespa.')
  }

  # get data from github
  df.info <- gitrd.get.info.companies(type.data = 'companies_files')
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

  # remove duplicates/NA and filter for type.data
  idx <- !duplicated(df.to.process[, c('id.company', 'id.date', 'type.fin.report')])
  df.to.process <- df.to.process[idx, ]

  idx <- !is.na(df.to.process$id.company)
  df.to.process <- df.to.process[idx, ]

  idx <- !is.na(df.to.process$name.company)
  df.to.process <- df.to.process[idx, ]

  if (nrow(df.to.process) == 0){
    stop('Cannot find any dates related to companies in registry. You should try different dates and companies.')
  }

  # msg to prompt
  if (length(unique(type.info))==1){
    msg.reach <- type.info[1]
  } else {
    # find most frequent
    tbl <- sort(table(type.info), decreasing = TRUE)
    msg.reach <- paste0('mostly ', names(tbl)[1])
  }

  cat(paste0('\n\nDownloading data for ', length(name.companies), ' companies',
             '\nType of financial reports: ', msg.reach,
             '\nPeriodicy of financial reports: ', periodicy.fin.report, ' (',type.fin.report, ' system)',
             '\nFirst Date: ',first.date,
             '\nLaste Date: ',last.date,
             '\nInflation index: ', inflation.index,
             '\n\n') )

  cat(paste0('Downloading inflation data' ))

  # download inflation data using BETS

  df.inflation <- gitrd.get.inflation.data(inflation.index)

  cat('\tDone\n\n')

  # try to find company's names
  idx <- !name.companies %in% unique(df.to.process$name.company)

  if (any(idx)) {
    cat(paste0('\nWARNING: Cant find available dates for ', paste0(name.companies[idx], collapse = ', ')))
  }

  # warn user for lack of cash flow data
  if (any(df.to.process$id.date < as.Date('2009-01-01'))) {
    cat('\nWARNING: For data before 2009, the cash flow statements are not available\n\n')
  }

  # start processing
  cat(paste0('Inputs looking good! Starting download of files:\n' ) )

  for (i.company in unique(df.to.process$name.company) ) {
    idx <- (df.to.process$name.company == i.company)&
      (df.to.process$type.fin.report == type.fin.report)
    temp.df <- df.to.process[idx, ]
    cat(paste0('\n', i.company) )

    cat(paste0('\n\tAvailable periods: ', paste0(temp.df$id.date, collapse = '\t')) )

  }

  cat('\n\n')

  tibble.out <- tibble::tibble()

  for (i.company in unique(df.to.process$name.company)) {

    idx <- (df.to.process$name.company == i.company)&
      (df.to.process$type.fin.report == type.fin.report)
    temp.df <- df.to.process[idx,  ]

    # get data from Bovespa site
    my.id <- temp.df$id.company[1]

    l.out.bov <- gitrd.get.bovespa.data(my.id)

    current.stock.holders <- l.out.bov$df.stock.holders
    current.stock.composition <- l.out.bov$df.stock.composition
    df.dividends <- l.out.bov$df.dividends
    company.segment <- l.out.bov$company.segment

    type.info.now <- type.info[which(i.company == name.companies)]
    df.assets <- data.frame()
    df.liabilities <- data.frame()
    df.income <- data.frame()
    df.cashflow <- data.frame()
    df.fre.stock.holders <- data.frame()
    df.fre.capital <- data.frame()
    for (i.date in as.character(temp.df$id.date) ) {

      temp.df2 <- temp.df[temp.df$id.date == i.date,  ]

      # cases for more than one file per quarter
      if (nrow(temp.df2)> 1) {
        # find id with highest value (most recent file)
        temp.df2 <- temp.df2[which.max(temp.df2$id.file), ]
      }

      if (!be.quiet) {
        cat(paste0('\nProcessing ', i.company, ', Date = ', i.date  ) )
      }


      # get dfp/itr data
      dl.link <- temp.df2$dl.link
      type.fin.report <- temp.df2$type.fin.report

      # fix file names for latin characters
      my.filename <- iconv(temp.df2$name.company, to = 'ASCII//TRANSLIT')
      my.filename <- stringr::str_replace_all(my.filename, stringr::fixed('?'), '_')

      temp.file = file.path(folder.out, paste0(temp.df2$id.company, '_',
                                               stringr::str_sub(my.filename,1,4), '_',
                                               i.date, '.zip') )


      cat(paste0('\n\tAcessing ', type.fin.report, ' data') )

      if (file.exists(temp.file)) {
        cat(' | file exists (no dl)')
      } else {
        cat(' | downloading file')

        utils::download.file(url = dl.link,
                             destfile = temp.file,
                             quiet = T,
                             mode = 'wb')
      }

      cat(' | reading file')

      suppressWarnings({
        l.out <- gitrd.read.zip.file(my.zip.file = temp.file, folder.to.unzip = tempdir(),
                                     id.type = temp.df2$id.type, type.fin.report)
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

      # get data from FRE

      cat(paste0('\n\tAcessing fre data') )

      idx <- (df.to.process$name.company == i.company)&
        (df.to.process$type.fin.report == 'fre')&
        (df.to.process$id.date == temp.df2$id.date)

      temp.df.fre <- df.to.process[idx,  ]

      if (nrow(temp.df.fre) == 0) {
        cat('\n\t\tNo FRE file available..')

        next()

      }

      temp.file = file.path(folder.out, paste0('FRE_', temp.df2$id.company, '_',
                                               stringr::str_sub(my.filename,1,4), '_',
                                               i.date, '.zip') )


      dl.link <- temp.df.fre$dl.link

      if (file.exists(temp.file)) {
        cat(' | file exists (no dl)')
      } else {
        cat(' | downloading file')

        utils::download.file(url = dl.link,
                             destfile = temp.file,
                             quiet = T,
                             mode = 'wb')
      }

      cat(' | reading file')

      l.out.FRE <- gitrd.read.fre.zip.file(my.zip.file = temp.file,
                                           folder.to.unzip = tempdir(),
                                           l.other.info = list(company.name = temp.df2$name.company,
                                                               ref.date = temp.df2$id.date))

      # set new cols and fix order
      old.names <- names(l.out.FRE$df.stockholders)
      l.out.FRE$df.stockholders$name.company <- temp.df2$name.company
      l.out.FRE$df.stockholders$id.date <- temp.df2$id.date

      my.cols <- c('name.company', 'id.date', old.names)
      l.out.FRE$df.stockholders <- l.out.FRE$df.stockholders[ ,my.cols]

      # set new cols and fix order
      old.names <- names(l.out.FRE$df.capital)
      l.out.FRE$df.capital$name.company <- temp.df2$name.company
      l.out.FRE$df.capital$id.date <- temp.df2$id.date

      my.cols <- c('name.company', 'id.date', old.names)
      l.out.FRE$df.capital <- l.out.FRE$df.capital[ ,my.cols]

      df.fre.stock.holders <- rbind(df.fre.stock.holders, l.out.FRE$df.stockholders)
      df.fre.capital <- rbind(df.fre.capital, l.out.FRE$df.capital)
    }

    # clean up dataframes before saving
    df.assets <-      gitrd.fix.dataframes(stats::na.omit(df.assets), inflation.index, df.inflation,max.levels)
    df.liabilities <- gitrd.fix.dataframes(stats::na.omit(df.liabilities), inflation.index, df.inflation,max.levels)
    df.income <-      gitrd.fix.dataframes(stats::na.omit(df.income), inflation.index, df.inflation, max.levels)
    df.cashflow <-    gitrd.fix.dataframes(stats::na.omit(df.cashflow), inflation.index, df.inflation, max.levels)

    tibble.company <- tibble::tibble(company.name = i.company,
                                     company.code = temp.df$id.company[1],
                                     type.info = type.info.now,
                                     min.date = min(temp.df$id.date),
                                     max.date = max(temp.df$id.date),
                                     n.periods = length(temp.df$id.date),
                                     company.segment = company.segment,
                                     current.stockholders = list(current.stock.holders),
                                     current.stock.composition = list(current.stock.composition),
                                     dividends.history = list(df.dividends),
                                     fr.assets = list(df.assets),
                                     fr.liabilities = list(df.liabilities),
                                     fr.income = list(df.income),
                                     fr.cashflow = list(df.cashflow),
                                     history.stock.holders = list(df.fre.stock.holders),
                                     history.capital = list(df.fre.capital) )

    tibble.out <- dplyr::bind_rows(tibble.out, tibble.company)

  }

  return(tibble.out)
}
