#' Downloads and reads Title
#'
#' @param zip.files
#' @param folder.out
#'
#' @return
#' @export
#'
#' @examples
gdfpd.GetDFPData <- function(name.companies, 
                             first.date = Sys.Date()-6*30, 
                             last.date = Sys.Date(),
                             type.info = 'individual',
                             folder.out = tempdir(),
                             be.quiet = FALSE) {

  # sanity check
  possible.values <- c('individual', 'consolidated')
  
  if ( !(any(type.info %in% possible.values)) ){
    stop('Input type.info should be "individual" or "consolidated"')
  }
  
  # get data from github
  df.info <- gdfpd.get.info.companies()
  unique.names <- unique(df.info$name.company)

  idx <- !(name.companies %in% unique.names)
  if (any( idx)) {
    stop(paste0('Name of companies: \n\n ', paste0(name.companies[idx], collapse = '\n'), '\n\n',
                'not found in registry. Use df.info <- gdfpd.get.info.companies() to find the names of all available companies.'))
  }

  # check dates
  first.date <- as.Date(first.date)
  last.date <- as.Date(last.date)

  if ( (class(first.date) != 'Date')|(class(last.date) != 'Date') )  {
    stop('Inputs first.date or last.date does not seem to be dates. Use format YYYY-MM-DD')
  }

  idx <- (df.info$name.company %in% name.companies)&
    (df.info$id.date >= first.date)&(df.info$id.date <= last.date)

  df.to.process <- na.omit(df.info[idx, ])

  # remove duplicates 
  idx <- !duplicated(df.to.process[, c('id.company', 'id.date')])
  df.to.process <- df.to.process[idx, ]
  
  # try to find company's names
  idx <- !name.companies %in% unique(df.to.process$name.company)

  if (any(idx)) {
    warning(paste0('Cant find available dates for ', paste0(name.companies[idx], collapse = ',\t')))
  }

  if (nrow(df.to.process) == 0){
    stop('Cannot find any dates related to companies in registry..')
  }

  # msg
  cat(paste0('\n\nDownloading data for ', length(name.companies), ' companies',
             '\nType of financial statements: ', type.info,
             '\nFirst Date: ',first.date,
             '\nLaste Date: ',last.date,
             '\n\n') )
      
  cat(paste0('Starting processing stage:' ) )
  
  for (i.company in unique(df.to.process$name.company) ) {
    temp.df <- df.to.process[df.to.process$name.company == i.company, ]
    cat(paste0('\n', i.company) )
    #cat(paste0('\n\tFirst trimester:', min(temp.df$id.date)) )
    #cat(paste0('\n\tLast  trimester:', max(temp.df$id.date)) )
    cat(paste0('\n\tAvailable quarters: ', paste0(temp.df$id.date, collapse = '\t')) )
  }

  cat('\n\n')
  
  tibble.out <- tibble::tibble()

  for (i.company in unique(df.to.process$name.company)) {
    
    temp.df <- df.to.process[df.to.process$name.company == i.company,  ]

    df.assets <- data.frame()
    df.liabilities <- data.frame()
    df.income <- data.frame()
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
      

      dl.link <- paste0('http://www.rad.cvm.gov.br/enetconsulta/frmDownloadDocumento.aspx?CodigoInstituicao=2&NumeroSequencialDocumento=',
                        temp.df2$id.file)
      
      
      temp.file = file.path(folder.out, paste0(temp.df2$id.file, '_', i.date, '.zip') )
      
      
      if (file.exists(temp.file)) {
        cat('\tfile exists (no dl)')
      } else {
        cat('\tDownloading')
        download.file(url = dl.link, destfile = temp.file, quiet = T)
      }
      
      l.out <- gdfpd.read.zip.file(my.zip.file = temp.file, folder.to.unzip = tempdir())
      
      if (type.info == 'individual') {
        out.df <- l.out$ind.dfs
      }
      
      if (type.info == 'consolidated') {
        out.df <- l.out$cons.dfs
      }
      
      df.assets <- rbind(df.assets, out.df$df.assets)
      df.liabilities <- rbind(df.liabilities, out.df$df.liabilities)
      df.income <- rbind(df.income, out.df$df.dre)
      
    }
    
    tibble.company <- tibble::tibble(company.name = i.company,
                                     company.code = temp.df$id.company[1],
                                     company.sector = temp.df2$sector,
                                     min.date = min(temp.df$id.date),
                                     max.date = max(temp.df$id.date),
                                     n.quarters = length(temp.df$id.date),
                                     assets = list(df.assets),
                                     liabilities = list(df.liabilities),
                                     income = list(df.income) )

    tibble.out <- dplyr::bind_rows(tibble.out, tibble.company)

  }

  return(tibble.out)
}
