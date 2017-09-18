#' Downloads and reads Title
#'
#' @param zip.files
#' @param folder.out
#'
#' @return
#' @export
#'
#' @examples
gdfpd.GetDFPData <- function(name.companies, first.date, last.date, folder.out = tempdir()) {

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

  idx <- !name.companies %in% unique(df.to.process$name.company)

  if (any(idx)) {
    warning(paste0('Cant find available dates for ', paste0(name.companies[idx], collapse = ',\t')))
  }

  if (nrow(df.to.process) == 0){
    stop('Cannot find any dates related to companies in registry..')
  }

  # msg
  cat('\nData to download:')
  for (i.company in unique(df.to.process$name.company) ) {
    temp.df <- df.to.process[df.to.process$name.company == i.company, ]
    cat(paste0('\n', i.company) )
    cat(paste0('\n\tFirst trimester:', min(temp.df$id.date)) )
    cat(paste0('\n\tLast  trimester:', max(temp.df$id.date)) )
  }


  tibble.out <- tibble::tibble()
  for (i.row in 1:nrow(df.to.process) ) {

    temp.df <- df.to.process[i.row, ]
    cat(paste0('\nProcessing ', temp.df$name.company, '\t', temp.df$id.date  ) )

    dl.link <- paste0('http://www.rad.cvm.gov.br/enetconsulta/frmDownloadDocumento.aspx?CodigoInstituicao=2&NumeroSequencialDocumento=',
                      temp.df$id.file)

    temp.file = file.path(folder.out, paste0(temp.df$id.file,'.zip') )


    if (file.exists(temp.file)) {
      cat('\tNo download needed (file exists)')
    } else {
      download.file(url = dl.link, destfile = temp.file, quiet = T)
    }


    l.out <- gdfpd.read.zip.file(my.zip.file = temp.file, folder.out)

    tibble.out <- dplyr::bind_rows(tibble.out, l.out)

  }

  return(tibble.out)
}
