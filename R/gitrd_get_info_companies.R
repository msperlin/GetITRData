#' Reads up to date information about Bovespa companies from a github file
#'
#' A csv file with information about available companies and time periods is downloaded from github and read.
#' This file is updated periodically and manually by the author.
#'
#' @return A dataframe with several information about Bovespa companies
#' @export
#'
#' @examples
#'
#' \dontrun{ # keep cran check fast
#' df.info <- gitrd.get.info.companies()
#' str(df.info)
#' }
gitrd.get.info.companies <- function() {

  # get data from github

  cat('\nReading info file from github')
  link.github <- 'https://raw.githubusercontent.com/msperlin/GetitrData_auxiliary/master/InfoBovespaCompanies.csv'

  my.cols <- readr::cols(
    id.company = readr::col_integer(),
    name.company = readr::col_character(),
    main.sector = readr::col_character(),
    sub.sector = readr::col_character(),
    segment = readr::col_character(),
    id.file = readr::col_integer(),
    dl.link = readr::col_character(),
    id.date = readr::col_date(),
    id.type = readr::col_character(),
    situation = readr::col_character()
  )


  df.info <- readr::read_csv(link.github, col_types = my.cols)

  # remove rows without id for dates or situation
  idx <- (!is.na(df.info$id.date))&(!is.na(df.info$situation))
  df.info <- df.info[idx, ]

  n.actives <- sum(unique(df.info[ ,c('name.company', 'situation')])$situation == 'ATIVO')
  n.inactives <- sum(unique(df.info[ ,c('name.company', 'situation')])$situation != 'ATIVO' )

  cat('\nFound', nrow(df.info), 'lines for', length(unique(df.info$name.company)), 'companies ',
      '[Actives = ', n.actives, ' Inactives = ', n.inactives, ']')

  my.last.update <- readLines('https://raw.githubusercontent.com/msperlin/GetitrData_auxiliary/master/LastUpdate.txt')
  cat('\nLast file update: ', my.last.update)

  return(df.info)
}
