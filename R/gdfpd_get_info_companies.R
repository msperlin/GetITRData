#' Reads up to date information about Bovespa companies from a github file
#'
#' @return A dataframe with information about Bovespa companies
#' @export
#'
#' @examples
#'
#' df.info <- gdfpd.get.info.companies()
gdfpd.get.info.companies <- function() {

  # get data from github

  cat('\nReading info file from github')
  link.github <- 'https://raw.githubusercontent.com/msperlin/GetDFPData_auxiliary/master/InfoBovespaCompanies.csv'

  my.cols <- readr::cols(
    id.company = readr::col_integer(),
    name.company = readr::col_character(),
    id.file = readr::col_integer(),
    id.date = readr::col_date(),
    situation = readr::col_character()
  )


  df.info <- readr::read_csv(link.github, col_types = my.cols)
  n.actives <- sum(unique(df.info[ ,c('name.company', 'situation')])$situation == 'ATIVO')
  n.inactives <- sum(unique(df.info[ ,c('name.company', 'situation')])$situation == 'CANCELADA')
  
  cat('\nFound', nrow(df.info), 'lines for', length(unique(df.info$name.company)), 'companies ',
      '[Actives = ', n.actives, ' Inactives = ', n.inactives, ']')

  my.last.update <- readLines('https://raw.githubusercontent.com/msperlin/GetDFPData_auxiliary/master/LastUpdate.txt')
  cat('\nLast file update: ', my.last.update)

  return(df.info)
}
