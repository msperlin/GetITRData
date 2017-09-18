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

  cat('\nReading file from github')
  link.github <- 'https://raw.githubusercontent.com/msperlin/GetDFPData_auxiliary/master/InfoBovespaCompanies.csv'

  my.cols <- readr::cols(
    id.company = readr::col_integer(),
    name.company = readr::col_character(),
    id.file = readr::col_integer(),
    id.date = readr::col_date(),
    situation = readr::col_character()
  )


  df.info <- readr::read_csv(link.github, col_types = my.cols)

  cat('\nFound', nrow(df.info), 'lines for', length(unique(df.info$name.company)), 'companies')

  my.last.update <- readLines('https://raw.githubusercontent.com/msperlin/GetDFPData_auxiliary/master/LastUpdate.txt')
  cat('\nLast update: ', my.last.update)

  return(df.info)
}
