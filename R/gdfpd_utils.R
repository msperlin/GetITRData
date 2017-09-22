#' Converts a dataframe from gdpfd_GetDFPData to the wide format
#'
#' @param data.in Data frame with financial information
#'
#' @return A dataframe in the wide format
#' @export
#'
#' @examples
#'
#' \dontrun{
#' name.companies <- 'PETROBRAS'
#' first.date <- '2000-01-01'
#' last.date <-  '2005-01-01'
#' df.statements <- gdfpd.GetDFPData(name.companies = name.companies,
#'                                  first.date = first.date,
#'                                  last.date = last.date)
#'
#' df.assets <- df.statements$assets[[1]]
#' df.assets.wide <- gdfpd.convert.to.wide(df.assets)
#' }
gdfpd.convert.to.wide <- function(data.in) {

  if (!any('data.frame' %in% class(data.in))) {
    stop('input data.in does not seems to be a dataframe..')
  }

  df.wide <- reshape2::dcast(data = data.in,
                   formula = acc.number + acc.desc + company.name  ~ ref.date,
                   value.var = 'acc.value'  )

  return(df.wide)

}

#' Helps users search for a company name
#'
#' @param char.to.search Character for partial matching
#'
#' @return Names of found companies
#' @export
#'
#' @examples
#'
#' \dontrun{
#' df.info <- gdfpd.search.company('GERDAU')
#' }
gdfpd.search.company <- function(char.to.search) {

  df.info <- gdfpd.get.info.companies()

  unique.names <- unique(df.info$name.company)
  char.target <- stringr::str_to_lower(unique.names)
  char.to.search <- stringr::str_to_lower(char.to.search)

  idx <- stringr::str_detect(char.target, pattern = stringr::fixed(char.to.search))

  char.out <- stats::na.omit(unique.names[idx])

  cat('\n\nFound', length(char.out), 'companies:')
  cat(paste0('\n', paste0(char.out, collapse = '\n')) )

}

#' Fix dataframe for version issues (internal)
#'
#' @param df.in A dataframe with financial statements
#'
#' @return The fixed data.frame
#'
#' @examples
#'  # no example
gdfpd.fix.dataframes <- function(df.in) {

  # fix .00 in acc.number
  df.in$acc.number <- stringr::str_replace_all(df.in$acc.number, '.00', '')

  # fix names of acc.desc using latest info
  df.in$ref.date <- as.Date(df.in$ref.date)
  max.date <- max(df.in$ref.date)

  # get reference table for substitution
  ref.table <- unique(df.in[df.in$ref.date == max.date, c('acc.number', 'acc.desc')])
  ref.table <- unique(df.in[ , c('acc.number', 'acc.desc', 'ref.date')])

  my.fct <- function(x, ref.table) {
    temp <- ref.table[ref.table$acc.number == x, ]

    idx <- which.max(temp$ref.date)
    return(temp$acc.desc[idx])
  }

  desc.to.use <- sapply(X = unique(df.in$acc.number), FUN = my.fct, ref.table = ref.table)

  # replace all
  idx <- match( df.in$acc.number, names(desc.to.use))
  df.in$acc.desc <- desc.to.use[idx]

  return(df.in)
}
