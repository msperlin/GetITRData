#' Downloads and reads Title
#'
#' @param zip.files
#' @param folder.out
#'
#' @return
#' @export
#'
#' @examples
gdfpd.convert.to.wide <- function(data.in) {
  
  if ('data.frame' in class(data.in)) {
    stop('input data.in does not seems to be a dataframe..')
  }
  
  df.wide <- reshape2::dcast(data = data.in,
                   formula = acc.number + acc.desc + company.name  ~ date.docs, 
                   value.var = 'acc.value'  )
  
  return(df.wide)
  
}

gdfpd.search.company <- function(char.to.search) {
  
  df.info <- gdfpd.get.info.companies()
  
  unique.names <- unique(df.info$name.company)
  char.target <- stringr::str_to_lower(unique.names)
  
  idx <- stringr::str_detect(char.target, pattern = char.to.search)
  
  char.out <- na.omit(unique.names[idx])
  
  cat('\nFound', length(char.out), 'companies:')
  cat(paste0('\n', paste0(char.out, collapse = '\n')) )
  
  
}