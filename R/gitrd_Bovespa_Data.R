#' Reads information for a company from Bovespa site
#'
#' Given a CVM code, this function scrapes information from the main page of Bovespa.
#'
#' @param my.id A CVM id
#' @return A list with several dataframes
#' @export
#'
#' @examples
#'
#' \dontrun{ # keep cran check fast
#' l.info.PETR <- gitrd.get.dovespa.data(my.id = 9512)
#' str(l.info.PETR)
#' }
gitrd.get.bovespa.data <- function(my.id) {

  # fct for cleaning numerical values
  fix.num.cols <- function(x) {
    x <- as.character(x)
    x <- stringr::str_replace_all(x, stringr::fixed('.'),'')
    x <- stringr::str_replace_all(x, stringr::fixed(','),'.')

    x <- as.numeric(x)
    return(x)
  }
  # get data from bovespa


  my.link <- paste0('http://bvmf.bmfbovespa.com.br/pt-br/mercados/acoes/empresas/ExecutaAcaoConsultaInfoEmp.asp?CodCVM=',
                    my.id, '&ViewDoc=0#a')
  data.out <- XML::readHTMLTable(my.link,
                                 skip.rows = 1)

  if (length(data.out) !=0 ) {



    # Stockholders data

    idx <- sapply(data.out, FUN = function(df.in) any(colnames(df.in) == '%ON'))
    tbl.idx <- which(idx)

    df.stock.holders <- data.out[[tbl.idx]]
    names(df.stock.holders) <- c('name', 'ON.percent', 'PN.percent', 'total.percent')
    df.stock.holders$ON.percent <- fix.num.cols(df.stock.holders$ON.percent)
    df.stock.holders$PN.percent <- fix.num.cols(df.stock.holders$PN.percent)
    df.stock.holders$total.percent <- fix.num.cols(df.stock.holders$total.percent)

    # total stocks
    idx <- sapply(data.out, FUN = function(df.in) any(colnames(df.in) == 'V1'))
    tbl.idx <- which(idx)

    df.stock.composition <- data.out[[tbl.idx]]
    names(df.stock.composition) <- c('type.stock', 'number.of.stocks')
    df.stock.composition$number.of.stocks <- fix.num.cols(df.stock.composition$number.of.stocks)

  } else {
    df.stock.holders <- NA
    df.stock.composition <- NA
  }

  # cash dividends
  my.url <- paste0('http://bvmf.bmfbovespa.com.br/cias-listadas/empresas-listadas/ResumoProventosDinheiro.aspx?codigoCvm=',
                   my.id,'&tab=3.1&idioma=pt-br')

  l.out <- XML::readHTMLTable(my.url)

  if (length(l.out) != 0) {

    df.dividends <- l.out[[1]]
    names(df.dividends) <- c('type.stock', 'date.aproval', 'value', 'unit.dividend', 'type.dividend',
                             'last.day.with.dividend','last.day.price.with.dividend', 'last.price', 'last.price.unit',
                             'dividend.by.price')

    df.dividends$type.stock <- as.character(df.dividends$type.stock)
    df.dividends$date.aproval <- as.Date(df.dividends$date.aproval, '%d/%m/%Y')
    df.dividends$value <-   fix.num.cols(df.dividends$value)
    df.dividends$unit.dividend <-   fix.num.cols(df.dividends$unit.dividend)
    df.dividends$type.dividend <-   as.character(df.dividends$type.dividend)
    df.dividends$last.day.with.dividend <-   as.Date(df.dividends$last.day.with.dividend, '%d/%m/%Y')
    df.dividends$last.day.price.with.dividend <-   as.Date(df.dividends$last.day.price.with.dividend, '%d/%m/%Y')
    df.dividends$last.price <-   fix.num.cols(df.dividends$last.price)
    df.dividends$last.price.unit <-   fix.num.cols(df.dividends$last.price.unit)
    df.dividends$dividend.by.price <-   fix.num.cols(df.dividends$dividend.by.price)

  } else {
    df.dividends <- NA
  }

  l.out <- list(df.stock.composition = df.stock.composition,
                df.stock.holders = df.stock.holders,
                df.dividends = df.dividends)

  return(l.out)


}
