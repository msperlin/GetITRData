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
gitrd.get.dovespa.data <- function(my.id) {

  # get data from bovespa


  my.link <- paste0('http://bvmf.bmfbovespa.com.br/pt-br/mercados/acoes/empresas/ExecutaAcaoConsultaInfoEmp.asp?CodCVM=',
                    my.id, '&ViewDoc=0#a')
  data.out <- XML::readHTMLTable(my.link,
                                 skip.rows = 1)

  if (length(data.out) !=0 ) {

    fix.cols <- function(x) {
      x <- as.character(x)
      x <- stringr::str_replace_all(x, stringr::fixed('.'),'')
      x <- stringr::str_replace_all(x, stringr::fixed(','),'.')

      x <- as.numeric(x)
      return(x)
    }

    # Stockholders data

    idx <- sapply(data.out, FUN = function(df.in) any(colnames(df.in) == '%ON'))
    tbl.idx <- which(idx)

    df.stock.holders <- data.out[[tbl.idx]]
    names(df.stock.holders) <- c('name', 'ON.percent', 'PN.percent', 'total.percent')
    df.stock.holders$ON.percent <- fix.cols(df.stock.holders$ON.percent)
    df.stock.holders$PN.percent <- fix.cols(df.stock.holders$PN.percent)
    df.stock.holders$total.percent <- fix.cols(df.stock.holders$total.percent)

    # total stocks
    df.stock.composition <- data.out[[10]]
    names(df.stock.composition) <- c('type.stock', 'number.of.stocks')
    df.stock.composition$number.of.stocks <- fix.cols(df.stock.composition$number.of.stocks)

  } else {
    df.stock.holders <- NA
    df.stock.composition <- NA
  }

  l.out <- list(df.stock.composition = df.stock.composition, df.stock.holders = df.stock.holders)

  return(l.out)


}
