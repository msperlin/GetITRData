#' Export tibble to an excel file
#'
#' @param data.in Tibble with financial information (output of gdfpd.GetDFPData)
#' @param base.file.name The basename of excel file (don't include extension)
#' @param type.export The extension of the desired format (only 'xlsx' implemented so far)
#' @param format.data The format of output. 'wide' (default) for a wide table and 'long' for a long table
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' name.companies <- 'PETROBRAS'
#' first.date <- '2005-01-01'
#' last.date <-  '2006-01-01'
#'
#' df.statements <- gdfpd.GetDFPData(name.companies = name.companies,
#'                                   first.date = first.date,
#'                                   last.date = last.date)
#'
#' gdfpd.export.DFP.data(df.statements, format.data = 'wide')
#' }
gdfpd.export.DFP.data <- function(data.in,
                                  base.file.name = paste0('GetDFPData_Export_',Sys.Date()),
                                  type.export = 'xlsx',
                                  format.data = 'wide') {

  # check args
  possible.exports <- c('xlsx')

  if (any(!(type.export %in% type.export))) {
    stop('input type.export should be "xlsx"')
  }

  possible.formats <- c('wide', 'long')

  if (any(!(type.export %in% type.export))) {
    stop('input format.data should be "wide" or "long"')
  }

  f.out <- paste0(base.file.name,'.', type.export)

  if (file.exists(f.out)) {
    cat('File ', f.out, ' already exists. Deleting it..')
    file.remove(f.out)
  }

  for (i.company in data.in$company.name) {
    cat('\nCopying', format.data, 'data for', i.company)

    temp.df <- data.in[data.in$company.name == i.company, ]

    if (nrow(temp.df$assets[[1]]) == 0) {
      cat('\n\tFound 0 row dataframe for assets. Skipping it..')
      out.asset = data.frame(col = 'NO DATA')
    } else {
      if (format.data == 'wide') {
        out.asset = gdfpd.convert.to.wide(temp.df$assets[[1]])
      } else {
        out.asset = temp.df$assets[[1]]
      }
    }

    if (nrow(temp.df$liabilities[[1]]) == 0) {
      cat('\n\tFound 0 row dataframe for liabilities. Skipping it..')
      out.liability = data.frame(col = 'NO DATA')
    } else {
      if (format.data == 'wide') {
        out.liability = gdfpd.convert.to.wide(temp.df$liabilities[[1]])
      } else {
        out.liability = temp.df$liabilities[[1]]
      }
    }

    if (nrow(temp.df$income[[1]]) == 0) {
      cat('\n\tFound 0 row dataframe for income. Skipping it..')
      out.income = data.frame(col = 'NO DATA')
    } else {
      if (format.data == 'wide') {
        out.income = gdfpd.convert.to.wide(temp.df$income[[1]])
      } else {
        out.income = temp.df$income[[1]]
      }
    }

    xlsx::write.xlsx(x = out.asset, file = f.out,
                     sheetName = paste0('ASSETS ',temp.df$company.code,'-', stringr::str_sub(temp.df$company.name,1,5)),
                     append = T )

    xlsx::write.xlsx(x = out.liability, file = f.out,
                     sheetName = paste0('LIABIL ',temp.df$company.code,'-', stringr::str_sub(temp.df$company.name,1,5)),
                     append = T )

    xlsx::write.xlsx(x = out.income, file = f.out,
                     sheetName = paste0('INCOME ',temp.df$company.code,'-', stringr::str_sub(temp.df$company.name,1,5)),
                     append = T )
  }

  cat('\nExport sucessful')
  return('\nExport sucessful')
}


