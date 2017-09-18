#' Reads a single zip file with financial data for a trimester
#'
#' @param my.zip.file Full path to zip file
#' @param folder.to.unzip Folder to unzip files (default = tempdir())
#'
#' @return A list with several dataframes containing financial statements
#' @export
#'
#' @examples
#' # no example
gdfpd.read.zip.file <- function(my.zip.file, folder.to.unzip = tempdir()) {
  
  # sanity check
  
  if (tools::file_ext(my.zip.file) != 'zip') {
    stop(paste('File', my.zip.file, ' is not a zip file.') )
  }
  
  if (!file.exists(my.zip.file)) {
    stop(paste('File', my.zip.file, ' does not exists.') )
  }
  
  if (file.size(my.zip.file) == 0){
    stop(paste('File', my.zip.file, ' has size 0!') )
  }
  
  if (!dir.exists(folder.to.unzip)) {
    cat(paste('Folder', folder.to.unzip, 'does not exist. Creating it.'))
    dir.create(folder.to.unzip)
  }
  
  my.basename <- tools::file_path_sans_ext(basename(my.zip.file))
  rnd.folder.name <- paste0(folder.to.unzip,'/',paste0('DIR-',my.basename))
  
  #unlink(rnd.folder.name)
  #dir.create(rnd.folder.name)
  
  unzip(my.zip.file, exdir = rnd.folder.name)
  company.reg.file <- paste0(rnd.folder.name,'/FormularioCadastral.xml')
  company.reg.file <- paste0(rnd.folder.name,'/FormularioDemonstracaoFinanceiraITR.xml')
  
  xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file))
  
  # get basic info
  
  company.name = xml_data$CompanhiaAberta$NomeRazaoSocialCompanhiaAberta
  company.cvm_code <- xml_data$CompanhiaAberta$CodigoCvm
  company.SeqNumber <- xml_data$CompanhiaAberta$NumeroSequencialRegistroCvm
  company.date.delivery <- xml_data$DataEntrega
  date.docs <- as.Date(xml_data$DataReferenciaDocumento, format = '%Y-%m-%d')
  
  #cat('\nReading', my.zip.file, '-', company.name, '|', as.character(date.docs))
  
  zipped.file <- paste0(rnd.folder.name, '/',list.files(rnd.folder.name, pattern = '*.itr')[1])
  unzip(zipped.file, exdir = rnd.folder.name)
  
  company.DFP.file <- paste0(rnd.folder.name,'/', 'InfoFinaDFin.xml')
  
  if (!file.exists(company.DFP.file)) {
    stop('Cant find file', company.DFP.file)
  }
  
  xml_data <- XML::xmlToList(XML::xmlParse(company.DFP.file))
  
  file.remove(company.DFP.file)
  
  # function to get individual DF
  my.fct <- function(x, type.df, info){
    
    if (type.df == 'individual') my.char = '1'
    if (type.df == 'consolidated') my.char = '2'
    
    if (x$PlanoConta$VersaoPlanoConta$CodigoTipoInformacaoFinanceira == my.char){
      
      if (info == 'Descricao') return(x$DescricaoConta1)
      if (info == 'Valor') return(x$ValorConta2)
      if (info == 'id') return(x$PlanoConta$NumeroConta)
      
    } else {
      return(NA)
    }
  }
  
  # get individual dfs
  type.df <- 'individual'
  acc.desc  <- as.character(sapply(xml_data, my.fct, type.df = type.df, info = 'Descricao'))
  acc.value <-   as.numeric(sapply(xml_data, my.fct, type.df = type.df, info = 'Valor'))
  acc.number <- as.character(sapply(xml_data, my.fct, type.df = type.df, info = 'id'))
  
  ind.df <- data.frame(acc.number,acc.desc,acc.value, date.docs, company.name)
  
  # save info
  df.assets <- na.omit(ind.df[stringr::str_sub(ind.df$acc.number,1,1) == '1', ])
  df.liabilities <- na.omit(ind.df[stringr::str_sub(ind.df$acc.number,1,1) == '2', ])
  df.dre    <- na.omit(ind.df[stringr::str_sub(ind.df$acc.number,1,1) == '3', ])
  df.cashflow <- na.omit(ind.df[stringr::str_sub(ind.df$acc.number,1,1) == '3', ])
  
  l.individual.dfs <- list(df.assets = df.assets,
                           df.liabilities = df.liabilities,
                           df.dre = df.dre)
  
  # get consolidated dfs
  type.df <- 'consolidated'
  acc.desc  <- as.character(sapply(xml_data, my.fct, type.df = type.df, info = 'Descricao'))
  acc.value <-   as.numeric(sapply(xml_data, my.fct, type.df = type.df, info = 'Valor'))
  acc.number <- as.character(sapply(xml_data, my.fct, type.df = type.df, info = 'id'))
  
  consolidated.df <- data.frame(acc.number,acc.desc,acc.value, date.docs, company.name)
  
  # save info
  df.assets <- na.omit(consolidated.df[stringr::str_sub(consolidated.df$acc.number,1,1) == '1', ])
  df.liabilities <- na.omit(consolidated.df[stringr::str_sub(consolidated.df$acc.number,1,1) == '2', ])
  df.dre    <- na.omit(consolidated.df[stringr::str_sub(consolidated.df$acc.number,1,1) == '3', ])
  
  l.consolidated.dfs <- list(df.assets = df.assets,
                             df.liabilities = df.liabilities,
                             df.dre = df.dre)
  
  my.l <- list(ind.dfs = l.individual.dfs, cons.dfs = l.consolidated.dfs)
  
  #browser()
  # my.l <- tibble::tibble(company.name = company.name,
  #                        ref.date = date.docs,
  #                        cvm.code = company.cvm_code,
  #                        assets = list(l.individual.dfs$df.assets),
  #                        liabilities = list(l.individual.dfs$df.liabilities),
  #                        income.statements = list(l.individual.dfs$df.dre))
  return(my.l)
  
}
