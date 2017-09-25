#' Reads a single zip file downloaded from Bovespa
#'
#' @param my.zip.file Full path to zip file
#' @param folder.to.unzip Folder to unzip files (default = tempdir())
#' @param id.type The type of file structure ('after 2011' or 'before 2011')
#'
#' @return A list with several dataframes containing financial statements
#' @export
#'
#' @examples
#' # no example
gdfpd.read.zip.file <- function(my.zip.file, folder.to.unzip = tempdir(), id.type) {

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

  if (length(my.zip.file) != 1){
    stop('This function only works for a single zip file... check your inputs')
  }

  if (!dir.exists(folder.to.unzip)) {
    cat(paste('Folder', folder.to.unzip, 'does not exist. Creating it.'))
    dir.create(folder.to.unzip)
  }

  if (id.type == 'after 2011') {
    my.l <- gdfpd.read.zip.file.type.1(my.zip.file, folder.to.unzip =folder.to.unzip)
  }

  if (id.type == 'before 2011') {
    my.l <- gdfpd.read.zip.file.type.2(my.zip.file, folder.to.unzip = folder.to.unzip)
  }

  return(my.l)
}

#' Reads zip file post 2011 (internal)
#'
#' @inheritParams gdfpd.read.zip.file
#'
#' @return A list with financial statements
#'
#' @examples
#' # no example
gdfpd.read.zip.file.type.1 <- function(my.zip.file, folder.to.unzip = tempdir()) {

  my.basename <- tools::file_path_sans_ext(basename(my.zip.file))
  rnd.folder.name <- paste0(folder.to.unzip,'/',paste0('DIR-',my.basename))

  #unlink(rnd.folder.name)
  #dir.create(rnd.folder.name)

  utils::unzip(my.zip.file, exdir = rnd.folder.name)
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
  utils::unzip(zipped.file, exdir = rnd.folder.name)

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
      if (info == 'Valor') {

        my.value <- as.numeric(c(x$ValorConta2, x$ValorConta3,x$ValorConta4))
        my.value <- my.value[my.value != 0]
        if (length(my.value)==0) {
          my.value <- 0
        } else {
          my.value <- my.value[1]
        }

        return(my.value)
      }
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

  ind.df <- data.frame(acc.number,acc.desc,acc.value)

  # save info
  df.assets <- stats::na.omit(ind.df[stringr::str_sub(ind.df$acc.number,1,1) == '1', ])
  df.liabilities <- stats::na.omit(ind.df[stringr::str_sub(ind.df$acc.number,1,1) == '2', ])
  df.income    <- stats::na.omit(ind.df[stringr::str_sub(ind.df$acc.number,1,1) == '3', ])

  l.individual.dfs <- list(df.assets = df.assets,
                           df.liabilities = df.liabilities,
                           df.income = df.income)

  # get consolidated dfs
  type.df <- 'consolidated'
  acc.desc  <- as.character(sapply(xml_data, my.fct, type.df = type.df, info = 'Descricao'))
  acc.value <-   as.numeric(sapply(xml_data, my.fct, type.df = type.df, info = 'Valor'))
  acc.number <- as.character(sapply(xml_data, my.fct, type.df = type.df, info = 'id'))

  consolidated.df <- data.frame(acc.number,acc.desc,acc.value)

  # save info
  df.assets <- stats::na.omit(consolidated.df[stringr::str_sub(consolidated.df$acc.number,1,1) == '1', ])
  df.liabilities <- stats::na.omit(consolidated.df[stringr::str_sub(consolidated.df$acc.number,1,1) == '2', ])
  df.income    <- stats::na.omit(consolidated.df[stringr::str_sub(consolidated.df$acc.number,1,1) == '3', ])


  l.consolidated.dfs <- list(df.assets = df.assets,
                             df.liabilities = df.liabilities,
                             df.income = df.income)

  my.l <- list(ind.dfs = l.individual.dfs,
               cons.dfs = l.consolidated.dfs)

  return(my.l)
}

#' Reads zip file pre 2011 (internal)
#'
#' @inheritParams gdfpd.read.zip.file
#'
#' @return A list with financial statements
#'
#' @examples
#' # no example
gdfpd.read.zip.file.type.2 <- function(my.zip.file, folder.to.unzip = tempdir()) {

  my.basename <- tools::file_path_sans_ext(basename(my.zip.file))
  rnd.folder.name <- paste0(folder.to.unzip,'/',paste0('DIR-',my.basename))

  #unlink(rnd.folder.name)
  #dir.create(rnd.folder.name)

  utils::unzip(my.zip.file, exdir = rnd.folder.name, junkpaths = TRUE)

  # get individual fin statements

  my.f <- paste0(rnd.folder.name,'/ITRBPAE.001')
  my.f <- list.files(rnd.folder.name, pattern = 'ITRBPA', full.names = T)
  df.assets <- gdfpd.read.fwf.file(my.f)

  my.f <- paste0(rnd.folder.name,'/ITRBPPE.001')
  my.f <- list.files(rnd.folder.name, pattern = 'ITRBPP', full.names = T)
  df.liabilities <- gdfpd.read.fwf.file(my.f)

  my.f <- paste0(rnd.folder.name,'/ITRDEREE.001')
  my.f <- list.files(rnd.folder.name, pattern = 'ITRDERE', full.names = T)
  df.income <- gdfpd.read.fwf.file(my.f)

  l.individual.dfs <- list(df.assets = df.assets,
                           df.liabilities = df.liabilities,
                           df.income = df.income)


  # get consolidated fin statements

  my.f <- paste0(rnd.folder.name,'/ITRCBPAE.001')
  my.f <- list.files(rnd.folder.name, pattern = 'ITRCBPA', full.names = T)
  df.assets <- gdfpd.read.fwf.file(my.f)

  my.f <- paste0(rnd.folder.name,'/ITRCBPPE.001')
  my.f <- list.files(rnd.folder.name, pattern = 'ITRCBPP', full.names = T)
  df.liabilities <- gdfpd.read.fwf.file(my.f)

  my.f <- paste0(rnd.folder.name,'/ITRCDERE.001')
  my.f <- list.files(rnd.folder.name, pattern = 'ITRCDER', full.names = T)
  df.income <- gdfpd.read.fwf.file(my.f)

  l.consolidated.dfs<- list(df.assets = df.assets,
                           df.liabilities = df.liabilities,
                           df.income = df.income)
  # get basic info

  my.l <- list(ind.dfs = l.individual.dfs,
               cons.dfs = l.consolidated.dfs)

  return(my.l)
}
