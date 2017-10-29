#' Reads a single zip file downloaded from Bovespa
#'
#' @param my.zip.file Full path to zip file
#' @param folder.to.unzip Folder to unzip files (default = tempdir())
#' @param id.type The type of file structure ('after 2011' or 'before 2011')
#' @param type.fin.report Peridodicy of fin data ('itr' - quarterly or 'dfp' - annual)
#'
#' @return A list with several dataframes containing financial statements
#' @export
#'
#' @examples
#'
#' my.f <- system.file('extdata/434_ARAC_2008-09-30.zip', package = 'GetITRData')
#'
#' my.l <- gitrd.read.zip.file(my.f, id.type = 'before 2011', type.fin.report = 'itr')
#' print(my.l)
#'
gitrd.read.fre.zip.file <- function(my.zip.file,
                                    folder.to.unzip = tempdir(),
                                    l.other.info) {

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

  my.basename <- tools::file_path_sans_ext(basename(my.zip.file))
  rnd.folder.name <- file.path(folder.to.unzip, paste0('DIR-',my.basename))

  if (!dir.exists(rnd.folder.name)) dir.create(rnd.folder.name)

  utils::unzip(my.zip.file, exdir = rnd.folder.name, junkpaths = TRUE)

  # list files and check it
  my.files <- list.files(rnd.folder.name)

  if (length(my.files) == 0) {
    #browser()

    file.remove(my.zip.file)
    stop(paste0('Zipped file contains 0 files. ',
                'This is likelly a problem with the downloaded file. ',
                'Try running the code again as the corrupted zip file was deleted and will be downloaded again.',
                '\n\nIf the problem persists, my suggestions is to remove the time period with problem.') )
  }


  my.l <- gitrd.read.zip.file.type.fre(rnd.folder.name, folder.to.unzip)

  return(my.l)
}

#' Reads folder for zip file post 2011 (internal)
#'
#' @inheritParams gitrd.read.zip.file
#' @param rnd.folder.name Folder where unzipped files are available
#'
#' @return A list with financial statements
#'
#' @examples
#' # no example (this functions not used directly)
gitrd.read.zip.file.type.fre <- function(rnd.folder.name, folder.to.unzip = tempdir()) {

  zipped.file <- file.path(rnd.folder.name, list.files(rnd.folder.name, pattern = '*.fre')[1])

  utils::unzip(zipped.file, exdir = rnd.folder.name)

  company.reg.file <- file.path(rnd.folder.name,'ControleAcionario.xml')

  xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file))


  # get stock holders composition

  fix.fct <- function(x) {
    if (is.null(x)) x <- NA
    return(x)
  }

  my.fct <- function(l.in, l.other.info) {

    df.out <- data.frame(type.register = l.in$TipoRegistro,
                         id.stockholder = l.in$NumeroIdentificacaoAcionista,
                         id.person = fix.fct(l.in$Pessoa$IdentificacaoPessoa),
                         id.nationality = fix.fct(l.in$Nacionalidade),
                         id.state = fix.fct(l.in$Estado$NomeEstado),
                         id.country = fix.fct(l.in$Estado$Pais$NomePais),
                         name.stockholder = fix.fct(l.in$Pessoa$NomePessoa),
                         type.stockholder = fix.fct(l.in$Pessoa$TipoPessoa),
                         qtd.ord.shares = l.in$QuantidadeAcoesOrdinarias,
                         perc.ord.shares = l.in$PercentualAcoesOrdinarias,
                         qtd.pref.shares = l.in$QuantidadeAcoesPreferenciais,
                         perc.pref.shares = l.in$PercentualAcoesPreferenciais,
                         majority.stockholder = switch(l.in$AcionistaControlador,
                                                       '1' = TRUE,
                                                       '2' = FALSE,
                                                       '0' = FALSE) )


    return(df.out)
  }

  df.stockholders <- do.call(what = rbind, lapply(xml_data, my.fct))
  rownames(df.stockholders) <- NULL

  # stock composition and value of company
  company.reg.file <- file.path(rnd.folder.name,'CapitalSocial.xml')

  xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file))

  if (length(xml_data) < 3 ) {
    df.capital <- data.frame(qtd.ord.shares = NA,
                             qtd.pref.shares = NA,
                             total.value = NA)
  } else {
    effective.capital <- xml_data[[3]]

    df.capital <- data.frame(qtd.ord.shares = effective.capital$QuantidadeAcoesOrdinarias,
                             qtd.pref.shares = effective.capital$QuantidadeAcoesPreferenciais,
                             total.value = effective.capital$ValorCapitalSocial)
  }


  my.l <- list(df.stockholders = df.stockholders,
               df.capital = df.capital)

  return(my.l)
}

