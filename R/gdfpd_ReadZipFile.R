
gdfpd_ReadZipFile <- function(my.zip.file, folder.out = tempdir()) {

  # sanity check

  #browser()

  if (tools::file_ext(my.zip.file) != 'zip') {
    stop(paste('File', my.zip.file, ' is not a zip file.') )
  }

  if (!file.exists(my.zip.file)) {
    stop(paste('File', my.zip.file, ' does not exists.') )
  }

  if (!dir.exists(folder.out)) {
    cat(paste('Folder', folder.out, 'does not exist. Creating it.'))
    dir.create(folder.out)
  }

  my.basename <- tools::file_path_sans_ext(basename(my.zip.file))
  rnd.folder.name <- paste0(folder.out,'/',paste0('DIR-',my.basename))

  #unlink(rnd.folder.name)
  #dir.create(rnd.folder.name)

  unzip(my.zip.file, exdir = rnd.folder.name)
  company.reg.file <- paste0(rnd.folder.name,'/FormularioCadastral.xml')

  xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file))

  # get basic info

  company.name = xml_data$CompanhiaAberta$NomeRazaoSocialCompanhiaAberta
  company.cvm_code <- xml_data$CompanhiaAberta$CodigoCvm
  company.SeqNumber <- xml_data$CompanhiaAberta$NumeroSequencialRegistroCvm
  company.date.delivery <- xml_data$DataEntrega
  date.docs <- as.Date(stringr::str_sub(my.basename, start = 7, end = 14), format = '%Y%m%d')
  format.file <- stringr::str_sub(my.basename,
                                  start = nchar(my.basename)-2, end = nchar(my.basename))
  format.file.abv <- stringr::str_sub(format.file, 1,1)


  cat('\nReading', my.zip.file, '-', company.name, '|', as.character(date.docs))



  if (format.file.abv == '4') {

    zipped.file <- paste0(rnd.folder.name, '/',list.files(rnd.folder.name, pattern = '*.dfp')[1])
    unzip(zipped.file, exdir = rnd.folder.name)

    company.DFP.file <- paste0(rnd.folder.name,'/', 'InfoFinaDFin.xml')

    if (!file.exists(company.DFP.file)) {
      stop('Cant find file', company.DFP.file)
    }

    xml_data <- XML::xmlToList(XML::xmlParse(company.DFP.file))

    file.remove(company.DFP.file)

    acc.desc <- as.character(sapply(xml_data, function(x) x$DescricaoConta1))
    acc.value <- as.numeric(sapply(xml_data, function(x) x$ValorConta1))
    #x3 <- sapply(xml_data, function(x) x$ValorConta2)
    #x4 <- sapply(xml_data, function(x) x$ValorConta4)
    acc.number <- as.character(sapply(xml_data, function(x) x$PlanoConta$NumeroConta))

    df <- data.frame(acc.number,acc.desc,acc.value, date.docs, company.name)

  }

  if (format.file.abv == '2'){

    zipped.file <- paste0(rnd.folder.name, '/',list.files(rnd.folder.name, pattern = '*.fre')[1])
    unzip(zipped.file, exdir = rnd.folder.name)

    company.DFP.file <- paste0(rnd.folder.name,'/', 'InformacoesFinanceiras.xml')

    if (!file.exists(company.DFP.file)) {
      stop('Cant find file', company.DFP.file)
    }

    xml_data <- XML::xmlToList(XML::xmlParse(company.DFP.file))

    file.remove(company.DFP.file)

    #browser()


    a <- xml_data[[1]]
    #a$FomularioReferencia
    acc.desc <-as.character( sapply(xml_data, function(x) x$DescricaoConta))
    acc.value <- as.numeric(sapply(xml_data, function(x) x$ValorConta))
    #x3 <- sapply(xml_data, function(x) x$ValorConta2)
    #x4 <- sapply(xml_data, function(x) x$ValorConta4)
    acc.number <- sapply(xml_data, function(x) x$DetalhePlanoConta$PlanoConta$NumeroConta)


  }

  df <- data.frame(acc.number,acc.desc,acc.value, date.docs, company.name)


  return(df)

}
