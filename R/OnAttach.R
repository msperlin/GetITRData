.onAttach <- function(libname, pkgname) {

  citation.apa <- 'Perlin, M., Ramos, H. (2016). GetDFPData'
  citation.bibtex <- 'BIBCITHERE'
  
  my.message <- paste('Thank you for using GetDFP',
                      '\n\nAPA:\n',citation.apa,
                      '\n\nbibtex:\n',citation.bibtex )
  packageStartupMessage(my.message)
}
