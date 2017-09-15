#' Title
#'
#' @param zip.files
#' @param folder.out
#'
#' @return
#' @export
#'
#' @examples
gdfpd_ReadDFP <- function(zip.files, folder.out = tempdir()) {

  n.files <- length(zip.files)

  df.DFP <- data.frame()
  for (i.file in zip.files) {



    df.temp <- gdfpd_ReadZipFile(i.file, folder.out)

    df.DFP <- dplyr::bind_rows(df.DFP, df.temp)
  }

  return(df.DFP)
}
