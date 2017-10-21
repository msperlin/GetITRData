# CRAN Package GetITRData - Reading Quarterly Financial Reports from Bovespa

Financial statements of companies traded at B3 (formerly Bovespa), the Brazilian stock exchange, are available in its [website](http://www.bmfbovespa.com.br/). Accessing the data for a single company is straightforwardd. In the website one can find a simple interface for accessing this dataset. An example is given [here](https://www.rad.cvm.gov.br/ENETCONSULTA/frmGerenciaPaginaFRE.aspx?NumeroSequencialDocumento=67775&CodigoTipoInstituicao=2). However, gathering and organizing the data for a large scale research, with many companies and many quarters, is painful. Quarterly reports must be downloaded or copied individually and later aggregated. Changes in the accounting format thoughout time can make this process slow, unreliable and irreproducible.

Package `GetITRData` provides a R interface to all financial statements available in the website. It not only downloads the data but also organizes it in a tabular format. Users can simply select companies and a time period to download all available data. Several information about current companies, such as sector and available quarters are also at reach. The main purpose of the package is to make it easy to access quarterly financial statements in large scale research, facilitating the reproducibility of such studies.


# Installation

The package is available in CRAN (release version) and in Github (development version). You can install any of those with the following code:

```
# Release version in CRAN
install.packages('GetDFPData') 

# Development version in Github
devtools::install_github('msperlin/GetDFPData')
```

# How to use GetDFPData

See manual and vignette in CRAN.
