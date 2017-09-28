# GetITRData - Reading Quarterly Financial Reports from Bovespa

Financial statements of companies traded at B3 (formerly Bovespa), the Brazilian stock exchange, are available in its [website](http://www.bmfbovespa.com.br/). Acessing the data for a single company is straighforward. In the website one can find a simple interface for accessing this dataset. An example is given [here](https://www.rad.cvm.gov.br/ENETCONSULTA/frmGerenciaPaginaFRE.aspx?NumeroSequencialDocumento=67775&CodigoTipoInstituicao=2). However, gathering and organizing the data for a large scale research, with many companies and different time periods, is painful. Quarterly statements must be downloaded individually and latter aggregated. Changes in the accounting format thoughout time can make this process unreliable and prone to error.

Package `GetITRData` provides a R interface to all financial statements available in the website. It not only downloads the data but also organizes it in a tabular format. Users can simply select companies and time periods to download all available data. Several information about companies, such as sector and available quarters are also at reach. The main purpose of the package is to make it easy to access quarterly financial statements in large scale research, facilitating the reproducibility of such studies.

# Installation

The package is available in CRAN (release version) and in Github (development version). You can install any of those with the following code:

```
# Release version in CRAN
install.packages('GetDFPData') # not in CRAN yet

# Development version in Github
devtools::install_github('msperlin/GetDFPData')
```

# How to use GetDFPData

See [vignette](https://CRAN.R-project.org/package=GetDFPData).

## Instalation

You can install the development version from github:

```
devtools::install_github('msperlin/GetDFPData')
``` 
    
The stable version is availabe in CRAN:

```
install.packages('GetDFPData')
``` 
