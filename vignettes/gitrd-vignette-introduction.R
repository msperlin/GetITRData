## ---- eval=FALSE---------------------------------------------------------
#  # Release version in CRAN
#  install.packages('GetITRData') # not in CRAN yet
#  
#  # Development version in Github
#  devtools::install_github('msperlin/GetITRData')

## ------------------------------------------------------------------------
library(GetITRData)

gitrd.search.company('petrobras')


## ------------------------------------------------------------------------
df.info <- gitrd.get.info.companies()

str(df.info)

## ------------------------------------------------------------------------
name.companies <- 'PETROBRAS'
first.date <- '2005-01-01'
last.date  <- '2006-01-01'
type.statements <- 'individual'

df.reports <- gitrd.GetITRData(name.companies = name.companies, 
                               first.date = first.date,
                               last.date = last.date,
                               type.info = type.statements)

## ------------------------------------------------------------------------
str(df.reports)

## ------------------------------------------------------------------------
df.income.long <- df.reports$income[[1]]

str(df.income.long)

## ------------------------------------------------------------------------
df.income.wide <- gitrd.convert.to.wide(df.income.long)

knitr::kable(df.income.wide )

## ------------------------------------------------------------------------
set.seed(5)
my.companies <- sample(unique(df.info$name.company), 5)

first.date <- '2005-01-01'
last.date  <- '2006-01-01'
type.statements <- 'individual'

df.reports <- gitrd.GetITRData(name.companies = my.companies, 
                                  first.date = first.date,
                                  last.date = last.date,
                                  type.info = type.statements)

## ------------------------------------------------------------------------
head(df.reports )

## ------------------------------------------------------------------------
df.assets <- do.call(what = rbind, args = df.reports$assets)
df.liabilities <- do.call(what = rbind, args = df.reports$liabilities)

df.assets.liabilities <- rbind(df.assets, df.liabilities)

## ------------------------------------------------------------------------
library(dplyr)

my.tab <- df.assets.liabilities %>%
  group_by(company.name, ref.date) %>%
  summarise(Liq.Index = acc.value[acc.number == '1.01']/ acc.value[acc.number == '2.01'])

my.tab

## ------------------------------------------------------------------------
library(ggplot2)

p <- ggplot(my.tab, aes(x = ref.date, y = Liq.Index, fill = company.name)) +
  geom_col(position = 'dodge' )
print(p)

## ---- eval=FALSE---------------------------------------------------------
#  my.basename <- 'MyExcelData'
#  my.format <- 'xlsx' # only supported so far
#  gitrd.export.ITR.data(data.in = df.reports,
#                        base.file.name = my.basename,
#                        type.export = my.format,
#                        format.data = 'long')

