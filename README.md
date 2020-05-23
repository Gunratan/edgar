# edgar 2.0.3
Tool for the U.S. SEC EDGAR Retrieval and Parsing of Corporate Filings

# Overview
In the USA, companies file different forms with the U.S. Securities and Exchange Commission (SEC) through EDGAR (Electronic Data Gathering, Analysis, and Retrieval system). The EDGAR database automated system collects all the different necessary filings and makes it publicly available. This package facilitates retrieving, storing, searching, and parsing of all the available filings on the EDGAR server. It downloads filings from SEC server in bulk with a single query. Additionally, it provides various useful functions: extracts 8-K triggering events, extract "Business (Item 1)" and "Management's Discussion and Analysis(Item 7)" sections of annual statements, searches filings for desired keywords, provides sentiment measures, parses filing header information, and provides HTML view of SEC filings. 

# Installation

You can install the released version of the edgar R package from
[CRAN](https://CRAN.R-project.org) with:

``` r
# install.packages("edgar")  # uncomment this to use the package
library("edgar")
```

The package is maintained on
[github](https://github.com/Gunratan/edgar-Rpackage).

# Implementation of the edgar package
## Download daily filing information
The `getDailyMaster` function takes date as an input parameter from a user, and downloads master index for the date from the U.S. SEC EDGAR server https://www.sec.gov/Archives/edgar/daily-index/. It strips headers and converts this daily filing information into dataframe format. Function creates new directory 'Daily Indexes' into working directory to save these downloaded daily master index files in Rda format.

``` r
output <- getDailyMaster('08/09/2016')
head(output)
#>       cik             company.name form.type date.filed
#> 1 1000045   NICHOLAS FINANCIAL INC      10-Q   20160809
#> 2 1000209 MEDALLION FINANCIAL CORP      10-Q   20160809
#> 3 1000275     ROYAL BANK OF CANADA     424B2   20160809
#> 4 1000275     ROYAL BANK OF CANADA     424B2   20160809
#> 5 1000275     ROYAL BANK OF CANADA     424B2   20160809
#> 6 1000275     ROYAL BANK OF CANADA       FWP   20160809
```

## Download quarterly filing information
The `getMasterIndex` function takes filing year as an input parameter from a user, downloads quarterly master indexes from the US SEC server https://www.sec.gov/Archives/edgar/full-index/. It then strips headers from the master index files, converts them into dataframe, and merges such quarterly dataframes into yearly dataframe, and stores them in Rda format. It has ability to download master indexes for multiple years based on the user input. This function creates a new directory `Master Indexes` into current working directory to save these Rda Master Index. Please note, for all other functions in this package need to locate the same working directory to access these Rda master index files.
``` r
getMasterIndex(2006)
load("Master Indexes\\2006master.Rda")  # Load the generated yearly filing information
head(year.master)
#>       cik           company.name form.type date.filed                                  edgar.link quarter
#> 1 1000045 NICHOLAS FINANCIAL INC      10-Q 2006-02-14 edgar/data/1000045/0001144204-06-005708.txt       1
#> 2 1000045 NICHOLAS FINANCIAL INC         4 2006-02-15 edgar/data/1000045/0001144204-06-006463.txt       1
#> 3 1000045 NICHOLAS FINANCIAL INC         4 2006-02-22 edgar/data/1000045/0001144204-06-007252.txt       1
#> 4 1000045 NICHOLAS FINANCIAL INC       5/A 2006-01-25 edgar/data/1000045/0000897069-06-000169.txt       1
#> 5 1000045 NICHOLAS FINANCIAL INC       5/A 2006-01-25 edgar/data/1000045/0000897069-06-000171.txt       1
#> 6 1000045 NICHOLAS FINANCIAL INC       5/A 2006-01-25 edgar/data/1000045/0000897069-06-000173.txt       1
```
## Search for filing information
The `getFilingInfo` function takes firm identifier (name or cik), filing year(s), quarter(s), and form type as input parameters from a user and provides filing information for the firm. The function automatically downloads master index for the input year(s) and the quarter(s) using `getMasterIndex` function if it is not already been downloaded in the current working directory. By default, information of all the form types filed in all the quarters of the input year by the firm will be provided by this function.
``` r
info <- getFilingInfo('United Technologies', c(2005, 2006), quarter = c(1,2), form.type = c('8-K','10-K')) 
#> Downloading Master Indexes from SEC server for 2005 ...
#> Master Index for quarter 1 
#> Master Index for quarter 2 
#> Master Index for quarter 3 
#> Master Index for quarter 4 
#> Searching master indexes for filing information ...

head(info)
#>      cik                  company.name form.type date.filed quarter filing.year
#> 1 101829 UNITED TECHNOLOGIES CORP  DE       10-K 2005-02-10       1        2005
#> 2 101829 UNITED TECHNOLOGIES CORP  DE        8-K 2005-01-21       1        2005
#> 3 101829 UNITED TECHNOLOGIES CORP  DE        8-K 2005-04-18       2        2005
#> 4 101829 UNITED TECHNOLOGIES CORP  DE        8-K 2005-04-20       2        2005
#> 5 101829 UNITED TECHNOLOGIES CORP  DE        8-K 2005-04-25       2        2005
#> 6 101829 UNITED TECHNOLOGIES CORP  DE        8-K 2005-05-06       2        2005
```

## Download filings
The `getFilings` function takes CIKs, form type, filing year, and quarter of the filing as input. It creates new directory `Edgar filings_full text` to store all downloaded filings. All the filings will be stored in the current working directory. Keep the same current working directory for further process.
``` r
output <- getFilings(cik.no = c(1000180, 38079), c('10-K','10-Q'), 2006, quarter = c(1, 2, 3), downl.permit = "n")
#> Total number of filings to be downloaded = 6. Do you want to download (y/n)? y
#> Downloading fillings. Please wait... 
#>   |=====================================================================================| 100%
output
#>       cik    company.name form.type date.filed quarter filing.year     accession.number           status
#> 1 1000180    SANDISK CORP      10-K 2006-03-15       1        2006 0000891618-06-000116 Download success
#> 2 1000180    SANDISK CORP      10-Q 2006-05-08       2        2006 0000891618-06-000190 Download success
#> 3 1000180    SANDISK CORP      10-Q 2006-08-10       3        2006 0000950134-06-015727 Download success
#> 4   38079 FOREST OIL CORP      10-K 2006-03-16       1        2006 0001047469-06-003499 Download success
#> 5   38079 FOREST OIL CORP      10-Q 2006-05-10       2        2006 0001104659-06-033149 Download success
#> 6   38079 FOREST OIL CORP      10-Q 2006-08-09       3        2006 0001104659-06-053129 Download success
```

## Get HTML view of filings
The `getFilingsHTML` function takes CIK(s), form type(s), filing year(s), and quarter of the filing as input. The function imports edgar filings downloaded via getFilings function; otherwise, it downloads the filings which are not already been downloaded. It then reads the downloaded filings, scraps filing text excluding exhibits, and saves the filing contents in `Edgar filings_HTML view` directory in HTML format.
``` r
output <- getFilingsHTML(cik.no = c(1000180, 38079), c('10-K','10-Q'), 2006, quarter = c(1, 2, 3))
#> Downloading fillings. Please wait... 
#>   |=========================================================================| 100%
#> Scrapping full EDGAR and converting to HTML...
#>   |================================================================================================| 100%
#> HTML filings are stored in 'Edgar filings_HTML view' directory.

head(output)
#>       cik    company.name form.type date.filed     accession.number    downld.status
#> 1 1000180    SANDISK CORP      10-K 2006-03-15 0000891618-06-000116 Download success
#> 2 1000180    SANDISK CORP      10-Q 2006-05-08 0000891618-06-000190 Download success
#> 3 1000180    SANDISK CORP      10-Q 2006-08-10 0000950134-06-015727 Download success
#> 4 1000180    SANDISK CORP      10-Q 2006-11-08 0000950134-06-020940 Download success
#> 5   38079 FOREST OIL CORP      10-K 2006-03-16 0001047469-06-003499 Download success
#> 6   38079 FOREST OIL CORP      10-Q 2006-05-10 0001104659-06-033149 Download success
```

## Extract filing header information
The `getFilingHeader` function takes CIK(s), form type(s), and year(s) as input parameters. The function first imports available downloaded filings in local woking directory `Edgar filings_full text` created by getFilings function; otherwise, it automatically downloads the filings which are not already been downloaded. It then parses all the important header information from filings. The function returns a dataframe with filing and header information.
``` r
header.df <- getFilingHeader(cik.no = c('1000180', '38079'), form.type = '10-K', filing.year = 2006) 
#> Downloading fillings. Please wait... 
#>   |================================================================================================| 100%
#> Scraping filing header information ...
#>   |================================================================================================| 100%
header.df
#>       cik    company.name form.type date.filed     accession.number period.of.report fiscal.yr.end
#> 1 1000180    SANDISK CORP      10-K 2006-03-15 0000891618-06-000116       2006-01-01          1231
#> 2   38079 FOREST OIL CORP      10-K 2006-03-16 0001047469-06-003499       2005-12-31          1231
#> 
#>   filer.no filer.company.name filer.cik  sic        irs state.of.incorp       business.street1
#> 1        1       SANDISK CORP   1000180 3572  770191793              DE      140 CASPIAN COURT
#> 2        1    FOREST OIL CORP     38079 3572  250484900              NY 707 SEVENTEENTH STREET
#> 
#>   business.street2 business.city business.state business.zip           mail.street1 mail.street2
#> 1                      SUNNYVALE             CA        94089      140 CASPIAN COURT             
#> 2       SUITE 3600        DENVER             CO        80202 707 SEVENTEENTH STREET   SUITE 3600
#> 
#>   mail.city mail.state mail.zip
#> 1 SUNNYVALE         CA    94089
#> 2    DENVER         CO    80202
```

## Search filings for input keywords
The `searchFilings` function takes search keyword vector, CIK(s), form type(s), and year(s) as input parameters. The function first imports available downloaded filings in the local woking directory `Edgar filings_full text` created by `getFilings` function; otherwise, it automatically downloads the filings which are not already been downloaded. It then reads the filings and searches for the input keywords. The function returns a dataframe with filing information and the number of keyword hits. Additionally, it saves the search information with surrounding content of search keywords in HTML format in the new directory `Keyword search results`. These HTML view of search results would help the user to analyze the search strategy and identify false positive hits.
``` r
word.list = c('derivative','hedging','currency forwards','currency futures')
output <- searchFilings(cik.no = c('1000180', '38079'), form.type = c("10-K", "10-K405","10KSB", "10KSB40"), filing.year = c(2005, 2006), word.list) 
#> Downloading fillings. Please wait... 
#>   |======================================================================================| 100%
#> Searching filings for the input words...
#>   |======================================================================================| 100%
#> Detailed search results are stored in 'Keyword search results' directory.
output
#>       cik    company.name form.type date.filed nword.hits
#> 1 1000180    SANDISK CORP      10-K 2005-03-18          1
#> 2 1000180    SANDISK CORP      10-K 2006-03-15          5
#> 3   38079 FOREST OIL CORP      10-K 2005-03-15         81
#> 4   38079 FOREST OIL CORP      10-K 2006-03-16        105
```

## Extract business description section from annual statements
The `getBusinDescr` function takes firm CIK(s) and filing year(s) as input parameters from a user and provides "Item 1" section extracted from annual statements along with filing information. The function imports annual filings (10-K statements) downloaded via `getFilings` function; otherwise, it automates the downloading process if not already been downloaded. It then reads the downloaded statements, cleans HTML tags, and parse the contents. This function automatically creates a new directory with the name `Business descriptions text` in the current working directory and saves scrapped business description sections in this directory. It considers "10-K", "10-K405", "10KSB", and "10KSB40" form types as annual statements.
``` r
output <- getBusinDescr(cik.no = c(1000180, 38079), filing.year = c(2005, 2006))
#> Downloading fillings. Please wait... 
#>   |======================================================================================| 100%
#> Extracting 'Item 1' section...
#>   |======================================================================================| 100%
#> Business descriptions are stored in 'Business descriptions text' directory.
output
#>       cik    company.name form.type date.filed     accession.number    downld.status extract.status
#> 1 1000180    SANDISK CORP      10-K 2005-03-18 0000950134-05-005462 Download success              1
#> 2 1000180    SANDISK CORP      10-K 2006-03-15 0000891618-06-000116 Download success              1
#> 3   38079 FOREST OIL CORP      10-K 2005-03-15 0001047469-05-006546 Download success              1
#> 4   38079 FOREST OIL CORP      10-K 2006-03-16 0001047469-06-003499 Download success              1
```

## Extract MD&A section from annual statements
The `getMgmtDisc` function takes firm CIK(s) and filing year(s) as input parameters from a user and provides "Item 7" section extracted from annual statements along with filing information. The function imports annual filings downloaded via `getFilings` function; otherwise, it downloads the filings which are not already been downloaded. It then reads, cleans, and parse the required section from the filings. It creates a new directory with the name `MD&A section text` in the current working directory to save scrapped "Item 7" sections in text format. It considers "10-K", "10-K405", "10KSB", and "10KSB40" form types as annual statements.
``` r
output <- getMgmtDisc(cik.no = c(1000180, 38079), filing.year = 2005)
#> Downloading fillings. Please wait... 
#>   |======================================================================================| 100%
#> Extracting 'Item 7' section...
#>   |======================================================================================| 100%
#> MD&A section texts are stored in 'MD&A section text' directory.
output
#>       cik    company.name form.type date.filed     accession.number    downld.status extract.status
#> 1 1000180    SANDISK CORP      10-K 2005-03-18 0000950134-05-005462 Download success              1
#> 2   38079 FOREST OIL CORP      10-K 2005-03-15 0001047469-05-006546 Download success              1
```

## Retrieve Form 8-K items information
The `get8KItems` function takes firm CIK(s) and filing year(s) as input parameters from a user and provides information on the Form 8-K triggering events along with the firm filing information. The function searches and imports existing downloaded 8-K filings in the current directory; otherwise it downloads them using `getFilings` function. It then reads the 8-K filings and parses them to extract events information.
``` r
output <- get8KItems(cik.no = 38079, filing.year = 2005)
#> Downloading fillings. Please wait... 
#>   |======================================================================================| 100%
#> Scraping 8-K filings...
#>   |======================================================================================| 100%
tail(output)
#>      cik    company.name form.type date.filed                                    event.info
#> 35 38079 FOREST OIL CORP       8-K 2005-10-24    Entry into a Material Definitive Agreement
#> 36 38079 FOREST OIL CORP       8-K 2005-11-10 Results of Operations and Financial Condition
#> 37 38079 FOREST OIL CORP       8-K 2005-11-10                      Regulation FD Disclosure
#> 38 38079 FOREST OIL CORP       8-K 2005-11-10             Financial Statements and Exhibits
#> 39 38079 FOREST OIL CORP       8-K 2005-12-22                      Regulation FD Disclosure
#> 40 38079 FOREST OIL CORP       8-K 2005-12-27    Entry into a Material Definitive Agreement
```

## Generate sentiment measures of SEC filings
The `getSentiment` function takes CIK(s), form type(s), and year(s) as input parameters. The function first imports available downloaded filings in the local working directory `Edgar filings_full text` created by getFilings function; otherwise, it automatically downloads the filings which are not already been downloaded. It then reads, cleans, and computes sentiment measures for these filings. The function returns a dataframe with filing information and sentiment measures.
``` r
senti.df <- getSentiment(cik.no = c('1000180', '38079'), form.type = '10-K', filing.year = 2006) 
#> Downloading fillings. Please wait... 
#>   |==========================================================================================| 100%
#> Computing sentiment measures...
#>   |==========================================================================================| 100%
senti.df
#>       cik    company.name form.type date.filed     accession.number file.size word.count
#> 1 1000180    SANDISK CORP      10-K 2006-03-15 0000891618-06-000116      1666      35843
#> 2   38079 FOREST OIL CORP      10-K 2006-03-16 0001047469-06-003499      1436      36192
#> 
#>   unique.word.count stopword.count char.count complex.word.count lm.dictionary.count
#> 1              3266           9211     227147              16058               33533
#> 2              2931           8672     228638              16380               34512
#> 
#>   lm.negative.count lm.positive.count lm.strong.modal.count lm.moderate.modal.count
#> 1              1123               240                   124                     104
#> 2               676               243                   118                     111
#> 
#>   lm.weak.modal.count lm.uncertainty.count lm.litigious.count hv.negative.count
#> 1                 320                  694                581              1986
#> 2                 204                  688                364              1987
```

Following are the definitions of the text characteristics and the sentiment measures:

`file.size` = The filing size of a complete filing on the EDGAR server in kilobyte (KB).

`word.count` = The total number of words in a filing text, excluding HTML tags and exhibits text.

`unique.word.count` = The total number of unique words in a filing text, excluding HTML tags and exhibits text.

`stopword.count` = The total number of stop words in a filing text, excluding exhibits text.

`char.count` = The total number of characters in a filing text, excluding HTML tags and exhibits text.

`complex.word.count` = The total number of complex words in the filing text. When vowels (a, e, i, o, u) occur more than three times in a word, then that word is identified as a complex word.

`lm.dictionary.count` = The number of words in the filing text that occur in the Loughran-McDonald (LM) master dictionary.

`lm.negative.count` = The number of LM financial-negative words in the document.

`lm.positive.count` = The number of LM financial-positive words in the document.

`lm.strong.modal.count` = The number of LM financial-strong modal words in the document.

`lm.moderate.modal.count` = The number of LM financial-moderate Modal words in the document.

`lm.weak.modal.count` = The number of LM financial-weak modal words in the document.

`lm.uncertainty.count` = The number of LM financial-uncertainty words in the document.

`lm.litigious.count` = The number of LM financial-litigious words in the document.

`hv.negative.count` = The number of words in the document that occur in the 'Harvard General Inquirer' Negative word list, as defined by LM.
