#' Retrieves EDGAR filings from SEC server
#'
#' \code{getFilings} retrieves EDGAR filings for a specific CIKs, form-type,
#' filing year and quarter of the filing.
#'
#' getFilings function takes CIKs, form type, filing year, and quarter of the
#' filing as input. It creates new directory "Edgar filings_full text" to
#' store all downloaded filings. All the filings will be stored in the
#' current working directory. Keep the same current working directory for
#' further process. According to SEC EDGAR's guidelines a user also needs to
#' declare user agent. The progress bar can be controlled with the 
#' \link[progressr]{progressr} package. See in particular 
#' \link[progressr:handlers]{handlers}
#'
#' @usage getFilings(cik.no, form.type, filing.year, quarter, downl.permit, useragent)
#'
#' @param cik.no vector of CIK number of firms in integer format. Suppress leading
#' zeroes from CIKs. Keep cik.no = 'ALL' if needs to download for all CIKs.
#'
#' @param form.type character vector containing form type to be downloaded.
#' form.type = 'ALL' if need to download all forms.
#'
#' @param filing.year vector of four digit numeric year
#'
#' @param quarter vector of one digit quarter integer number. By deault, it is kept
#' as c(1 ,2, 3, 4).
#'
#' @param downl.permit "y" or "n". The default value of downl.permit is "n". It
#' asks a user permission to download fillings. This permission helps the user
#' to decide in case if number of filings are large. Setting downl.permit = "y"
#' will not ask for user permission to download filings.
#'
#' @param useragent Should be in the form of "Your Name Contact@domain.com"
#'
#' @return Function downloads EDGAR filings and returns download status in dataframe
#' format with CIK, company name, form type, date filed, accession number, and
#' download status.
#'
#' @examples
#' \dontrun{
#' # if a progress update is desired
#' library(progressr)
#' handlers(global = TRUE)
#' 
#' output <- getFilings(cik.no = c(1000180, 38079), c('10-K','10-Q'),
#'                      2006, quarter = c(1, 2, 3), downl.permit = "n", useragent)
#'
#' ## download '10-Q' and '10-K' filings filed by the firm with
#' CIK = 1000180 in quarters 1, 2, and 3 of the year 2006. These
#' filings will be stored in the current working directory.
#'
#' output <- getFilings(cik.no = 1000180, c('10-K','10-Q'),
#'                      2006, quarter = c(1, 2, 3), downl.permit = "y", useragent)
#'}
#' @export
#' @import utils httr
#' @importFrom R.utils gunzip
#' @importFrom progressr progressor handlers

getFilings <-
  function (cik.no = "ALL",
            form.type = "ALL",
            filing.year,
            quarter = c(1, 2, 3, 4),
            downl.permit = "n",
            useragent = "")
  {
    options(warn = -1)
    if (!is.numeric(filing.year)) {
      cat("Error: Input year(s) is not numeric.")
      return()
    }
    filing.year <- filing.year[filing.year >= 1994]
    if (length(filing.year) == 0) {
      cat("Please provide filing years after 1993.")
      return()
    }
    if (useragent != "") {
      bb <- any(grepl("lonare.gunratan@gmail.com|glonare@uncc.edu|bharatspatil@gmail.com", 
                      useragent, ignore.case = T))
      if (bb == TRUE) {
        cat("Please provide a valid User Agent. \n      Visit https://www.sec.gov/os/accessing-edgar-data \n      for more information")
        return()
      }
    }
    else {
      cat("Please provide a valid User Agent. \n      Visit https://www.sec.gov/os/accessing-edgar-data \n      for more information")
      return()
    }
    DownloadSECFile <- function(link, dfile, useragent, extra = NULL) {
      tryCatch({
        r <- httr::GET(link, httr::add_headers(Connection = "keep-alive", 
                                               `User-Agent` = useragent), httr::write_disk(dfile, overwrite = TRUE))
        if (httr::status_code(r) == 200) {
          return(TRUE)
        }
        else {
          return(FALSE)
        }
      }, error = function(e) {
        return(FALSE)
      })
    }
    index.df <- data.frame()
    for (year in filing.year) {
      yr.master <- paste0(year, "master.Rda")
      filepath <- paste0("Master Indexes/", yr.master)
      if (!file.exists(filepath)) {
        getMasterIndex(year, useragent)
      }
      load(filepath)
      if ((length(form.type) == 1) && (form.type == "ALL")) {
        form.type <- unique(year.master$form.type)
      }
      if ((length(cik.no) == 1) && (cik.no == "ALL")) {
        year.master <- year.master[which(year.master$form.type %in% 
                                           form.type & year.master$quarter %in% quarter), 
        ]
      }
      else {
        year.master <- year.master[which(year.master$cik %in% 
                                           cik.no & year.master$form.type %in% form.type & 
                                           year.master$quarter %in% quarter), ]
      }
      if (nrow(year.master) > 0) {
        year.master$filing.year <- year
        index.df <- rbind(index.df, year.master)
      }
    }
    if (nrow(index.df) == 0) {
      cat("No filing information found for given CIK(s) and Form Type in the mentioned year(s)/quarter(s).\n")
      return()
    }
    index.df <- index.df[order(index.df$cik, index.df$filing.year), 
    ]
    total.files <- nrow(index.df)
    msg3 <- paste0("Total number of filings to be downloaded = ", 
                   total.files, ". Do you want to download (y/n)? ")
    if (as.character(downl.permit) == "n") {
      downl.permit <- readline(prompt = msg3)
    }
    if (as.character(downl.permit) == "y") {
      dir.create("Edgar filings_full text")
      cat("Downloading fillings. Please wait...", "\n")
      p <- progressr::progressor(along = 1:nrow(index.df))
      
      index.df$edgar.link <- as.character(index.df$edgar.link)
      accessions <- do.call(rbind.data.frame, strsplit(index.df$edgar.link, 
                                                       "\\/"))[4]
      index.df$accession.number <- gsub("\\.txt", "", accessions[, 
                                                                 1])
      row.names(index.df) <- c(1:nrow(index.df))
      index.df$status <- NA
      for (i in 1:total.files) {
        edgar.link <- paste0("https://www.sec.gov/Archives/", 
                             index.df$edgar.link[i])
        f.type <- gsub("/", "", index.df$form.type[i])
        year <- index.df$filing.year[i]
        cik <- index.df$cik[i]
        new.dir <- paste0("Edgar filings_full text/Form ", 
                          f.type)
        dir.create(new.dir)
        new.dir2 <- paste0(new.dir, "/", cik)
        dir.create(new.dir2)
        dest.filename <- paste0(new.dir2, "/", cik, "_", 
                                f.type, "_", index.df$date.filed[i], "_", index.df$accession.number[i], 
                                ".txt")
        if (file.exists(dest.filename)) {
          index.df$status[i] <- "Download success"
        }
        else {
          k = 1
          while (TRUE) {
            res <- DownloadSECFile(edgar.link, dest.filename, useragent)
            if (res) {
              if (file.info(dest.filename)$size < 6000) {
                aa <- readLines(dest.filename)
                if (any(grepl("For security purposes, and to ensure that the service remains available to users, this government computer system", 
                              aa)) == FALSE) {
                  index.df$status[i] <- "Download success"
                  break
                }
              }
              else {
                index.df$status[i] <- "Download success"
                Sys.sleep(3)
                break
              }
            }
            if (k == 16) {
              index.df$status[i] <- "Download Error"
              break
            }
            k = k + 1
            Sys.sleep(6)
          }
        }
        if (i %% 10 == 0) {p()}
      }
      index.df$edgar.link <- NULL
      return(index.df)
    }
  }