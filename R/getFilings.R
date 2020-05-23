#' Retrieves EDGAR filings from SEC server
#'
#' \code{getFilings} retrieves EDGAR filings for a specific CIKs, form-type,
#' filing year and quarter of the filing.
#'
#' getFilings function takes CIKs, form type, filing year, and quarter of the 
#' filing as input. It creates new directory "Edgar filings_full text" to 
#' store all downloaded filings. All the filings will be stored in the 
#' current working directory. Keep the same current working directory for 
#' further process. 
#' 
#' @usage getFilings(cik.no, form.type, filing.year, quarter, downl.permit)
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
#'    
#' @return Function downloads EDGAR filings and returns download status in dataframe 
#' format with CIK, company name, form type, date filed, accession number, and 
#' download status.
#'   
#' @examples
#' \dontrun{
#' 
#' output <- getFilings(cik.no = c(1000180, 38079), c('10-K','10-Q'), 
#'                      2006, quarter = c(1, 2, 3), downl.permit = "n")
#'                      
#' ## download '10-Q' and '10-K' filings filed by the firm with 
#' CIK = 1000180 in quarters 1,2, and 3 of the year 2006. These 
#' filings will be stored in the current working directory.
#' 
#' }

getFilings <- function(cik.no = "ALL", form.type = "ALL", filing.year, quarter = c(1, 2, 3, 4),
                       downl.permit = "n") {
  
  options(warn = -1)  # remove warnings
  
  # Check the year array validity
  if (!is.numeric(filing.year)) {
    cat("Error: Input year(s) is not numeric.")
    return()
  }
  
  filing.year <- filing.year[filing.year >= 1994]
  
  if ( length(filing.year) == 0 ) {
    cat("Please provide filing years after 1993.")
    return()
  }
  
  
  # Check the download compatibility based on OS
  getdownCompat <- function() {
    
    if (nzchar(Sys.which("libcurl"))) {
      dmethod <- "libcurl"
    } else if (nzchar(Sys.which("wget"))) {
      dmethod <- "wget"
    } else if (nzchar(Sys.which("curl"))) {
      dmethod <- "curl"
    } else if (nzchar(Sys.which("lynx"))) {
      dmethod <- "lynx"
    } else if (nzchar(Sys.which("wininet"))) {
      dmethod <- "wininet"
    } else {
      dmethod <- "auto"
    }
    
    return(dmethod)
  }
  
  
  
  # function to download file and return FALSE if download error
  DownloadSECFile <- function(link, dfile, dmethod) {
    
    tryCatch({
      utils::download.file(link, dfile, method = dmethod, quiet = TRUE)
      return(TRUE)
    }, error = function(e) {
      return(FALSE)
    })
    
  }
  
  dmethod <- getdownCompat() ## Check the download compatibility based on OS
  
  # Create empty master index file and then updated it yearwise
  index.df <- data.frame()
  
  # Iterate thorugh each years
  for( year in filing.year ){
    
    yr.master <- paste0(year, "master.Rda")  ## Create specific year .Rda filename.
    
    filepath <- paste0("Master Indexes/", yr.master)
    
    if (!file.exists(filepath)) {
      getMasterIndex(year)  # download master index
    }
    
    load(filepath)  # Import master Index
    
    if(form.type == "ALL"){
      form.type <- unique(year.master$form.type)
    }
    
    if( cik.no == "ALL" ){
      year.master <- year.master[which(year.master$form.type %in% form.type 
                                       & year.master$quarter %in% quarter), ]
    } else {
      year.master <- year.master[which(year.master$cik %in% cik.no & year.master$form.type %in% form.type 
                                       & year.master$quarter %in% quarter), ]
    }
    
    if(nrow(year.master)>0){

      year.master$filing.year <- year
      
      # Update main master index file
      index.df <- rbind(index.df, year.master)
    }

  }

  if (nrow(index.df) == 0) {
    cat("No filing information found for given CIK(s) and Form Type in the mentioned year(s)/quarter(s).\n")
    return()
  }
  
  index.df <- index.df[order(index.df$cik, index.df$filing.year), ]
  
  # Downloading files
  total.files <- nrow(index.df)
  
  msg3 <- paste0("Total number of filings to be downloaded = ", total.files, 
                 ". Do you want to download (y/n)? ")
  
  if (as.character(downl.permit) == "n") {
    downl.permit <- readline(prompt = msg3)
  }
  
  if (as.character(downl.permit) == "y") {
    
    dir.create("Edgar filings_full text")
    
    cat("Downloading fillings. Please wait...", "\n")
    
    # Create progress bar object
    progress.bar <- txtProgressBar(min = 0, max = total.files, style = 3)

    
    # Convert edgar link column to character from levels
    index.df$edgar.link <- as.character(index.df$edgar.link)
    
    # get ACCESSION NUMBER as a fourth element of edgar link delimted by '/'
    accessions <- do.call(rbind.data.frame, strsplit(index.df$edgar.link, "\\/"))[4]
    index.df$accession.number <- gsub("\\.txt", "", accessions[, 1])
    
    row.names(index.df) <- c(1:nrow(index.df))
      
    index.df$status <- NA
    
    for (i in 1:total.files) {
      
      edgar.link <- paste0("https://www.sec.gov/Archives/", index.df$edgar.link[i])
      
      f.type <- gsub("/", "", index.df$form.type[i])
      
      year <- index.df$filing.year[i]
      cik <- index.df$cik[i]
        
      new.dir <- paste0("Edgar filings_full text/Form ", f.type)
      dir.create(new.dir)
      new.dir2 <- paste0(new.dir, "/", cik)
      dir.create(new.dir2)
      
      dest.filename <- paste0(new.dir2, "/", cik, "_", f.type, 
                              "_", index.df$date.filed[i], 
                              "_", index.df$accession.number[i], ".txt")
      
      if (file.exists(dest.filename)) {
        res <- TRUE
      } else {
        res <- DownloadSECFile(edgar.link, dest.filename, dmethod)
      }
      
      ## Try downloding one more time if Download error
      if (res == FALSE) {
        Sys.sleep(5) ## wait for 5 seconds and then try downloading again
        res <- DownloadSECFile(edgar.link, dest.filename, dmethod)
      }
      
      ## Fill up the status 
      if (res) {
        index.df$status[i] <- "Download success"
        
      } else {
        index.df$status[i] <- "Download Error"
      }
      
      # Update progress bar
      setTxtProgressBar(progress.bar, i)
      
    }
    
    index.df$edgar.link <- NULL

    # Close progress bar
    close(progress.bar)
    
    return(index.df)
  }
  
}
