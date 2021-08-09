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
#' declare user agent.  
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
#' 
#' output <- getFilings(cik.no = c(1000180, 38079), c('10-K','10-Q'), 
#'                      2006, quarter = c(1, 2, 3), downl.permit = "n", useragent)
#'                      
#' ## download '10-Q' and '10-K' filings filed by the firm with 
#' CIK = 1000180 in quarters 1,2, and 3 of the year 2006. These 
#' filings will be stored in the current working directory.
#' 
#' }

getFilings <- function(cik.no = "ALL", form.type = "ALL", filing.year, quarter = c(1, 2, 3, 4),
                       downl.permit = "n", useragent= "") {
  
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
  
  ### Check for valid user agent
  if(useragent != ""){
    # Check user agent
    bb <- any(grepl( "lonare.gunratan@gmail.com|glonare@uncc.edu|bharatspatil@gmail.com",
                     useragent, ignore.case = T))
    
    if(bb == TRUE){
      
      cat("Please provide a valid User Agent. 
      Visit https://www.sec.gov/os/accessing-edgar-data 
      for more information")
      return()
    }
    
  }else{
    
    cat("Please provide a valid User Agent. 
      Visit https://www.sec.gov/os/accessing-edgar-data 
      for more information")
    return()
  }
  
  # function to download file and return FALSE if download error
  DownloadSECFile <- function(link, dfile, dmethod, useragent) {
    
    tryCatch({
      utils::download.file(link, dfile, method = dmethod, quiet = TRUE,
                           headers = c("User-Agent" = useragent,
                                       "Accept-Encoding"= "deflate, gzip",
                                       "Host"= "www.sec.gov"))
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
      getMasterIndex(year, useragent)  # download master index
    }
    
    load(filepath)  # Import master Index
    
    if((length(form.type) == 1) && (form.type == "ALL")){
      form.type <- unique(year.master$form.type)
    }
    
    if( (length(cik.no) == 1) && (cik.no == "ALL" )){
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
        
        index.df$status[i] <- "Download success"
        
      } else {
        
        ### Go inside a loop to download
        k = 1
        
        while(TRUE){
          
          res <- DownloadSECFile(edgar.link, dest.filename, dmethod, useragent)
          
          if (res){
            
            if (file.info(dest.filename)$size < 6000){
              
              aa <- readLines(dest.filename)
              
              if(any(grepl("For security purposes, and to ensure that the public service remains available to users, this government computer system", aa)) == FALSE){
                
                index.df$status[i] <- "Download success"
                
                break
              }
              
            }else{
              
              index.df$status[i] <- "Download success"
              
              break
            }
          }
          
          ### If waiting for more than 10*15 seconds, put as server error
          if(k == 16){
            index.df$status[i] <- "Download Error"
            break
          }
          
          k = k + 1
          Sys.sleep(10) ## Wait for multiple of 10 seconds to ease request load on SEC server. 
        }
        
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
