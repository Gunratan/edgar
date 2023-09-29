#' Retrieves daily master index
#'
#' \code{getDailyMaster} retrieves daily master index from the U.S. SEC EDGAR server.
#'
#' getDailyMaster function takes date as an input parameter from a user,  
#' and downloads master index for the date from the U.S. SEC EDGAR server 
#' \url{https://www.sec.gov/Archives/edgar/daily-index/}. It strips headers 
#' and converts this daily filing information into dataframe format.
#' Function creates new directory 'Daily Indexes' into working directory 
#' to save these downloaded daily master index files in Rda format. According to SEC 
#' EDGAR's guidelines a user also needs to declare user agent. 
#' 
#' @usage getDailyMaster(input.date, useragent)
#' 
#' @param input.date in character format 'mm/dd/YYYY'.
#' 
#' @param useragent Should be in the form of "Your Name Contact@domain.com"
#' 
#' @return Function returns filings information in a dataframe format.
#'   
#' @examples
#' \dontrun{
#' 
#' output <- getDailyMaster('08/09/2016', useragent)
#'} 
#' @export
#' @import utils

getDailyMaster <- function(input.date, useragent="") {
    
    
    dir.create("Daily Indexes")
    
    input.date <- as.Date(input.date, "%m/%d/%Y")
    
    if (is.na(input.date)) {
        msg <- "Select the appropriate date."
        cat(msg)
        return()
    }
    
    if (Sys.Date() < input.date) {
        msg <- "Select the appropriate date."
        cat(msg)
        return()
    }
    
    year <- format(input.date, "%Y")
    month <- format(input.date, "%m")
    day <- format(input.date, "%d")
    
    options(warn = -1)  # remove warnings
    
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
    
    
    DownloadSECFile2 <- function(link, dfile, dmethod, useragent) {

      ### Go inside a loop to download
      i = 1
      
      while(TRUE){
        
        res <- DownloadSECFile(link, dfile, dmethod, useragent)
        
        if (res){
          
          if (file.info(dfile)$size < 6000){
            
            aa <- readLines(dfile)
            
            if(any(grepl("For security purposes, and to ensure that the service remains available to users, this government computer system", aa)) == FALSE){
              
              break
            }
            
          }else{
            
            break
          }
        }
        
        
        i = i + 1 
        Sys.sleep(10) ## Wait for multiple of 10 seconds to ease request load on SEC server. 
      }
      return(TRUE)
    }
    
    # function for downloading daily Index
    GetDailyInfo <- function(day, month, year) {
        
        dmethod <- getdownCompat() ## Check the download compatibility based on OS
      
        date <- paste0(year, month, day)
        
        filename <- paste0("Daily Indexes/daily_idx_", date)
        
        link1 <- paste0("https://www.sec.gov/Archives/edgar/daily-index/master.", date, ".idx")
        
        link2 <- paste0("https://www.sec.gov/Archives/edgar/daily-index/", year, "/QTR", ceiling(as.integer(month)/3), 
            "/master.", date, ".idx")
        
        link3 <- paste0("https://www.sec.gov/Archives/edgar/daily-index/", year, "/QTR", ceiling(as.integer(month)/3), 
            "/master.", substr(as.character(year), 3, 4), month, day, ".idx")
        
        link4 <- paste0("https://www.sec.gov/Archives/edgar/daily-index/", year, "/QTR", ceiling(as.integer(month)/3), 
            "/master.", date, ".idx")
        
        down.success = FALSE
        
        if (year < 1999) {
          down.success <- DownloadSECFile2(link3, filename, dmethod, useragent)
        }
        
        if (year > 1998 && year < 2012) {
          down.success <- DownloadSECFile2(link4, filename, dmethod, useragent)
        }
        
        if (year > 2011) {
            fun.return1 <- DownloadSECFile2(link1, filename, dmethod, useragent)
            if (fun.return1 && file.size(filename) > 500) {
                down.success = TRUE
                
            } else {
                fun.return2 <- DownloadSECFile2(link2, filename, dmethod, useragent)
                if (fun.return2) {
                  down.success = TRUE
                }
            }
        }
        
        if (down.success) {
            
            # Removing ''' so that scan with '|' not fail due to occurrence of ''' in company name
            temp.data <- gsub("'", "", readLines(filename))
            # writting back to storage
            writeLines(temp.data, filename)
            
            # Find line number where header description ends
            header.end <- grep("--------------------------------------------------------", temp.data)
            
            scrapped.data <- scan(filename, what = list("", "", "", "", ""), flush = F, skip = header.end, sep = "|", 
                quiet = T)
            
            final.data <- data.frame(cik = scrapped.data[[1]], company.name = scrapped.data[[2]], form.type = scrapped.data[[3]], 
                date.filed = scrapped.data[[4]], edgar.link = scrapped.data[[5]])
            
            final.data$edgar.link <- NULL
            
            ## Save daily master index in Rda format
            file.remove(filename)
            save(final.data, file = paste0(filename, ".Rda"))
  
            return(final.data)
        }else{
           cat(" Daily master index is not availbale for this date.")
          return()
        }
    }
    
    ## Call above GetDailyInfo function
    return(GetDailyInfo(day, month, year))
}
