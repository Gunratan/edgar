#' Retrieves quarterly master index
#'
#' \code{getMasterIndex} retrieves the quarterly master indexes from the U.S. SEC EDGAR server.
#'
#' getMasterIndex function takes filing year as an input parameter from a user,  
#' downloads quarterly master indexes from the US SEC server.
#' \url{https://www.sec.gov/Archives/edgar/full-index/}. It then strips headers from the 
#' master index files, converts them into dataframe, and 
#' merges such quarterly dataframes into yearly dataframe, and stores them 
#' in Rda format. It has ability to download master indexes for multiple years 
#' based on the user input. This function creates a new directory 'Master Indexes' 
#' into current working directory to save these Rda Master Index. Please note, for 
#' all other functions in this package need to locate the same working 
#' directory to access these Rda master index files. According to SEC 
#' EDGAR's guidelines a user also needs to declare user agent. 
#'     
#' @usage getMasterIndex(filing.year, useragent)
#'
#' @param filing.year vector of integer containing filing years.
#' 
#' @param useragent Should be in the form of "Your Name Contact@domain.com"
#' 
#' @return Function downloads quarterly master index files and stores them 
#' into the mentioned directory.
#'   
#' @examples
#' \dontrun{
#' 
#' useragent <- "Your Name Contact@domain.com"
#' 
#' getMasterIndex(2006, useragent) 
#' ## Downloads quarterly master index files for 2006 and 
#' stores into yearly 2006master.Rda file.
#' 
#' getMasterIndex(c(2006, 2008), useragent) 
#' ## Downloads quarterly master index files for 2006 and 2008, and 
#' stores into 2006master.Rda and 2008master.Rda files.
#'}
#' @export
#' @importFrom R.utils gunzip
#' @importFrom utils download.file

getMasterIndex <- function(filing.year, useragent= "") {

    options(warn = -1)
    
    # Check year validity
    if (is.na(as.numeric(filing.year))) {
        cat("Please provide valid year.")
        return()
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
  DownloadSECFile <- function(link, dfile, useragent) {
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
  
    dir.create("Master Indexes")
    
    status.array <- data.frame()
    
    for (i in 1:length(filing.year)) {
        
        year <- filing.year[i]
        
        year.master <- data.frame()
        
        quarterloop <- 4
        
        # Find the number of quarters completed in input year
        if (year == format(Sys.Date(), "%Y")) {
            quarterloop <- ceiling(as.integer(format(Sys.Date(), "%m"))/3)
        }
        
        cat("Downloading Master Indexes from SEC server for",year,"...\n")

        for (quarter in 1:quarterloop) {
            
            # save downloaded file as specific name
            dfile <- paste0("Master Indexes/", year, "QTR", quarter, "master.gz")
            file <- paste0("Master Indexes/", year, "QTR", quarter, "master")
            
            # form a link to download master file
            link <- paste0("https://www.sec.gov/Archives/edgar/full-index/", year, "/QTR", quarter, "/master.gz")
            
            ### Go inside a loop to download
            i = 1
            
            while(TRUE){

              res <- DownloadSECFile(link, dfile, useragent)
            
              if (res){
                
                if (file.info(dfile)$size < 6000){
                  
                  aa <- readLines(dfile)
                  
                  if(any(grepl("For security purposes, and to ensure that the public service remains available to users, this government computer system", aa)) == FALSE){
                  
                      break
                  }
                  
                }else{
                  
                  break
                }
              }
              
              ### If waiting for more than 10*15 seconds, put as server error
              if(i == 16){
                
                status.array <- rbind(status.array, data.frame(Filename = paste0(year, ": quarter-", quarter),
                                                               status = "Server Error"))
                break
              }
              
              i = i + 1 
              Sys.sleep(10) ## Wait for multiple of 10 seconds to ease request load on SEC server. 
            }
            
            
            # Unzip gz file
            R.utils::gunzip(dfile, destname = file, temporary = FALSE, skip = FALSE, overwrite = TRUE, remove = FALSE)
            
            # Removing ''' so that scan with '|' not fail due to occurrence of ''' in company name
            # Convert to UTF-8 as different encodings may throw errors with gsub
            raw.data <- gsub("'", "", iconv(readLines(file), "latin1", "UTF-8"))

            # Find line number where header description ends
            header.end <- grep("--------------------------------------------------------", raw.data)
            
            # writing back to storage
            writeLines(raw.data, file)
            
            scraped.data <- scan(file, what = list("", "", "", "", ""), flush = F, skip = header.end, sep = "|",
                                 quiet = T)
            
            # Remove punctuation characters from company names
            company.name <- gsub("[[:punct:]]", " ", scraped.data[[2]], perl = T)
            
            final.data <- data.frame(cik = scraped.data[[1]], company.name = company.name, form.type = scraped.data[[3]],
                                     date.filed = scraped.data[[4]], edgar.link = scraped.data[[5]], quarter = quarter)
            
            year.master <- rbind(year.master, final.data)
            
            file.remove(file)
            
            status.array <- rbind(status.array, data.frame(Filename = paste0(year, ": quarter-", quarter),
                                                           status = "Download success"))
            
            cat("Master Index for quarter", quarter,"\n")
        }
        
  
        assign(paste0(year, "master"), year.master)
        
        save(year.master, file = paste0("Master Indexes/", year, "master.Rda"))
      
    }
    
}
