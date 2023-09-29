#' Retrieves Form 8-K event information
#'
#' \code{get8KItems} retrieves Form 8-K event information of firms based on CIK numbers
#' and filing year.
#'
#' get8KItems function takes firm CIK(s) and filing year(s) as input parameters from 
#' a user and provides information on the Form 8-K triggering events along with the firm 
#' filing information. The function searches and imports existing downloaded 
#' 8-K filings in the current directory; otherwise it downloads them using 
#' \link[edgar:getFilings]{getFilings} function. It then reads the 8-K filings and parses them 
#' to extract events information. According to SEC EDGAR's guidelines a user also needs to 
#' declare user agent. The function uses the \link[future.apply:future_lapply]{future_lapply} 
#' function to extract information in parallel. Please set \link[future:plan]{plan} 
#' according to your needs before running the function. The progress bar
#' can be controlled with the \link[progressr]{progressr} package. See in particular 
#' \link[progressr:handlers]{handlers} 
#' 
#' @usage get8KItems(cik.no, filing.year, useragent)
#' 
#' @param cik.no vector of CIK(s) in integer format. Suppress leading 
#' zeroes from CIKs.
#' 
#' @param filing.year vector of four digit numeric year
#' 
#' @param useragent Should be in the form of "Your Name Contact@domain.com"
#' 
#' @return Function returns dataframe with Form 8-K events information along with CIK
#'  number, company name, date of filing, and accession number.
#'   
#' @examples
#' \dontrun{
#' library(future.apply)
#' plan(multisession, workers = availableCores()-1)
#' 
#' # if a progress update is desired
#' library(progressr)
#' handlers(global = TRUE)
#' 
#' output <- get8KItems(cik.no = 38079, filing.year = 2005, useragent)
#' ## Returns 8-K event information for CIK '38079' filed in year 2005.
#' 
#' output <- get8KItems(cik.no = c(1000180,38079), 
#'                      filing.year = c(2005, 2006), useragent) 
#'}
#' @export
#' @importFrom progressr progressor handlers
#' @importFrom future.apply future_lapply

get8KItems <- function(cik.no, filing.year, useragent="") {
    
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
    
    
    ## Download related filings
    output <- getFilings(cik.no = cik.no, form.type = "8-K", filing.year, quarter = c(1, 2, 3, 4), 
	                     downl.permit = "y", useragent)
    
    if(is.null(output)){
      return()
    }
    
    cat("Scraping 8-K filings...\n")
    
    p <- progressr::progressor(along = 1:nrow(output))
    
    results <- future.apply::future_lapply(
      X = 1:nrow(output),
      FUN = function(i) {
        
        dest.filename <- paste0(getwd(),"/","Edgar filings_full text/Form ", "8-K", 
                              "/", output$cik[i], "/", output$cik[i], "_", "8-K", "_", 
                              output$date.filed[i], "_", output$accession.number[i], ".txt")
        
        filing.text <- readLines(dest.filename)

        # Capture ITEM INFORMATION
        event.info <- filing.text[grep("^ITEM INFORMATION:", filing.text, ignore.case = TRUE)]
        
        if (length(event.info) > 0) {
            event.info <- gsub("ITEM INFORMATION|:|\t", "", event.info)
        } else {
            event.info = ""
        }
        event.info <- gsub(",", " ", event.info)
        
        
        temp.df <- data.frame(cik = output$cik[i], company.name = output$company.name[i], 
                              form.type= output$form.type[i], date.filed = output$date.filed[i],
                              event.info = event.info)
        p()

        return(temp.df)
    })
    
    output.8K.df <- do.call(rbind, results)

    # # # convert dates into R dates
    output.8K.df$date.filed <- as.Date(as.character(output.8K.df$date.filed), "%Y-%m-%d")
    
    return(output.8K.df)
}
