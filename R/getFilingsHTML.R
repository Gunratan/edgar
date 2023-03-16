#' Get HTML view of EDGAR filings
#'
#' \code{getFilingsHTML} retrieves complete EDGAR filings and store them in
#' HTML format for view.
#'
#' getFilingsHTML function takes CIK(s), form type(s), filing year(s), and quarter of the 
#' filing as input. The function imports edgar filings downloaded 
#' via \link[edgar]{getFilings} function; otherwise, it downloads the filings which are 
#' not already been downloaded. It then reads the downloaded filings, scraps filing text
#' excluding exhibits, and saves the filing contents in 'Edgar filings_HTML view' 
#' directory in HTML format. The new directory 'Edgar filings_HTML view' will be 
#' automatically created by this function. According to SEC EDGAR's guidelines a 
#' user also needs to declare user agent. 
#' 
#' @usage getFilingsHTML(cik.no, form.type, filing.year, quarter, useragent)
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
#' @param useragent Should be in the form of "Your Name Contact@domain.com"
#'         
#' @return Function saves EDGAR filings in HTML format and returns filing information 
#' in dataframe format.
#'   
#' @examples
#' \dontrun{
#' 
#' output <- getFilingsHTML(cik.no = c(1000180, 38079), c('10-K','10-Q'), 
#'                          2006, quarter = c(1, 2, 3), useragent)
#' 
#' ## download '10-Q' and '10-K' filings filed by the firm with 
#' CIK = 1000180 in quarters 1,2, and 3 of the year 2006. These filings 
#' will be stored in the current working directory.
#' 
#' }

getFilingsHTML <- function(cik.no = "ALL", form.type = "ALL", filing.year, 
                           quarter = c(1, 2, 3, 4), useragent="") {
  
  # Check the year validity
  if (!is.numeric(filing.year)) {
    cat("Please check the input year.")
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
  
  ### Download filings
  output <- getFilings(cik.no = cik.no, form.type , filing.year, 
                       quarter = c(1, 2, 3, 4), downl.permit = "y", useragent)
  
  if (is.null(output)){
    #cat("No annual statements found for given CIK(s) and year(s).")
    return()
  }
  
  cat("Scrapping full EDGAR and converting to HTML...\n")
  
  progress.bar <- txtProgressBar(min = 0, max = nrow(output), style = 3)
  
  dir.create("Edgar filings_HTML view")
  
  for (i in 1:nrow(output)) {
    f.type <- gsub("/", "", output$form.type[i])
    year <- output$filing.year[i]
    cik <- output$cik[i]
    date.filed <- output$date.filed[i]
    accession.number <- output$accession.number[i]
    
    dest.filename <- paste0("Edgar filings_full text/Form ", f.type, 
                            "/", cik, "/", cik, "_", f.type, "_", 
                            date.filed, "_", accession.number, ".txt")
    # Read filing
    filing.text <- readLines(dest.filename)

    # Extract data from first <TEXT> to </TEXT>
    tryCatch({
      filing.text <- filing.text[(grep("<TEXT>", filing.text, ignore.case = TRUE)[1]):(grep("</TEXT>", 
                                                                                                filing.text, ignore.case = TRUE)[1])]
    }, error = function(e) {
      filing.text <- filing.text ## In case opening and closing TEXT TAG not found, cosnider full web page
    })

    #Define problematic text function
    remove_problematic_text <- function(t) {
    t <- gsub("\t"," ", t)
    t <- gsub("<CAPTION>|<S>|<C>", "", t, ignore.case = T)
    ## Append with PRE to keep the text format as it is
    t <- c("<PRE style='font-size: 15px'>", t, "</PRE>")
    }
    
    ifelse(!grepl(pattern ='<xml>|<type>xml|<html>', filing.text, ignore.case=T),
         remove_problematic_text(filing.text), filing.text)

    ## Form new dir and filename
    new.dir <- paste0("Edgar filings_HTML view/Form ", f.type)
    dir.create(new.dir)
    new.dir2 <- paste0(new.dir, "/", cik)
    dir.create(new.dir2)
    
    dest.filename2 <- paste0(new.dir2, "/", cik, "_", f.type, 
                            "_", output$date.filed[i], 
                            "_", output$accession.number[i], ".html")
    
    ## Writing filing text to html file
    writeLines(filing.text, dest.filename2)
    
    # update progress bar
    setTxtProgressBar(progress.bar, i)
  }
  
  ## convert dates into R dates
  output$date.filed <- as.Date(as.character(output$date.filed), "%Y-%m-%d")

  # Close progress bar
  close(progress.bar)
  
  output$quarter <- NULL
  output$filing.year <- NULL
  names(output)[names(output) == 'status'] <- 'downld.status'
  
  cat("HTML filings are stored in 'Edgar filings_HTML view' directory.")
  
  return(output)
}
