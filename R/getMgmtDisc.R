#' Retrieves management's discussion and analysis section
#'
#' \code{getMgmtDisc} retrieves "Item 7. Management's Discussion and Analysis of 
#' Financial Condition and Results of Operations" section of firms from annual statements 
#' based on CIK number and filing year.
#'
#' getMgmtDisc function takes firm CIK(s) and filing year(s) as input parameters from 
#' a user and provides "Item 7" section extracted from annual statements along with
#' filing information. The function imports annual filings downloaded 
#' via \link[edgar]{getFilings} function; otherwise, it downloads the filings which are 
#' not already been downloaded. It then reads, cleans, and parse the required section 
#' from the filings. It creates a new directory with the name "MD&A section text" 
#' in the current working directory to save scrapped "Item 7" sections in text format. 
#' It considers "10-K", "10-K405", "10KSB", and "10KSB40" form types as annual statements. 
#' According to SEC EDGAR's guidelines a user also needs to declare user agent. 
#' 
#'   
#' @usage getMgmtDisc(cik.no, filing.year, useragent)
#' 
#' @param cik.no vector of firm CIK numbers in integer format. Suppress leading 
#' zeroes from CIKs.
#' 
#' @param filing.year vector of four digit numeric year
#' 
#' @param useragent Should be in the form of "Your Name Contact@domain.com"
#' 
#' @return Function saves scrapped "Item 7" section from annual filings in 
#' "MD&A section text" directory present in the working directory. 
#' The output dataframe contains information on CIK number, company name, 
#' date of filing, and accession number. For a successful extraction of M&A section, 
#' 'extract.status' column returns 1, other return 0 for failed extraction. 
#'   
#' @examples
#' \dontrun{
#' 
#' output <- getMgmtDisc(cik.no = c(1000180, 38079), filing.year = 2005, useragent)
#' 
#' ## saves scrapped "Item 7" section from 10-K filings for CIKs in 
#' "MD&A section text" directory present in the working directory. 
#' Also, it provides filing information in the output datframe.
#' 
#' output <- getMgmtDisc(cik.no = c(1000180, 38079), 
#'                       filing.year = c(2005, 2006), useragent)
#'}

getMgmtDisc <- function(cik.no, filing.year, useragent="" ) {
    
  
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
  
  
    f.type <- c("10-K", "10-K405","10KSB", "10KSB40")
    # 10-K, 10-K405, 10-KSB, 10-KT, 10KSB, 10KSB40, and 10KT405 filings in the EDGAR database

    # Check the year validity
    if (!is.numeric(filing.year)) {
        cat("Please check the input year.")
        return()
    }
    
    output <- getFilings(cik.no = cik.no, form.type = f.type , filing.year, 
						 quarter = c(1, 2, 3, 4), downl.permit = "y", useragent)
    
    if (is.null(output)){
      cat("No annual statements found for given CIK(s) and year(s).")
      return()
    }
    
    cat("Extracting 'Item 7' section...\n")
    
    progress.bar <- txtProgressBar(min = 0, max = nrow(output), style = 3)
    
    # Function for text cleaning
    CleanFiling2 <- function(text) {
      
      text <- gsub("[[:digit:]]+", "", text)  ## remove Alphnumerics
      
      text <- gsub("\\s{1,}", " ", text)
      
      text <- gsub('\"',"", text)
      
      #text <- RemoveStopWordsFilings(text)
      
      return(text)
    }

    new.dir <- paste0("MD&A section text")
    dir.create(new.dir)
    
    output$extract.status <- 0
    
    output$company.name <- toupper(as.character(output$company.name))
    output$company.name <- gsub("\\s{2,}", " ",output$company.name)
    
    for (i in 1:nrow(output)) {
        f.type <- gsub("/", "", output$form.type[i])
        cname <- gsub("\\s{2,}", " ",output$company.name[i])
        year <- output$filing.year[i]
        cik <- output$cik[i]
        date.filed <- output$date.filed[i]
        accession.number <- output$accession.number[i]
        
        dest.filename <- paste0("Edgar filings_full text/Form ", f.type, 
                                "/", cik, "/", cik, "_", f.type, "_", 
                                date.filed, "_", accession.number, ".txt")
        # Read filing
        filing.text <- readLines(dest.filename)
        
        # Extract data from first <DOCUMENT> to </DOCUMENT>
        tryCatch({
          filing.text <- filing.text[(grep("<DOCUMENT>", filing.text, ignore.case = TRUE)[1]):(grep("</DOCUMENT>", 
                                                                                                    filing.text, ignore.case = TRUE)[1])]
        }, error = function(e) {
          filing.text <- filing.text ## In case opening and closing DOCUMENT TAG not found, cosnider full web page
        })
        
        # See if 10-K is in XLBR or old text format
        if (any(grepl(pattern = "<xml>|<type>xml|<html>|10k.htm", filing.text, ignore.case = T))) {
            
            doc <- XML::htmlParse(filing.text, asText = TRUE, useInternalNodes = TRUE, addFinalizer = FALSE)
            
            f.text <- XML::xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", 
                XML::xmlValue)
            
            f.text <- iconv(f.text, "latin1", "ASCII", sub = " ")
			
			      ## Free up htmlParse document to avoid memory leakage, this calls C function
            #.Call('RS_XML_forceFreeDoc', doc, package= 'XML')
            
        } else {
            f.text <- filing.text
        }
        
        # Preprocessing the filing text
        f.text <- gsub("\\n|\\t|$", " ", f.text)
        f.text <- gsub("^\\s{1,}", "", f.text)
        f.text <- gsub(" s ", " ", f.text)
        
        # Check for empty Lines and delete it
        empty.lnumbers <- grep("^\\s*$", f.text)
        
        if (length(empty.lnumbers) > 0) {
            f.text <- f.text[-empty.lnumbers]  ## Remove all lines only with space
        }
        
        
        # Get MD&A sections
        # M&D section is in Item 7 for 10-K, 10-K405 and in Item 6 for 10KSB and 10KSB40
        if (f.type == "10-K" | f.type == "10-K405"){
          
          startline <- grep("^Item\\s{0,}7\\.{0,}[^A.|\\(A\\)]", f.text, ignore.case = TRUE)
          endline <- grep("^Item\\s{0,}7\\.{0,}(A|\\(A\\)|\\.A)", f.text, ignore.case = TRUE)
          
          # if don't have Item 7A, then take up to Item 8
          # but also check if Item 7A match different number of times to Item 7.
          # if this is the case and Item 7A match is before the second or subsequent match for Item 7, extraction will be faulty use Item 8 as delimiter in this case
          if (length(endline) == 0 | (length(endline) != length(startline) & !all(endline - startline >= 0))) {
            endline <- grep("^Item\\s{0,}8", f.text, ignore.case = TRUE)
          }
          
          # if more than one match is identified, determine which one to use
          # use the match that that covers the most lines 
          # when a table of content in the source of the double match there will be few lines between start and end line
          # so take the match that covers more lines
          if (length(startline) >= 2 && length(endline)  >= 2) {
            startline <- startline[which.max(endline - startline)]
            endline <- endline[which.max(endline - startline)]
          }
        } else {
          startline <- grep("^Item\\s{0,}6", f.text, ignore.case = TRUE)
          endline <- grep("^Item\\s{0,}7", f.text, ignore.case = TRUE)
          
          # if more than one match is identified, determine which one to use
          # check if both startline and endline have two matches
          # if this is the case use the match where start line is before endline
          if (length(endline) != length(startline) & !all(endline - startline >= 0)) {
            startline <- startline[which.max(endline - startline)]
            endline <- endline[which.max(endline - startline)]
          }
          
          # use the match that that covers the most lines 
          # when a table of content in the source of the double match there will be few lines between start and end line
          # so take the match that covers more lines
          if (length(startline) >= 2 && length(endline) >= 2) {
            startline <- startline[which.max(endline - startline)]
            endline <- endline[which.max(endline - startline)]
          }
          
        }
        
        
        md.dicusssion <- NA
        words.count <- 0
        
        if (length(startline) != 0 && length(endline) != 0) {
            
            startline <- startline[length(startline)]
            endline <- endline[length(endline)] - 1
            
            md.dicusssion <- paste(f.text[startline:endline], collapse = " ")
            md.dicusssion <- gsub("\\s{2,}", " ", md.dicusssion)
            words.count <- stringr::str_count(md.dicusssion, pattern = "\\S+")
            
            #md.dicusssion <- gsub(" co\\.| inc\\.| ltd\\.| llc\\.| comp\\.", " ", md.dicusssion, ignore.case = T)
            
            #md.dicusssion2 <- unlist(strsplit(md.dicusssion, "\\. "))
            #md.dicusssion2 <- paste0(md.dicusssion2, ".")
            #md.dicusssion <- CleanFiling2(md.dicusssion)
            header <- paste0("CIK: ", cik, "\n", "Company Name: ", cname, "\n", 
                             "Form Type : ", f.type, "\n", "Filing Date: ", date.filed, "\n",
                             "Accession Number: ", accession.number)  
            md.dicusssion <- paste0(header, "\n\n\n", md.dicusssion)
            
            
        }
        
        if( (!is.na(md.dicusssion)) & (words.count>100)){
          filename2 <- paste0(new.dir, '/',cik, "_", f.type, "_", date.filed, 
                              "_", accession.number, ".txt")
          
          writeLines(md.dicusssion, filename2)
          output$extract.status[i] <- 1
        }
        
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
    
    cat("MD&A section texts are stored in 'MD&A section text' directory.")
    
    return(output)
}
